airqualityUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "EPA Air Quality",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("EPA's ambient air quality data",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 10px"),
            p(
              a(href = "https://www.epa.gov/criteria-air-pollutants/naaqs-table",
                "National Ambient Air Quality Standards (NAAQS)", target = "_blank",
                style = "color: #663399;"),
              style = "bottom-margin: 15px"
            ),
            selectizeInput(
              inputId = ns("pollutant"),
              label = NULL,
              choice = .pollutant_list,
              multiple = FALSE,
              selected = "CO 1-hour 1971"
            ),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              "EPA's AQS API annualData service"),
            p("Field used:",
              style = "font-weight: bold; color: orange; margin-bottom: 3px"),
            selectizeInput(
              inputId = ns("data_field"),
              label = NULL,
              choice = "arithmetic_mean",
              multiple = FALSE,
              selected = "arithmetic_mean"
            ),
            helpText(
              icon("circle-info"),
              "Please check",
              a(href = "https://aqs.epa.gov/aqsweb/documents/AQS_Data_Dictionary.html",
                "AQS Data Dictionary", target = "_blank"), "for field descriptions.",
              style = "color: #3B71CA;"
            ),
            hr(),
            radioButtons(
              inputId = ns("data_type"),
              label = "Geographic unit",
              ## disable temporarily Grid
              ## choices = c("County", "Census Tract", "Zip Code", "Grid"),
              choices = c("County", "Census Tract"),
              selected = "Census Tract"
            ),
            hr(),
            selectizeInput(
              inputId = ns("event"),
              label = "Exceptional event",
              choice = c("Events Included", "Events Excluded"),
              multiple = FALSE,
              selected = "Events Included"
            ),
            hr(),
            selectizeInput(
              inputId = ns("year"),
              label = "Year",
              choice = sort(unique(airquality$county$YEAR), decreasing = TRUE),
              multiple = TRUE,
              selected = sort(unique(airquality$county$YEAR), decreasing = TRUE)[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("airquality_smap"), height = "67vh"))
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("airquality_mmap")))
          ## uiOutput(ns("airquality_mmap"))
        )
      )
    )
  )
}

airqualityServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL, unit = NULL)
      observeEvent({
        req(input$pollutant)
        req(input$data_type)
        req(input$data_field)
        req(input$event)
        req(input$year)
      }, {
        field_list <- .get_data_field(input$pollutant)
        if (input$data_field %in% field_list) {
          updateSelectizeInput(session, inputId = "data_field", choices = field_list,
                               selected = input$data_field)
          data_field_selected <- input$data_field
        } else {
          updateSelectizeInput(session, inputId = "data_field", choices = field_list,
                               selected = "arithmetic_mean")
          data_field_selected <- "arithmetic_mean"
        }
        rv$unit <- .get_pollutant_unit(sub("^(.*?) (.*)", "\\1", input$pollutant))
        if (data_field_selected == "primary_exceedance_count") {
          rv$unit <- "(count)"
        }
        rv$data <- .subset_airquality(
          input$data_type, input$pollutant, data_field_selected,
          input$event, input$year
        )
      })
      observeEvent({
        rv$data
      }, {
        if (input$data_type == "Grid") {
          p <- .draw_airquality_grid(rv$data, rv$unit, input$year)
        } else {
          p <- .draw_airquality_sf(rv$data, rv$unit, input$year)
        }
        if (length(input$year) == 1) {
          output$airquality_smap <- renderLeaflet(p)
        } else {
          output$airquality_mmap <- renderUI(p)
        }
      })
    }
  )
}

.pollutant_list <- list(
  `Carbon monoxide (42101)` = list("CO 1-hour 1971", "CO 8-hour 1971"),
  `Sulfur dioxide (42401)` = list("SO2 1-hour 2010"),
  `Nitrogen dioxide (42602)` = list("NO2 1-hour 2010", "NO2 Annual 1971"),
  `Ozone (44201)` = list("Ozone 8-hour 2015"),
  `PM2.5 - Local conditions (88101)` = list("PM2.5 24-hour 2012", "PM2.5 Annual 2012"),
  `PM10 - Total 0-10um STP (81102)` = list("PM10 24-hour 2006")
)

.get_data_field <- function(x) {
  dat_field <- c("arithmetic_mean")
  to_append <- switch(
    x,
    "CO 1-hour 1971" = "second_max_value",
    "CO 8-hour 1971" = "second_max_nonoverlap_value",
    "SO2 1-hour 2010" = "ninety_ninth_percentile",
    "NO2 1-hour 2010" = "ninety_eighth_percentile",
    "NO2 Annual 1971" = "arithmetic_mean",
    "Ozone 8-hour 2015" = "fourth_max_value",
    "PM10 24-hour 2006" = "primary_exceedance_count",
    "PM2.5 24-hour 2012" = "ninety_eighth_percentile",
    "PM2.5 Annual 2012" = "arithmetic_mean"
  )
  unique(append(dat_field, to_append))
}

.translate_standard <- function(x) {
  x <- gsub(" ", "_", x)
  x <- gsub("-", "_", x)
  gsub("Annual", "annual", x)
}

.get_pollutant_unit <- function(x) {
  switch(
    x,
    "CO" = "(ppm)",
    "SO2" = "(ppb)",
    "NO2" = "(ppb)",
    "Ozone" = "(ppm)",
    "PM2.5" = "(μg/m<sup>3</sup>)",
    "PM10" = "(μg/m<sup>3</sup>)"
  )
}

.subset_airquality_grid <- function(x, pollutant, data_field, year, event) {
  idx_pollutant <- match(pollutant, names(x))
  x <- x[[idx_pollutant]]
  x <- x[, , , , data_field, event, drop = TRUE]
  x[, , , as.character(year), drop = TRUE]
}

.subset_airquality_sf <- function(x, pollutant, data_field, year, event) {
  x <- x[x$POLLUTANT_STANDARD == pollutant, ]
  x <- x[x$DATA_FIELD == data_field, ]
  x <- x[x$EVENT == event, ]
  x[x$YEAR %in% as.integer(year), ]
}

.subset_airquality <- function(type = c("Grid", "County",
                                        "Census Tract", "Zip Code"),
                               pollutant, data_field,
                               event = c("Events Included", "Events Excluded"),
                               year) {
  type <- match.arg(type)
  type <- switch(
    type,
    "Grid" = "grid", "County" = "county",
    "Census Tract" = "census_tract", "Zip Code" = "zip_code"
  )
  x <- airquality[[type]]
  pollutant <- .translate_standard(pollutant)
  event <- match.arg(event)
  if (type == "grid") {
    .subset_airquality_grid(x, pollutant, data_field, year, event)
  } else {
    .subset_airquality_sf(x, pollutant, data_field, year, event)
  }
}

.draw_airquality_grid <- function(x, unit, year) {
  min_val <- min(x[[1]], na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x[[1]], na.rm = TRUE) * 1.01
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_airquality_leaflet(
        x[, , , k, drop = TRUE], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), project = FALSE, grid = TRUE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_airquality_leaflet(x, min_val, max_val, title = unit,
                             project = FALSE, grid = TRUE)
  }
}

.draw_airquality_sf <- function(x, unit, year) {
  min_val <- min(x$VALUE, na.rm = TRUE)
  max_val <- max(x$VALUE, na.rm = TRUE)
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_airquality_leaflet(
        x[x$YEAR == k, ], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_airquality_leaflet(x, min_val, max_val, title = unit, grid = FALSE)
  }
}

.draw_airquality_leaflet <- function(x, min_val, max_val, title = NULL,
                                     zoom = 11, num_fmt = "%.3f",
                                     project = FALSE, grid = TRUE,
                                     col_reverse = TRUE, palette = "Spectral") {
  p <- leaflet() |>
    addTiles() |>
    setView(lng = -75.1652, lat = 39.9525, zoom = zoom) |>
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Recenter",
      onClick = JS("function(btn, map){map.setView([39.9525, -75.1652], 11);}")
    )) |>
    addLegend(
      position = "bottomright",
      pal = .get_pal(min_val, max_val, reverse = !col_reverse, palette = palette),
      values = c(min_val, max_val),
      labFormat = labelFormat(transform = function(k) sort(k, decreasing = TRUE)),
      title = title
    )
  if (grid) {
    p |>
      addRasterImage(
        as(x, "Raster"), colors = .get_pal(min_val, max_val, reverse = col_reverse,
                                           palette = palette),
        opacity = 0.6, project = project
      )
  } else {
    if ("NAME20" %in% names(x)) {
      ## zip code data
      polygon_label <- paste0("Zip code ", x$NAME20, ": ", sprintf(num_fmt, x$VALUE))
    } else if ("NAMELSADCO" %in% names(x)) {
      polygon_label <- paste0(
        x$NAMELSAD, ", ", x$NAMELSADCO, ", ", x$STUSPS,
        ": ", sprintf(num_fmt, x$VALUE)
      )
    } else {
      ## polygon_label <- paste0(
      ##   x$NAMELSAD, ", ", x$STUSPS, ": ", sprintf(num_fmt, x$VALUE)
      ## )
      polygon_label <- paste0(x$NAME, ": ", sprintf(num_fmt, x$VALUE))
    }
    p |>
      addPolygons(
        data = st_transform(x, 4326),
        fillColor = ~.get_pal(min_val, max_val, reverse = col_reverse,
                              palette = palette)(VALUE),
        weight = 1, opacity = 1,
        color = "#444444",
        dashArray = NULL, fillOpacity = 0.6,
        highlightOptions = highlightOptions(
          weight = 3, color = "#444444", dashArray = NULL,
          fillOpacity = 0.9, bringToFront = FALSE
        ),
        label = polygon_label
      )
  }
}
