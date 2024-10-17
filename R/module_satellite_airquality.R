satelliteUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Satellite Air Quality",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("Satellite-derived air quality data",
               style = "font-weight: bold; color: #DC4C64; margin-top: 10px; margin-bottom: 15px"),
            selectizeInput(
              inputId = ns("pollutant"),
              label = NULL,
              choice = .satellite_var_list,
              multiple = FALSE,
              selected = "Cooper 2022"
            ),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(HTML(paste0("NO", tags$sub("2"))),
              style = "font-weight: bold; color: orange; margin-bottom: 3px"),
            a(href = "https://doi.org/10.1038/s41586-021-04229-0",
              "Cooper et al. Nature 2022", target = "_blank"),
            br(),
            a(href = "https://doi.org/10.1016/s2542-5196(21)00255-2",
              "Annenberg et al. Lancet Planet Health 2022", target = "_blank"),
            p("PM2.5", style = "font-weight: bold; color: orange; margin-top: 10px; margin-bottom: 3px"),
            a(href = "https://pubs.acs.org/doi/10.1021/acsestair.3c00054",
              "Shen et al. ACS EST Air 2024", target = "_blank"),
            br(),
            a(href = "https://pubs.acs.org/doi/full/10.1021/acs.est.1c05309",
              "van Donkelaar et al. Environ Sci Technol 2021", target = "_blank"),
            hr(),
            radioButtons(
              inputId = ns("data_type"),
              label = "Geographic unit",
              ## disable temporarily Grid
              ## choices = c("County", "Census Tract", "Zip Code", "Grid"),
              choices = .satellite_type_list,
              selected = "census_tract"
            ),
            hr(),
            selectizeInput(
              inputId = ns("year"),
              label = "Year",
              choice = .get_year_list("Cooper 2022"),
              multiple = TRUE,
              selected = .get_year_list("Cooper 2022")[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("satellite_smap"), height = "67vh")),
          conditionalPanel(
            "input.data_type != 'grid'",
            ns = ns,
            hr(),
            p(strong("Click a polygon of interest to view historical change."),
              style = "color: #3CB371; margin-bottom: 20px"),
            plotlyOutput(ns("trend"))
          )
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("satellite_mmap")))
        )
      )
    )
  )
}

satelliteServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL, unit = NULL, year_selected = NULL,
                           trend = NULL, trend_subtitle = NULL)
      observeEvent({
        req(input$pollutant)
        req(input$data_type)
        input$year
      }, {
        year_list <- .get_year_list(input$pollutant)
        if (is.null(input$year) || any(input$year %ni% year_list)) {
          updateSelectizeInput(session, inputId = "year", choices = year_list,
                               selected = year_list[1])
          ## Don't rv$year_selected <- input$year as input$year has not been
          ## updated until observeEvent{} ends
          rv$year_selected <- year_list[1]
        } else {
          updateSelectizeInput(session, inputId = "year", choices = year_list,
                               selected = input$year)
          rv$year_selected <- input$year
        }
        rv$unit <- switch(
          input$pollutant,
          "Cooper 2022" = "(ppbv)",
          "Annenberg 2022" = "(ppb)",
          "Shen 2024" = "(μg/m<sup>3</sup>)",
          "van Donkelaar 2021" = "(μg/m<sup>3</sup>)"
        )
        rv$data <- .subset_satellite(input$data_type, input$pollutant,
                                     rv$year_selected)
        rv$trend <- .subset_satellite(input$data_type, input$pollutant,
                                      year_list)
        if (input$pollutant %in% c("Cooper 2022", "Annenberg 2022")) {
          pollutant_var <- "NO2"
        } else if (input$pollutant %in% c("Shen 2024", "van Donkelaar 2021")) {
          pollutant_var <- "PM2.5"
        } else {
          pollutant_var <- NULL
        }
        rv$trend_subtitle <- paste0("<sub>", pollutant_var, " (",
                                    input$pollutant, ")</sub>")
      })
      observeEvent({
        rv$data
      }, {
        if (input$data_type == "grid") {
          p <- .draw_satellite_grid(rv$data, rv$unit, rv$year_selected)
        } else {
          p <- .draw_satellite_sf(rv$data, rv$unit, rv$year_selected)
        }
        if (length(input$year) == 1) {
          output$satellite_smap <- renderLeaflet(p)
        } else {
          output$satellite_mmap <- renderUI(p)
        }
      })
      observeEvent({
        req(input$satellite_smap_shape_click)
      }, {
        click_info <- input$satellite_smap_shape_click
        x <- rv$trend[rv$trend$LOCATION %in% click_info$id, ] |>
          st_drop_geometry()
        ylabel <- gsub("\\(|\\)", "", rv$unit)
        title <- paste0(click_info$id, "<br>", rv$trend_subtitle)
        output$trend <- renderPlotly(.trend_plot(x, title = title, ylab = ylabel))
      })
    }
  )
}

.get_year_list <- function(x = c("Cooper 2022", "Annenberg 2022", "Shen 2024",
                                 "van Donkelaar 2021")) {
  x <- match.arg(x)
  year_list <- switch(
    x,
    `Cooper 2022` = unique(satellite$no2_cooper[[2]]$YEAR),
    `Annenberg 2022` = unique(satellite$no2_annenberg[[2]]$YEAR),
    `Shen 2024` = unique(satellite$pm25_shen[[2]]$YEAR),
    `van Donkelaar 2021` = unique(satellite$pm25_van[[2]]$YEAR)
    )
  sort(year_list, decreasing = TRUE)
}

.satellite_var_list <- list(
  `NO2` = list("Cooper 2022", "Annenberg 2022"),
  `PM2.5` = list("Shen 2024", "van Donkelaar 2021")
)

.satellite_type_list <- list(
  `County` = "county",
  `Census Tract` =  "census_tract"
)

.subset_satellite <- function(type = c("census_tract", "county"), pollutant, year) {
  type <- match.arg(type)
  x <- switch(
    pollutant,
    `Cooper 2022` = satellite$no2_cooper[[type]],
    `Annenberg 2022` = satellite$no2_annenberg[[type]],
    `Shen 2024` = satellite$pm25_shen[[type]],
    `van Donkelaar 2021` = satellite$pm25_van[[type]],
  )
  if (type == "grid") {
    ## year <- as.character(year)
    ## x[, , , year, drop = TRUE]
    x[, , , as.character(year), drop = TRUE]
  } else {
    x[x$YEAR %in% as.integer(year), ]
  }
}

.draw_satellite_grid <- function(x, unit, year) {
  min_val <- min(x[[1]], na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x[[1]], na.rm = TRUE) * 1.01
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_satellite_leaflet(
        x[, , , as.character(k), drop = TRUE], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), project = FALSE, grid = TRUE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_satellite_leaflet(x, min_val, max_val, title = unit,
                            project = FALSE, grid = TRUE)
  }
}

.draw_satellite_sf <- function(x, unit, year) {
  min_val <- min(x$VALUE, na.rm = TRUE)
  max_val <- max(x$VALUE, na.rm = TRUE)
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_satellite_leaflet(
        x[x$YEAR == k, ], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_satellite_leaflet(x, min_val, max_val, title = unit, grid = FALSE)
  }
}

.draw_satellite_leaflet <- function(...) .draw_airquality_leaflet(...)
