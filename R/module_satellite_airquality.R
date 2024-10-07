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
              choice = .satellite_pollutant_list,
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
              choices = c("County", "Census Tract"),
              selected = "Census Tract"
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
          withSpinner(leafletOutput(ns("satellite_smap"), height = "67vh"))
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
      rv <- reactiveValues(data = NULL, unit = NULL, year_selected = NULL)
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
        rv$data <- .subset_satellite(input$data_type, input$pollutant, rv$year_selected)
      })
      observeEvent({
        rv$data
      }, {
        if (input$data_type == "Grid") {
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
    }
  )
}

.get_year_list <- function(x = c("Cooper 2022", "Annenberg 2022", "Shen 2024",
                                 "van Donkelaar 2021")) {
  x <- match.arg(x)
  year_list <- switch(
    x,
    `Cooper 2022` = unique(satellite$no2_cooper[[2]]$year),
    `Annenberg 2022` = unique(satellite$no2_annenberg[[2]]$year),
    `Shen 2024` = unique(satellite$pm25_shen[[2]]$year),
    `van Donkelaar 2021` = unique(satellite$pm25_van[[2]]$year)
    )
  sort(year_list, decreasing = TRUE)
}

.satellite_pollutant_list <- list(
  `NO2` = list("Cooper 2022", "Annenberg 2022"),
  `PM2.5` = list("Shen 2024", "van Donkelaar 2021")
)

.subset_satellite <- function(type, pollutant, year) {
  type <- switch(
    type,
    "Grid" = "grid",
    "County" = "county",
    "Census Tract" = "census_tract",
    "Block Group" = "block_group",
    "Zip Code" = "zip_code"
  )
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
    x[x$year %in% as.integer(year), ]
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
  value_idx <- match("value", names(x))
  names(x)[value_idx] <- "VALUE"
  min_val <- min(x$VALUE, na.rm = TRUE)
  max_val <- max(x$VALUE, na.rm = TRUE)
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_satellite_leaflet(
        x[x$year == k, ], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_satellite_leaflet(x, min_val, max_val, title = unit, grid = FALSE)
  }
}

.draw_satellite_leaflet <- function(...) .draw_airquality_leaflet(...)
