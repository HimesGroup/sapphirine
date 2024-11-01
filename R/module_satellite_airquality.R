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
              inputId = ns("fig_type"),
              label = "Figure type",
              choices = list(`Map` = "map", `Line Graph` = "line_graph"),
              selected = "map",
              inline = TRUE
            ),
            radioButtons(
              inputId = ns("data_type"),
              label = "Geographic unit",
              ## disable temporarily Grid
              ## choices = c("County", "Census Tract", "Zip Code", "Grid"),
              choices = .satellite_type_list,
              selected = "census_tract"
            ),
            hr(),
            conditionalPanel(
              "input.fig_type == 'map'",
              ns = ns,
              selectizeInput(
                inputId = ns("year"),
                label = "Year",
                choice = .get_year_list("no2_cooper"),
                multiple = TRUE,
                selected = .get_year_list("no2_cooper")[1]
              )
            ),
            conditionalPanel(
              "input.fig_type == 'line_graph'",
              ns = ns,
              selectizeInput(
                inputId = ns("location"),
                label = "Regions of interest (up to 8)",
                choices = NULL,
                multiple = TRUE,
                options = list(maxItems = 8)
              )
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.fig_type == 'map'",
          ns = ns,
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
        ),
        conditionalPanel(
          "input.fig_type == 'line_graph'",
          ns = ns,
          plotlyOutput(ns("line"))
        )
      )
    )
  )
}

satelliteServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL, unit = NULL,
                           year_selected = NULL, trend_subtitle = NULL)
      observeEvent({
        req(input$pollutant)
      }, {
        updateSelectizeInput(
          session, inputId = "year", choices = .get_year_list(input$pollutant),
          selected = NULL
        )
        rv$unit <- switch(
          input$pollutant,
          "no2_cooper" = "(ppbv)",
          "no2_annenberg" = "(ppb)",
          "pm25_shen" = "(μg/m<sup>3</sup>)",
          "pm25_van" = "(μg/m<sup>3</sup>)"
        )
        if (grepl("^no2", input$pollutant)) {
          name_idx <- which(.satellite_var_list[["NO2"]] == input$pollutant)
          rv$trend_subtitle <- paste0(
            "<sub>NO2 (", names(.satellite_var_list[["NO2"]])[name_idx], ")</sub>"
          )
        }
        if (grepl("^pm25", input$pollutant)) {
          name_idx <- which(.satellite_var_list[["PM2.5"]] == input$pollutant)
          rv$trend_subtitle <- paste0(
            "<sub>PM2.5 (", names(.satellite_var_list[["PM2.5"]])[name_idx], ")</sub>"
          )
        }
      }, ignoreInit = TRUE)
      observeEvent({
        req(input$data_type)
        input$year
      }, {
        output$satellite_smap <- renderLeaflet(NULL)
        output$satellite_mmap <- renderLeaflet(NULL)
        if (!is.null(input$year)) {
          rv$data <- .subset_satellite(input$data_type, input$pollutant, NULL)
          x <- rv$data[rv$data$YEAR %in% input$year, ]
          if (input$data_type == "grid") {
            p <- .draw_satellite_grid(x, rv$unit, input$year)
          } else {
            p <- .draw_satellite_sf(x, rv$unit, input$year)
          }
          if (length(input$year) == 1) {
            output$satellite_smap <- renderLeaflet(p)
          } else {
            output$satellite_mmap <- renderUI(p)
          }
          output$trend <- renderPlotly(NULL)
        }
      }, ignoreNULL = FALSE)
      observeEvent({
        req(input$satellite_smap_shape_click)
      }, {
        click_info <- input$satellite_smap_shape_click
        x <- rv$data[rv$data$LOCATION %in% click_info$id, ] |>
          st_drop_geometry()
        ylabel <- gsub("\\(|\\)", "", rv$unit)
        title <- paste0(click_info$id, "<br>", rv$trend_subtitle)
        output$trend <- renderPlotly(.trend_plot(x, title = title, ylab = ylabel))
      })
      observeEvent({
        req(input$data_type)
      }, {
        ## Don't know why but if previously selected items in different
        ## geographic unit memorized in the updated list; so clear list
        ## first.
        updateSelectizeInput(
          session, inputId = "location", choices = character(0)
        )
        updateSelectizeInput(
          session, inputId = "location",
          choices = unique(satellite[[input$pollutant]][[input$data_type]]$LOCATION),
          selected = NULL, server = TRUE
        )
        output$line <- renderPlotly(NULL)
        output$trend <- renderPlotly(NULL)
      })
      observeEvent({
        req(input$pollutant)
        input$location
      }, {
        if (!is.null(input$location)) {
          x <- .subset_satellite(input$data_type, input$pollutant, NULL)
          x <- x[x$LOCATION %in% input$location, ] |>
            st_drop_geometry()
          ylabel <- gsub("\\(|\\)", "", rv$unit)
          p <- .line_plot(x, fmt_y = "%{y:.3f}", ylab = ylabel)
          output$line <- renderPlotly(p)
        } else {
          output$line <- renderPlotly(NULL)
        }
      }, ignoreNULL = FALSE)
    }
  )
}

.get_year_list <- function(x) {
  sort(unique(satellite[[x]][[1]]$YEAR), decreasing = TRUE)
}

.satellite_var_list <- list(
  `NO2` = list(`Cooper 2022` = "no2_cooper", `Annenberg 2022` = "no2_annenberg"),
  `PM2.5` = list(`Shen 2024` = "pm25_shen", `van Donkelaar 2021` = "pm25_van")
)

.satellite_type_list <- list(
  `County` = "county",
  `Census Tract` =  "census_tract"
)

.subset_satellite <- function(type = c("census_tract", "county"), pollutant, year) {
  type <- match.arg(type)
  x <- satellite[[pollutant]][[type]]
  if (!is.null(year)) {
    if (type == "grid") {
      ## year <- as.character(year)
      ## x[, , , year, drop = TRUE]
      x[, , , as.character(year), drop = TRUE]
    } else {
      x[x$YEAR %in% as.integer(year), ]
    }
  } else {
    x
  }
 }

.draw_satellite_grid <- function(x, unit, year) {
  min_val <- min(x[[1]], na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x[[1]], na.rm = TRUE) * 1.01
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_leaflet(
        x[, , , as.character(k), drop = TRUE], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), project = FALSE, grid = TRUE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_leaflet(x, min_val, max_val, title = unit,
                            project = FALSE, grid = TRUE)
  }
}

.draw_satellite_sf <- function(x, unit, year) {
  min_val <- min(x$VALUE, na.rm = TRUE)
  max_val <- max(x$VALUE, na.rm = TRUE)
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_leaflet(
        x[x$YEAR == k, ], min_val, max_val,
        title = paste("Year:", k, "<br>", unit), grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_leaflet(x, min_val, max_val, title = unit, grid = FALSE)
  }
}
