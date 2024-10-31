adiUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "ADI",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("Area Deprivation Index (ADI)",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://www.neighborhoodatlas.medicine.wisc.edu/",
                "Neighborhood Atlas®", target = "_blank")),
            p(span("Geographic unit: ", style = "font-weight: bold; color: orange"),
              "Census block group"),
            p(span("Value: ", style = "font-weight: bold; color: orange"),
              "National ADI percentiles (1-100; least to most disadvantaged)"),
            p(span("PH: ", style = "font-weight: bold"),
              "Suppression due to low population and/or housing"),
            p(span("GQ: ", style = "font-weight: bold"),
              "Suppression due to a high group quarters population"),
            p(span("PH-GQ: ", style = "font-weight: bold"),
              "Suppression due to both types of suppression criteria"),
            p(span("QDI: ", style = "font-weight: bold"),
              "Questionable Data Integrity"),
            hr(),
            radioButtons(
              inputId = ns("fig_type"),
              label = "Figure type",
              choices = list(`Map` = "map", `Line Graph` = "line_graph"),
              selected = "map",
              inline = TRUE
            ),
            hr(),
            conditionalPanel(
              "input.fig_type == 'map'",
              ns = ns,
              selectizeInput(
                inputId = ns("year"),
                label = "Year",
                choices =  sort(unique(adi$YEAR), decreasing = TRUE),
                multiple = TRUE,
                selected = sort(unique(adi$YEAR), decreasing = TRUE)[1]
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
            withSpinner(leafletOutput(ns("adi_smap"), height = "67vh")),
            hr(),
            p(strong("Click a polygon of interest to view historical change."),
              style = "color: #3CB371; margin-bottom: 20px"),
            plotlyOutput(ns("trend"))
          ),
          conditionalPanel(
            "input.year.length > 1",
            ns = ns,
            withSpinner(uiOutput(ns("adi_mmap")))
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

adiServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        ## req(input$adi_var)
        req(input$year)
      }, {
        p <- .draw_adi(input$year)
        if (length(input$year) == 1) {
          output$adi_smap <- renderLeaflet(p)
        } else {
          output$adi_mmap <- renderUI(p)
          output$trend <- renderPlotly(NULL)
        }
      })
      observeEvent({
        req(input$adi_smap_shape_click)
      }, {
        click_info <- input$adi_smap_shape_click
        x <- adi[adi$LOCATION %in% click_info$id, ] |>
          st_drop_geometry()
        value_idx <- match("ADI_NATRANK", names(x))
        names(x)[value_idx] <- "VALUE"
        output$trend <- renderPlotly(
          .trend_plot(x, click_info$id, fmt_y = "%{y}",
                      ylab = "National ADI Percentile")
        )
      })
      updateSelectizeInput(
        session, inputId = "location",
        choices = unique(adi$LOCATION), selected = NULL,
        server = TRUE
      )
      observeEvent({
        input$location
      }, {
        if (!is.null(input$location)) {
          x <- adi[adi$LOCATION %in% input$location, ] |>
            st_drop_geometry()
          value_idx <- match("ADI_NATRANK", names(x))
          names(x)[value_idx] <- "VALUE"
          p <- .line_plot(x, ylab = "National ADI Percentile")
          output$line <- renderPlotly(p)
        } else {
          output$line <- renderPlotly(NULL)
        }
      })
    }
  )
}

.draw_adi <- function(year) {
  x <- adi[adi$YEAR %in% year, ]
  suppressWarnings(x$VALUE <- as.numeric(x[["ADI_NATRANK"]])) # need numeric for color scale
  min_val <- min(x[["VALUE"]], na.rm = TRUE) * 0.99
  max_val <- max(x[["VALUE"]], na.rm = TRUE) * 1.01
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_adi_leaflet(x[x$YEAR == k, ], min_val, max_val,
                        title = paste0("Year: ", k))
    })
    do.call(sync, plist)
  } else {
    .draw_adi_leaflet(x, min_val, max_val)
  }
}

.draw_adi_leaflet <- function(x, min_val, max_val, title = NULL) {
  leaflet() |>
    addTiles() |>
    setView(lng = -75.1652, lat = 39.9525, zoom = 11) |>
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Recenter",
      onClick = JS("function(btn, map){map.setView([39.9525, -75.1652], 11);}")
    )) |>
    addLegend(
      position = "bottomright",
      pal = .get_pal(min_val, max_val, reverse = FALSE),
      values = c(min_val, max_val),
      labFormat = labelFormat(transform = function(k) sort(k, decreasing = TRUE)),
      title = title
    ) |>
  addPolygons(
    data = st_transform(x, 4326),
    fillColor = ~.get_pal(min_val, max_val)(VALUE),
    weight = 1, opacity = 1,
    color = "#444444",
    dashArray = NULL, fillOpacity = 0.6,
    highlightOptions = highlightOptions(
      weight = 3, color = "#444444", dashArray = NULL,
      fillOpacity = 0.9, bringToFront = FALSE
    ),
    layerId = ~ LOCATION,
    label = paste0(x$LOCATION, ": ", x$ADI_NATRANK)
  )
}
