trafficUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Traffic",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("Traffic Volume",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            selectizeInput(
              inputId = ns("traffic_var"),
              label = NULL,
              choice = .traffic_var_list,
              multiple = FALSE,
              selected = "AADT",
              ),
            checkboxInput(inputId = ns("log2"), label = "Log2 scale", value = FALSE),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://data-pennshare.opendata.arcgis.com/datasets/PennShare::rmstraffic-traffic-volumes/about",
                "RMSTRAFFIC (Traffic Volumes)", target = "_blank"),
              "(Philadelphia County only)"
              ),
            p(span("Road Segment Key: ", style = "font-weight: bold; color: orange"),
              "RMSTRAFFIC_LRS_KEY (length: 23)"),
            ## p(span("Preprocessing: ", style = "font-weight: bold; color: orange"),
            ##   "2D Cartesian lengths of roads are obtained in",
            ##   a(href = "https://epsg.io/32618", "EPSG:32618 (WGS 84 / UTM zone 18N)"),
            ##   "projected coordinate system to calculate Daily Vehicle Miles Traveled."),
            hr(),
            radioButtons(
              inputId = ns("fig_type"),
              label = "Figure type",
              choices = list(`Map` = "map", `Line Graph` = "line_graph"),
              selected = "map",
              inline = TRUE
            ),
            conditionalPanel(
              "input.fig_type == 'map'",
              ns = ns,
              selectizeInput(
                inputId = ns("year"),
                label = "Year",
                choice = sort(unique(traffic$YEAR), decreasing = TRUE),
                multiple = TRUE,
                selected = sort(unique(traffic$YEAR), decreasing = TRUE)[1]
              )
            ),
            conditionalPanel(
              "input.fig_type == 'line_graph'",
              ns = ns,
              selectizeInput(
                inputId = ns("location"),
                label = "Segments of interest (up to 8)",
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
            withSpinner(leafletOutput(ns("traffic_smap"), height = "67vh")),
            hr(),
            p(strong("Click a road segment of interest to view historical change."),
              style = "color: #3CB371; margin-bottom: 20px"),
            plotlyOutput(ns("trend"))
          ),
          conditionalPanel(
            "input.year.length > 1",
            ns = ns,
            withSpinner(uiOutput(ns("traffic_mmap")))
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

trafficServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        req(input$traffic_var)
        req(input$year)
        input$log2
      }, {
        p <- .draw_traffic(input$traffic_var, input$year, input$log2)
        if (length(input$year) == 1) {
          output$traffic_smap <- renderLeaflet(p)
        } else {
          output$traffic_mmap <- renderUI(p)
          output$trend <- renderPlotly(NULL)
        }
      })
      observeEvent({
        req(input$traffic_smap_shape_click)
      }, {
        click_info <- input$traffic_smap_shape_click
        x <- traffic[traffic$RMSTRAFFIC_LRS_KEY %in% click_info$id, ] |>
          st_drop_geometry()
        value_idx <- match(input$traffic_var, names(x))
        names(x)[value_idx] <- "VALUE"
        ylabel <- names(.traffic_var_list)[.traffic_var_list == input$traffic_var]
        title <- paste0("RMSTRAFFIC_LRS_KEY: ", click_info$id)
        output$trend <- renderPlotly(
          .trend_plot(x, title, fmt_y = "%{y}", ylab = ylabel)
        )
      })
      updateSelectizeInput(
        session, inputId = "location",
        choices = unique(traffic$RMSTRAFFIC_LRS_KEY), selected = NULL,
        server = TRUE
      )
      observeEvent({
        req(input$traffic_var)
        req(input$location)
        input$log2
      }, {
        x <- traffic[traffic$RMSTRAFFIC_LRS_KEY %in% input$location, ] |>
          st_drop_geometry()
        value_idx <- match(input$traffic_var, names(x))
        names(x)[value_idx] <- "VALUE"
        id_idx <- match("RMSTRAFFIC_LRS_KEY", names(x))
        names(x)[id_idx] <- "LOCATION"
        fmt_y <- "%{y}"
        if (input$log2) {
          x$VALUE <- log2(x$VALUE)
          x$VALUE[is.infinite(x$VALUE)] <- 0
          fmt_y <- "%{y:.3f}"
        }
        ## ylabel <- names(.traffic_var_list)[.traffic_var_list == input$traffic_var]
        p <- .line_plot(x, fmt_y = "%{y}", ylab = input$traffic_var)
        output$line <- renderPlotly(p)
      })
    }
  )
}

.traffic_var_list <- list(
  `Annual Average Daily Traffic (AADT)` = "AADT",
  `Daily Vehicle Miles Traveled (DVMT)` = "DVMT"
)

.draw_traffic <- function(traffic_var, year, log2 = FALSE) {
  x <- traffic[traffic$YEAR %in% year, ]
  value_idx <- match(traffic_var, names(x))
  names(x)[value_idx] <- "VALUE"
  x$LABEL <- x$VALUE
  if (log2) {
    x$VALUE <- log2(x$VALUE)
    x$VALUE[is.infinite(x$VALUE)] <- 0
  }
  min_val <- min(x[["VALUE"]], na.rm = TRUE)
  max_val <- max(x[["VALUE"]], na.rm = TRUE)
  unit <- traffic_var
  if (log2) {
    unit <- paste0("log<sub>2</sub>(", traffic_var, ")")
  }
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_traffic_leaflet(x[x$YEAR == k, ], min_val, max_val,
                            title = paste("Year:", k, "<br>", unit))
    })
    do.call(sync, plist)
  } else {
    .draw_traffic_leaflet(x, min_val, max_val, title = unit)
  }
}

.draw_traffic_leaflet <- function(x, min_val, max_val, title = NULL, zoom = 11) {
  leaflet() |>
    addTiles() |>
    setView(lng = -75.1652, lat = 39.9525, zoom = zoom) |>
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Recenter",
      onClick = JS("function(btn, map){map.setView([39.9525, -75.1652], 11);}")
    )) |>
    addPolylines(
      data = st_transform(x, 4326),
      color = ~.get_pal(min_val, max_val)(VALUE),
      weight = 2, opacity = 1,
      dashArray = NULL, fillOpacity = 0.6,
      highlightOptions = highlightOptions(
        weight = 3, color = "#444444", dashArray = NULL,
        fillOpacity = 0.9, bringToFront = FALSE
      ),
      layerId = ~ RMSTRAFFIC_LRS_KEY,
      label = paste0(x$RMSTRAFFIC_LRS_KEY, ": ", prettyNum(x$LABEL))
    ) |>
    addLegend(
      position = "bottomright",
      pal = .get_pal(min_val, max_val, reverse = FALSE),
      values = c(min_val, max_val),
      labFormat = labelFormat(transform = function(k) sort(k, decreasing = TRUE)),
      title = title
    )
}
