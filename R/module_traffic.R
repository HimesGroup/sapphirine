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
              choice = list(
                `Annual Average Daily Traffic (AADT)` = "AADT",
                `Daily Vehicle Miles Traveled (DVMT)` = "DVMT"
                ),
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
            p(span("Preprocessing: ", style = "font-weight: bold; color: orange"),
              "2D Cartesian lengths of roads are obtained in",
              a(href = "https://epsg.io/32618", "EPSG:32618 (WGS 84 / UTM zone 18N)"),
              "projected coordinate system to calculate Daily Vehicle Miles Traveled."),
            hr(),
            selectizeInput(
              inputId = ns("year"),
              label = "Year",
              choice = sort(unique(traffic$YEAR), decreasing = TRUE),
              multiple = TRUE,
              selected = sort(unique(traffic$YEAR), decreasing = TRUE)[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("traffic_smap"), height = "67vh"))
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("traffic_mmap")))
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
        }
      })
    }
  )
}

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
      .draw_traffic_leaflet(x[x$YEAR == k, ], min_val, max_val, traffic_var,
                            title = paste("Year:", k, "<br>", unit))
    })
    do.call(sync, plist)
  } else {
    .draw_traffic_leaflet(x, min_val, max_val, traffic_var, title = unit)
  }
}

.draw_traffic_leaflet <- function(x, min_val, max_val, value_represented,
                                  title = NULL, zoom = 11) {
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
      label = paste0(value_represented, ": ", prettyNum(x$LABEL))
    ) |>
    addLegend(
      position = "bottomright",
      pal = .get_pal(min_val, max_val, reverse = FALSE),
      values = c(min_val, max_val),
      labFormat = labelFormat(transform = function(k) sort(k, decreasing = TRUE)),
      title = title
    )
}
