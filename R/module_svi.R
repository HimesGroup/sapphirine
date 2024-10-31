sviUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "SVI",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("CDC/ATSDR Social Vulnerability Index (SVI)",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            selectizeInput(
              inputId = ns("svi_var"),
              label = NULL,
              choice = .svi_var_list,
              multiple = FALSE,
              selected = "RPL_THEMES",
              ),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://www.atsdr.cdc.gov/placeandhealth/svi/index.html",
                "CDC/ATSDR Social Vulnerability Index (SVI)", target = "_blank")),
            p(span("Value: ", style = "font-weight: bold; color: orange"),
              "Percentile ranking values range from 0 to 1, with higher values",
              "indicating greater social vulnerability"),
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
              choices = .svi_type_list,
              selected = "census_tract"
            ),
            hr(),
            conditionalPanel(
              "input.fig_type == 'map'",
              ns = ns,
              selectizeInput(
                inputId = ns("year"),
                label = "Year",
                choice =  sort(unique(svi[[1]]$YEAR), decreasing = TRUE),
                multiple = TRUE,
                selected = sort(unique(svi[[1]]$YEAR), decreasing = TRUE)[1]
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
            withSpinner(leafletOutput(ns("svi_smap"), height = "67vh")),
            hr(),
            p(strong("Click a polygon of interest to view historical change."),
              style = "color: #3CB371; margin-bottom: 20px"),
            plotlyOutput(ns("trend"))
          ),
          conditionalPanel(
            "input.year.length > 1",
            ns = ns,
            withSpinner(uiOutput(ns("svi_mmap")))
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

sviServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        req(input$data_type)
        req(input$svi_var)
        req(input$year)
      }, {
        p <- .draw_svi(input$data_type, input$svi_var, input$year)
        if (length(input$year) == 1) {
          output$svi_smap <- renderLeaflet(p)
        } else {
          output$svi_mmap <- renderUI(p)
          output$trend <- renderPlotly(NULL)
        }
      })
      observeEvent({
        req(input$svi_smap_shape_click)
      }, {
        click_info <- input$svi_smap_shape_click
        x <- svi[[input$data_type]]
        x <- x[x$LOCATION %in% click_info$id, ] |>
          st_drop_geometry()
        x <- x[order(x$YEAR), ]
        p <- x |>
          plot_ly(x = ~ YEAR, y = ~ RPL_THEMES,
                  type = "scatter", mode = "lines+markers",
                  name = "Overall Summary Ranking",
                  hovertemplate = "%{y:.3f}") |>
          add_trace(y = ~ RPL_THEME1, name = "Socioeconomic Status") |>
          add_trace(y = ~ RPL_THEME2, name = "Household Characteristics") |>
          add_trace(y = ~ RPL_THEME3, name = "Racial & Ethnic Minority Status") |>
          add_trace(y = ~ RPL_THEME4, name = "Housing Type & Transportation") |>
          layout(title = click_info$id, xaxis = list(title = "Year"),
                 yaxis = list(title = "SVI Theme Score"),
                 hovermode = "x unified")
        output$trend <- renderPlotly(p)
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
          choices = unique(ndvi[[input$data_type]]$LOCATION), selected = NULL,
          server = TRUE
        )
        output$line <- renderPlotly(NULL)
        output$trend <- renderPlotly(NULL)
      })
      observeEvent({
        req(input$svi_var)
        req(input$location)
      }, {
        x <- svi[[input$data_type]]
        value_idx <- match(input$svi_var, names(x))
        names(x)[value_idx] <- "VALUE"
        ylabel <- names(.svi_var_list)[.svi_var_list == input$svi_var]
        x <- x[x$LOCATION %in% input$location, ] |>
          st_drop_geometry()
        p <- .line_plot(x, fmt_y = "%{y:.3f}", ylab = ylabel)
        output$line <- renderPlotly(p)
      })
    }
  )
}

.svi_var_list <- list(
  `Overall Summary Ranking` = "RPL_THEMES",
  `Socioeconomic Status` = "RPL_THEME1",
  `Household Characteristics` = "RPL_THEME2",
  `Racial & Ethnic Minority Status` = "RPL_THEME3",
  `Housing Type & Transportation` = "RPL_THEME4"
)

.svi_type_list <- list(
  `County` = "county",
  `Census Tract` =  "census_tract"
)

.draw_svi <- function(type = c("census_tract", "county"), var, year) {
  type <- match.arg(type)
  x <- svi[[type]]
  x <- x[x$YEAR %in% year, ]
  names(x)[match(var, names(x))] <- "VALUE"
  x$VALUE <- ifelse(x$VALUE < 0, NA, x$VALUE)
  min_val <- min(x[["VALUE"]], na.rm = TRUE) * 0.99
  max_val <- max(x[["VALUE"]], na.rm = TRUE) * 1.01
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_svi_leaflet(x[x$YEAR == k, ], min_val, max_val,
                        title = paste0("Year: ", k))
    })
    do.call(sync, plist)
  } else {
    .draw_svi_leaflet(x, min_val, max_val)
  }
}

.draw_svi_leaflet <- function(x, min_val, max_val, title = NULL) {
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
      label = paste0(x$LOCATION, ": ", sprintf("%.3f", x$VALUE))
    )
}
