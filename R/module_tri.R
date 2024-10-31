triUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "TRI",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3(a(href = "https://www.epa.gov/trinationalanalysis/air-releases",
                 "Air Releases of Toxic Release Inventory (TRI) Chemicals",
                 style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px")),
            selectizeInput(
              inputId = ns("tri_var"),
              label = NULL,
              choice = .tri_var_list,
              multiple = FALSE,
              selected = "FUGITIVE.AIR"
            ),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://www.epa.gov/toxics-release-inventory-tri-program/2023-tri-preliminary-dataset-basic-data-files",
                "TRI Basic Data Files", target = "_blank")),
            p(span("Fugitive air emission: ", style = "font-weight: bold; color: orange"),
              "All releases of the EPCRA Section 313 chemicals to the air that are not",
              "released through stacks, vents, ducts, pipes, or any other confined air stream."
              ),
            p(span("Stack air emission: ", style = "font-weight: bold; color: orange"),
              "All releases of the EPCRA Section 313 chemicals to the air that occur",
              "through stacks, vents, ducts, pipes, or any other confined air stream."
              ),
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
              label = "Total emissions by",
              choices = .tri_type_list,
              selected = "census_tract"
            ),
            hr(),
            conditionalPanel(
              "input.fig_type == 'map'",
              ns = ns,
              selectizeInput(
                inputId = ns("year"),
                label = "Year",
                choice = sort(unique(tri$county$YEAR), decreasing = TRUE),
                multiple = TRUE,
                selected = sort(unique(tri$county$YEAR), decreasing = TRUE)[1]
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
            withSpinner(leafletOutput(ns("tri_smap"), height = "67vh")),
            hr(),
            p(strong("Click a polygon of interest to view historical change."),
              style = "color: #3CB371; margin-bottom: 20px"),
            plotlyOutput(ns("trend"))
          ),
          conditionalPanel(
            "input.year.length > 1",
            ns = ns,
            withSpinner(uiOutput(ns("tri_mmap")))
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

triServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        ## Ensure that it is triggered when non-NULL values are given
        req(input$tri_var)
        req(input$data_type)
        req(input$year)
      }, {
        p <- .draw_tri(input$data_type, input$tri_var, input$year)
        output$tri_map <- renderLeaflet(p)
        if (length(input$year) == 1) {
          output$tri_smap <- renderLeaflet(p)
        } else {
          output$tri_mmap <- renderUI(p)
        }
        output$trend <- renderPlotly(NULL)
      })
      observeEvent({
        req(input$tri_smap_shape_click)
      }, {
        click_info <- input$tri_smap_shape_click
        x <- tri[[input$data_type]]
        x <- x[x$LOCATION %in% click_info$id, ] |>
          st_drop_geometry()
        x <- x[order(x$YEAR), ]
        p <- x |>
          plot_ly(x = ~ YEAR, y = ~ FUGITIVE.AIR,
                  type = "scatter", mode = "lines+markers",
                  name = "Fugitive Air Emission",
                  hovertemplate = "%{y:.3f}") |>
          add_trace(y = ~ STACK.AIR, name = "Stack Air Emission") |>
          layout(title = click_info$id, xaxis = list(title = "Year"),
                 yaxis = list(title = "Pound"),
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
          choices = unique(tri[[input$data_type]]$LOCATION), selected = NULL,
          server = TRUE
        )
        output$line <- renderPlotly(NULL)
        output$trend <- renderPlotly(NULL)
      })
      observeEvent({
        req(input$tri_var)
        req(input$location)
      }, {
        x <- tri[[input$data_type]]
        value_idx <- match(input$tri_var, names(x))
        names(x)[value_idx] <- "VALUE"
        ylabel <- names(.tri_var_list)[.tri_var_list == input$tri_var] |>
          paste0(" (Pound)")
        x <- x[x$LOCATION %in% input$location, ] |>
          st_drop_geometry()
        p <- .line_plot(x, fmt_y = "%{y:.3f}", ylab = ylabel)
        output$line <- renderPlotly(p)
      })
    }
  )
}

.tri_var_list <- list(
  `Fugitive Air Emission` = "FUGITIVE.AIR",
  `Stack Air Emission` = "STACK.AIR"
)

.tri_type_list <- list(
  `County` = "county",
  `Census Tract` =  "census_tract"
)

.subset_tri <- function(type = c("census_tract", "county"), year) {
  type <- match.arg(type)
  x <- tri[[type]]
  x[x$YEAR %in% year, ]
}

.draw_tri <- function(type, var, year) {
  x <- .subset_tri(type, year)
  value_idx <- match(var, names(x))
  names(x)[value_idx] <- "VALUE"
  min_val <- min(x$VALUE, na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x$VALUE, na.rm = TRUE) * 1.01
  location <- tri$location[tri$location$YEAR %in% year, ]
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_tri_leaflet(
        location[location$YEAR == k, ], x[x$YEAR == k, ], min_val, max_val,
        title = paste("Year:", k, "<br>", "(pound)"), grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_tri_leaflet(location, x, min_val, max_val, "(pound)", grid = FALSE)
  }
}

.draw_tri_leaflet <- function(location, x, min_val, max_val, title,
                              grid = FALSE) {
  .draw_leaflet(x = x, min_val = min_val, max_val = max_val,
                title = title, grid = grid) |>
    addLayersControl(
      overlayGroups = "Facility Locations",
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    addMarkers(
      data = location,
      popup = paste(
        paste0("<b>", location$FACILITY.NAME, "</b>"),
        paste0("Fugitive Air Emission: ", sprintf("%.2f", location$FUGITIVE.AIR), " pound"),
        paste0("Stack Air Emission: ", sprintf("%.2f", location$STACK.AIR), " pound"),
        sep = "<br/>"
      ),
      label = ~ FACILITY.NAME,
      clusterOptions = leaflet::markerClusterOptions(),
      group = "Facility Locations"
    )
}
