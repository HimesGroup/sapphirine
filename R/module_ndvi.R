ndviUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "NDVI",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("Landsat Normalized Difference Vegetation Index (NDVI)",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2",
                "USGS Landsat 8 Level 2, Collection 2, Tier 1", target = "_blank")),
            p(span("Preprocessing: ", style = "font-weight: bold; color: orange"),
              a(href = "https://developers.google.com/earth-engine/guides/ic_visualization",
                "Pixels obscured by cloud, cloud shadow, or saturation are masked out in NDVI calculations.",
                target = "_blank")),
            p(span("Low NDVI (< 0.1): ", style = "font-weight: bold"),
              "Areas of barren rock, sand, or snow"),
            p(span("Moderate NDVI (0.2-0.5): ", style = "font-weight: bold"),
              "Sparse vegetation such as shrubs and grasslands or senescing crops"),
            p(span("High NDVI (0.6-0.9): ", style = "font-weight: bold"),
              "Dense vegetation such as that found in temperate and tropical",
              "forests or crops at their peak growth stage"),
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
              choice = sort(unique(ndvi$county$year), decreasing = TRUE),
              multiple = TRUE,
              selected = sort(unique(ndvi$county$year), decreasing = TRUE)[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("ndvi_smap"), height = "67vh"))
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("ndvi_mmap"))),
          hr(),
          uiOutput(ns("trend"))
        )
      )
    )
  )
}

ndviServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        ## Ensure that it is triggered when non-NULL values are given
        req(input$data_type)
        req(input$year)
      }, {
        if (input$data_type == "Grid") {
          p <- .draw_ndvi_grid(input$data_type, input$year)
        } else {
          p <- .draw_ndvi_sf(input$data_type, input$year)
        }
        if (length(input$year) == 1) {
          output$ndvi_smap <- renderLeaflet(p)
        } else {
          output$ndvi_mmap <- renderUI(p)
        }
      })
      ## observeEvent({
      ##   req(input$ndvi_smap_shape_click)
      ## }, {
      ##   browser()
      ##   click <- input$ndvi_smap_shape_click
      ## })
    }
  )
}

.subset_ndvi <- function(type, year) {
  type <- switch(
    type,
    "Grid" = "grid",
    "County" = "county",
    "Census Tract" = "census_tract",
    "Zip Code" = "zip_code"
  )
  x <- ndvi[[type]]
  if (type == "grid") {
    ## year <- as.character(year)
    ## x[, , , year, drop = TRUE]
    x[, , , as.character(year), drop = TRUE]
  } else {
    x[x$year %in% as.integer(year), ]
  }
}


.draw_ndvi_grid <- function(type, year) {
  x <- .subset_ndvi(type, year)
  min_val <- min(x[[1]], na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x[[1]], na.rm = TRUE) * 1.01
  ## min_val = -1
  ## max_val = 1
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_ndvi_leaflet(
        x[, , , as.character(k), drop = TRUE], min_val, max_val,
        title = paste0("Year: ", k), project = FALSE, grid = TRUE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_ndvi_leaflet(x, min_val, max_val, project = TRUE, grid = TRUE)
  }
}

.draw_ndvi_sf <- function(type, year) {
  x <- .subset_ndvi(type, year)
  value_idx <- match("value", names(x))
  names(x)[value_idx] <- "VALUE"
  min_val <- min(x$VALUE, na.rm = TRUE)
  max_val <- max(x$VALUE, na.rm = TRUE)
  ## min_val = -1
  ## max_val = 1
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_ndvi_leaflet(x[x$year == k, ], min_val, max_val,
                         title = paste0("Year: ", k), grid = FALSE,
                         col_reverse = FALSE, palette = "Greens")
    })
    do.call(sync, plist)
  } else {
    .draw_ndvi_leaflet(x, min_val, max_val, grid = FALSE,
                       col_reverse = FALSE, palette = "Greens")
  }
}

.draw_ndvi_leaflet <- function(...) .draw_airquality_leaflet(...)
