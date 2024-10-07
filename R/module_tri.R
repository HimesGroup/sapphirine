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
              inputId = ns("pollutant"),
              label = NULL,
              choice = .tri_list,
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
              inputId = ns("data_type"),
              label = "Total emissions by",
              choices = c("County", "Census Tract"),
              selected = "Census Tract"
            ),
            hr(),
            selectizeInput(
              inputId = ns("year"),
              label = "Year",
              choice = sort(unique(tri$county$YEAR), decreasing = TRUE),
              multiple = TRUE,
              selected = sort(unique(tri$county$YEAR), decreasing = TRUE)[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("tri_smap"), height = "67vh"))
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("tri_mmap")))
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
        req(input$pollutant)
        req(input$data_type)
        req(input$year)
      }, {
        p <- .draw_tri(input$pollutant, input$data_type, input$year)
        output$tri_map <- renderLeaflet(p)
        if (length(input$year) == 1) {
          output$tri_smap <- renderLeaflet(p)
        } else {
          output$tri_mmap <- renderUI(p)
        }
      })
    }
  )
}

.tri_list <- list(
  `Fugitive Air Emission` = "FUGITIVE.AIR",
  `Statck Air Emission` = "STACK.AIR"
)

.subset_tri <- function(type, year) {
  type <- switch(
    type,
    "County" = "county",
    "Census Tract" = "census_tract",
    "Zip Code" = "zip_code",
    )
  x <- tri[[type]]
  x[x$YEAR %in% year, ]
}

.draw_tri <- function(pollutant, type, year) {
  x <- .subset_tri(type, year)
  value_idx <- match(pollutant, names(x))
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
  .draw_airquality_leaflet(x = x, min_val = min_val, max_val = max_val,
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
