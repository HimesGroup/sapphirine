violationUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "L&I Violation",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("Asthma-related licenses and inspections code violations",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            selectizeInput(
              inputId = ns("violation_var"),
              label = NULL,
              choice = .violation_list,
              multiple = FALSE,
              selected = "total",
              ),
            checkboxInput(ns("pop_adj"), label = "Violations per 100 people"),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://opendataphilly.org/datasets/licenses-and-inspections-code-violations/",
                "OpenDataPhilly", target = "_blank"),
              "(Philadelphia County only)"),
            ## p(span("Geographic unit: ", style = "font-weight: bold; color: orange"),
            ##   "Census tract"),
            p(span("Water damage codes: ", style = "font-weight: bold; color: orange"),
              paste(.water_damage, collapse = ", ")),
            p(span("Air contaminant codes: ", style = "font-weight: bold; color: orange"),
              paste(.air_contaminant, collapse = ", ")),
            p(span("Pest infestation codes: ", style = "font-weight: bold; color: orange"),
              paste(.pest_infestation, collapse = ", ")),
            helpText(
              icon("circle-info"),
              "Please check 'Violation Descriptions and Explanations' in",
              "the L&I Violations (Metadata) page for details.",
              style = "color: #3B71CA;"
            ),
            hr(),
            radioButtons(
              inputId = ns("data_type"),
              label = "Geographic Unit",
              choices = c("Census Tract", "Block Group"),
              selected = "Census Tract"
            ),
            hr(),
            selectizeInput(
              inputId = ns("year"),
              label = "Year",
              choice =  sort(unique(violation$location$YEAR), decreasing = TRUE),
              multiple = TRUE,
              selected = sort(unique(violation$location$YEAR), decreasing = TRUE)[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        ## withSpinner(leafletOutput(ns("violation_map"), height = "67vh"))
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("violation_smap"), height = "67vh"))
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("violation_mmap")))
        )
      )
    )
  )
}

violationServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        req(input$violation_var)
        req(input$data_type)
        req(input$year)
        input$pop_adj
      }, {
        p <- .draw_violation(input$data_type, input$violation_var,
                             input$year, input$pop_adj)
        output$violation_map <- renderLeaflet(p)
        if (length(input$year) == 1) {
          output$violation_smap <- renderLeaflet(p)
        } else {
          output$violation_mmap <- renderUI(p)
        }
      })
    }
  )
}

.violation_list <- list(
  `Total Violations` = "total",
  `Water Damage Violations` = "water_damage",
  `Air Contaminant Violations` = "air_contaminant",
  `Pest Infestation Violations` = "pest_infestation"
)

.draw_violation <- function(type, violation_var, year, pop_adj) {
  if (type == "Census Tract") {
    x <- violation$census_tract[violation$census_tract$YEAR %in% year, ]
  } else {
    x <- violation$block_group[violation$block_group$YEAR %in% year, ]
  }
  value_idx <- match(violation_var, names(x))
  names(x)[value_idx] <- "VALUE"
  num_fmt <- "%.0f"
  if (pop_adj) {
    x$VALUE <- (x$VALUE / x$estimate) * 100
    ## Inf can be produced when violations exist, but no people
    x$VALUE <- ifelse(is.nan(x$VALUE) | is.infinite(x$VALUE), NA, x$VALUE)
    num_fmt <- "%.3f"
  }
  min_val <- min(x$VALUE, na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x$VALUE, na.rm = TRUE) * 1.01
  location <- violation$location[violation$location$YEAR %in% year, ]
  if (violation_var != "total") {
    location <- switch(
      violation_var,
      "water_damage" = location[location$violationc %in% .water_damage, ],
      "air_contaminant" = location[location$violationc %in% .air_contaminant, ],
      "pest_infestation" = location[location$violationc %in% .pest_infestation, ],
      )
  }
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_violation_leaflet(
        location[location$YEAR == k, ], x[x$YEAR == k, ], min_val, max_val,
        title = paste("Year:", k), num_fmt = num_fmt, grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_violation_leaflet(location, x, min_val, max_val, title = NULL,
                            num_fmt = num_fmt, grid = FALSE)
  }
}

.draw_violation_leaflet <- function(location, x, min_val, max_val, title,
                                    num_fmt, grid = FALSE) {
  p <- .draw_airquality_leaflet(x = x, min_val = min_val, max_val = max_val,
                                zoom = 11, num_fmt = num_fmt,
                                title = title, grid = grid)
  if (nrow(location) > 0) {
    p |>
      addLayersControl(
        overlayGroups = "Violation Locations",
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addMarkers(
        data = location,
        popup = paste(
          paste0("<b>", location$violationc, "</b>"),
          paste0(location$violatio_1),
          sep = "<br/>"
        ),
        label = ~ violationc,
        ## label = paste0(location$violationc, ": ", location$violatio_1),
        clusterOptions = leaflet::markerClusterOptions(),
        group = "Violation Locations"
      )
  } else {
    p
  }
}

.water_damage <- c(
  "14-704/1",  "14-704/2", "A-302.1/1", "CP-303","CP-304", "CP-342",
  "CP-349", "PM-302.4/1", "PM-302.4/5", "PM-304.4/1", "PM-304.8/6",
  "PM15-302.2", "PM15-304.16", "PM15-304.1D", "PM15-304.2", "PM15-304.6",
  "PM15-304.7"
)

.air_contaminant <- c("03-306/1", "03-306/2")

.pest_infestation <- c(
  "CP-306", "CP-307", "CP-308", "CP-327", "CP-336", "CP-337", "CP-349",
  "PM-303.4/1", "PM-303.4/2", "PM-303.5/1", "PM-303.5/2", "PM-303.5/3",
  "PM-303.5/4", "PM-304.2/1", "PM-304.2/2", "PM-304.8/19", "PM-304.8/20",
  "PM-304.8/6", "PM-305.3/1", "PM15-302.5", "PM15-304.16", "PM15-304.17",
  "PM15-304.5", "PM15-309.1", "PM15-309.3", "PM15-309.4"
)
