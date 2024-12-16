placesUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "CDC PLACES",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("CDC PLACES: Local Data for Better Health",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            selectizeInput(
              inputId = ns("places_var"),
              label = NULL,
              choice = .places_var_list,
              multiple = FALSE,
              selected = "Any Disability"
            ),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://data.cdc.gov/browse?category=500+Cities+%26+Places&sortBy=newest&utf8",
                "PLACES Data Portal", target = "_blank")),
            p(span("Release: ", style = "font-weight: bold; color: orange"),
              "August 2024"),
            p(span("Measure: ", style = "font-weight: bold; color: orange"),
              textOutput(ns("measure"), inline = TRUE)),
            p(span("Year: ", style = "font-weight: bold; color: orange"),
              textOutput(ns("year"), inline = TRUE)),
            ## p(span("Value: ", style = "font-weight: bold; color: orange"),
            ##   "Model-based mean estimate with 95% confidence interval"),
            hr(),
            radioButtons(
              inputId = ns("data_type"),
              label = "Geographic unit",
              ## disable temporarily Grid
              ## choices = c("County", "Census Tract", "Zip Code", "Grid"),
              choices = .places_type_list,
              selected = "census_tract"
            ),
            hr(),
            conditionalPanel(
              "input.data_type == 'county'",
              ns = ns,
              selectizeInput(
                inputId = ns("value_type"),
                label = "Estimate",
                choice = sort(unique(places$county$data_value_type)),
                multiple = FALSE,
                selected = sort(unique(places$county$data_value_type))[1]
              )
            ),
            conditionalPanel(
              "input.data_type == 'census_tract'",
              ns = ns,
              selectizeInput(
                inputId = ns("value_type"),
                label = "Estimate",
                choice = sort(unique(places$census_tract$data_value_type)),
                multiple = FALSE,
                selected = sort(unique(places$census_tract$data_value_type))[1]
              )
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        withSpinner(leafletOutput(ns("places_smap"), height = "67vh")),
      )
    )
  )
}

placesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        ## Ensure that it is triggered when non-NULL values are given
        req(input$places_var)
        req(input$data_type)
        req(input$value_type)
      }, {
        x <- .subset_places(input$data_type, input$places_var, input$value_type)
        output$measure <- renderText(unique(x$measure))
        output$year <- renderText(unique(x$year))
        output$places_smap <- renderLeaflet(.draw_places(x))
      })
    }
  )
}

.places_var_list <- list(
  `Disability` = list(
    "Any Disability", "Cognitive Disability", "Hearing Disability",
    "Independent Living Disability", "Mobility Disability",
    "Self-care Disability", "Vision Disability"
  ),
  `Health Outcomes` = list(
    "All Teeth Lost", "Arthritis", "Cancer (non-skin) or Melanoma", "COPD",
    "Coronary Heart Disease", "Current Asthma", "Depression", "Diabetes",
    "High Blood Pressure", "High Cholesterol", "Obesity", "Stroke"
  ),
  `Health Risk Behaviors` = list(
    "Binge Drinking", "Current Cigarette Smoking", "Physical Inactivity",
    "Short Sleep Duration"
  ),
  `Health Status` = list(
    "Frequent Mental Distress", "Frequent Physical Distress", "General Health"
  ),
  `Health-Related Social Needs` =  list(
    "Food Insecurity", "Food Stamps", "Housing Insecurity",
    "Lack of Social/Emotional Support", "Social Isolation",
    "Transportation Barriers", "Utility Services Threat"
  ),
  `Prevention` = list(
    "Annual Checkup", "Cholesterol Screening", "Colorectal Cancer Screening",
    "Dental Visit", "Health Insurance", "High Blood Pressure Medication",
    "Mammography"
  )
)

.places_type_list <- list(
  `County` = "county",
  `Census Tract` =  "census_tract"
)

.subset_places <- function(type = c("census_tract", "county"), var, value_type) {
  type <- match.arg(type)
  x <- places[[type]]
  x <- x[x$short_question_text == var, ]
  x[x$data_value_type == value_type, ]
}

.draw_places <- function(x) {
  ## x <- .subset_places(type, var, value_type)
  unit <- paste0("(", unique(x$data_value_unit), ")")
  value_idx <- match("data_value", names(x))
  names(x)[value_idx] <- "VALUE"
  min_val <- min(x$VALUE, na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x$VALUE, na.rm = TRUE) * 1.01
  .draw_leaflet(x, min_val, max_val, grid = FALSE, num_fmt = "%.1f",
                title = unit)
}

