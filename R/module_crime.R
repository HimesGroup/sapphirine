crimeUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Crime",
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(
            12,
            h3("Crime Incidents from the Philadelphia Police Department",
               style = "font-weight: bold; color: #DC4C64; margin-top:10px; margin-bottom: 15px"),
            selectizeInput(
              inputId = ns("crime_var"),
              label = NULL,
              choice = .crime_list,
              multiple = FALSE,
              selected = "Total",
              ),
            h4("Source Info", style = "font-weight: bold; color: #332D2D"),
            p(span("Source: ", style = "font-weight: bold; color: orange"),
              a(href = "https://opendataphilly.org/datasets/crime-incidents/",
                "OpenDataPhilly", target = "_blank"),
              "(Philadelphia County only)"),
            p(span("Geographic unit: ", style = "font-weight: bold; color: orange"),
              "Census tract"),
            helpText(
              icon("circle-info"),
              "Crime reports lacking geographic coordinates are excluded in calculating incidents.",
              style = "color: #3B71CA;"
            ),
            hr(),
            selectizeInput(
              inputId = ns("year"),
              label = "Year",
              choice =  sort(unique(crime$census_tract$YEAR), decreasing = TRUE),
              multiple = TRUE,
              selected = sort(unique(crime$census_tract$YEAR), decreasing = TRUE)[1]
            )
          )
        ),
        width = 3
      ),
      mainPanel(
        conditionalPanel(
          "input.year.length == 1",
          ns = ns,
          withSpinner(leafletOutput(ns("crime_smap"), height = "67vh"))
        ),
        conditionalPanel(
          "input.year.length > 1",
          ns = ns,
          withSpinner(uiOutput(ns("crime_mmap")))
        )
      )
    )
  )
}

crimeServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({
        req(input$crime_var)
        req(input$year)
      }, {
        p <- .draw_crime(input$crime_var, input$year)
        if (length(input$year) == 1) {
          output$crime_smap <- renderLeaflet(p)
        } else {
          output$crime_mmap <- renderUI(p)
        }
      })
    }
  )
}

.crime_list <- list(
  `Total Incidents` = "Total",
  `Aggravated Assault Firearm` = "Aggravated.Assault.Firearm",
  `Aggravated Assault No Firearm` = "Aggravated.Assault.No.Firearm",
  `All Other Offenses` = "All.Other.Offenses",
  `Arson` = "Arson",
  `Burglary Non-Residential` = "Burglary.Non.Residential",
  `Burglary Residential` = "Burglary.Residential",
  `Disorderly Conduct` = "Disorderly.Conduct",
  `Embezzlement` = "Embezzlement",
  `Forgery and Counterfeiting` = "Forgery.and.Counterfeiting",
  `Fraud` = "Fraud",
  `Gambling Violations` = "Gambling.Violations",
  `Homicide - Criminal` = "Homicide...Criminal",
  `Homicide - Gross Negligence` = "Homicide...Gross.Negligence",
  `Homicide - Justifiable` = "Homicide...Justifiable",
  `Liquor Law Violations` = "Liquor.Law.Violations",
  `Motor Vehicle Theft` = "Motor.Vehicle.Theft",
  `Narcotic / Drug Law Violations` = "Narcotic...Drug.Law.Violations",
  `Offenses Against Family and Children` = "Offenses.Against.Family.and.Children",
  `Other Assaults` = "Other.Assaults",
  `Other Sex Offenses (Not Commercialized)` = "Other.Sex.Offenses..Not.Commercialized.",
  `Prostitution and Commercialized Vice` = "Prostitution.and.Commercialized.Vice",
  `Public Drunkenness` = "Public.Drunkenness",
  `Rape` = "Rape",
  `Receiving Stolen Property` = "Receiving.Stolen.Property",
  `Robbery Firearm` = "Robbery.Firearm",
  `Robbery No Firearm` = "Robbery.No.Firearm",
  `Theft from Vehicle` = "Theft.from.Vehicle",
  `Thefts` = "Thefts",
  `Vagrancy/Loitering` = "Vagrancy.Loitering",
  `Vandalism/Criminal Mischief` = "Vandalism.Criminal.Mischief",
  `Weapon Violations` = "Weapon.Violations"
)

.draw_crime <- function(crime_var, year) {
  x <- crime$census_tract[crime$census_tract$YEAR %in% year, ]
  value_idx <- match(crime_var, names(x))
  names(x)[value_idx] <- "VALUE"
  min_val <- min(x$VALUE, na.rm = TRUE) * 0.99 # small buffer
  max_val <- max(x$VALUE, na.rm = TRUE) * 1.01
  location <- crime$location[crime$location$YEAR %in% year, ]
  if (crime_var != "Total") {
    crime_idx <- which(.crime_list == crime_var)
    location <- location[location$text_gener == names(.crime_list)[crime_idx], ]
  }
  if (length(year) > 1) {
    plist <- lapply(year, function(k) {
      .draw_crime_leaflet(
        location[location$YEAR == k, ], x[x$YEAR == k, ],
        min_val, max_val, title = paste("Year:", k), grid = FALSE
      )
    })
    do.call(sync, plist)
  } else {
    .draw_crime_leaflet(location, x, min_val, max_val, title = NULL,
                        grid = FALSE)
  }
}

.draw_crime_leaflet <- function(location, x, min_val, max_val, title,
                                grid = FALSE) {
  p <- .draw_airquality_leaflet(x = x, min_val = min_val, max_val = max_val,
                                zoom = 11, num_fmt = "%.0f",
                                title = title, grid = grid)
  if (nrow(location) > 0) {
    p |>
      addLayersControl(
        overlayGroups = "Crime Incident Locations",
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addMarkers(
        data = location,
        label = ~ text_gener,
        clusterOptions = leaflet::markerClusterOptions(),
        group = "Crime Incident Locations"
      )
  } else {
    p
  }
}
