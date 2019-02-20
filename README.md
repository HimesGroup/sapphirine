# Sensor-based Analysis of Pollution in the PHIladelphia Region with Information on Neighborhoods and the Environment (SAPPHIRINE)
SAPPHIRINE is an app that integrates pollution data measured throughout the Philadelphia region into an interactive geospatial visualization tool. Data correspond to crowd-sourced pollution sensor databases and publicly available environmental data.

### Rationale
Personal portable sensors have created unique opportunities for citizen science in measuring environmental pollution. Certain crowd-sourced databases offer tools for visualizing the corresponding data, but these tools are limited in functionality, data availability, and geographic/temporal specificity.

SAPPHIRINE currently combines data downloaded from AirCasting and PurpleAir websites corresponding to the greater Philadelphia area, data collected with AirBeam sensors in a pilot study by the Himes Lab's funded by [CEET](http://ceet.upenn.edu/). The dynamic user interface allows researchers to subset the aggregate data with a high degree of specificity tailored to their specific needs.

### Dependencies
SAPPHIRINE was created using RStudio's Shiny. Running the app locally requires R, with the following packages installed: shiny, lubridate, dplyr, ggplot2, ggmap, grDevices, shinyWidgets, grid, gridExtra, gtable, leaflet, raster, shinyjs, mapview.
