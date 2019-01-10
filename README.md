# Sensor-based Analysis of Pollution in the Philadelphia Region (SAPPhiRe)
SAPPhiRe is an app that integrates pollution data measured throughout the Philadelphia region into an interactive geospatial visualization tool. SAPPhiRe includes data downloaded from crowd-sourced databases in addition to original data collected by the Himes Group.
### Rationale
Mass marketed sensors have created unique opportunities for citizen science in measuring environmental pollution. Certain crowd-sourced databases offer tools for visualizing the corresponding data, but these tools are limited in functionality, data availability, and geographic/temporal specificity.

SAPPhiRe combines data downloaded from two of these databases, AirCasting and PurpleAir, with data collected via the Himes Group's own AirCasting sensors, and allows these data to be visualized within an interactive map. The dynamic user interface allows researchers to subset the aggregate data with a high degree of specificity tailored to their specific needs, which is conducive to drawing better conclusions.
### Dependencies
SAPPhiRe was created using RStudio's Shiny. Running SAPPhiRe locally requires R, with the following packages installed: shiny, lubridate, dplyr, ggplot2, ggmap, grDevices, shinyWidgets, grid, gridExtra, gtable, leaflet, raster, shinyjs, mapview.
