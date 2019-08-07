# Sensor-based Analysis of Pollution in the Philadelphia Region with Information on Neighborhoods and the Environment (SAPPHIRINE)
SAPPHIRINE is an online web-app that integrates pollution and other geospatial data gathered throughout the Greater Philadelphia Area into an interactive map display that optimizes data visualization in a unique, revolutionary manner. 

Authors: Colin Christie, Sherrie Xie, Avantika Diwadkar, Rebecca E. Greenblatt, Blanca Himes.

### Rationale
Personal sensors offer new opportunities in measuring environmental pollution. Some crowdsourced databases offer tools for visualizing sensor-based data, but these tools are limited in functionality, data inclusion, and geographic specificity. Hence, We created SAPPHIRINE, whose extensive adjustability of user-defined parameters, high capacity for geographic specificity, ecletic scope of integrated data, and comprehensive data display make it a uniquely advantageous tool for geospatial data analysis.

Data for pollution originate from AirCasting and PurpleAir crowdsourced databases (http://aircasting.org/mobile_map, https://www.purpleair.com/sensorlist), and data for crime, poverty, and traffic originate from OpenDataPhilly (https://www.opendataphilly.org/dataset/crime-incidents), Neighborhood Atlas (https://www.neighborhoodatlas.medicine.wisc.edu/), and PennDOT (https://data-pennshare.opendata.arcgis.com/datasets/rmstraffic-traffic-volumes/data), respectively. Data for pollution were also colected with AirBeam sensors in a pilot study by the Himes Lab, funded by [CEET](http://ceet.upenn.edu/).

### Dependencies
SAPPHIRINE was created using RStudio's Shiny. Running the app locally requires R, with the following packages installed: shiny, lubridate, dplyr, ggplot2, ggmap, grDevices, shinyWidgets, grid, gridExtra, gtable, leaflet, raster, shinyjs, mapview, data.table, feather, colorRamps.
