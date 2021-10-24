# Sensor-based Analysis of Pollution in the Philadelphia Region with Information on Neighborhoods and the Environment (SAPPHIRINE)
SAPPHIRINE is an online web-app and R package that integrates pollution and other geospatial data gathered throughout the Greater Philadelphia Area for interactive visualization and customizable data retrieval to facilitate hypothesis generation for neighborhood-level health studies in Philadelphia.

Authors: Colin Christie, Sherrie Xie, Avantika R. Diwadkar, Rebecca E. Greenblatt, Alexandra Rizaldi, Blanca E. Himes.

## Rationale
A wide range of datasets containing geographically distributed measures of the environment and social factors is currently available, and as low-cost sensors and other devices become increasingly used, the volume of these data will continue to grow. Because such factors influence many health outcomes, researchers with varied interests often repeat tasks related to gathering and preparing these data for studies. We created Sensor-based Analysis of Pollution in the Philadelphia Region with Information on Neighborhoods and the Environment (SAPPHIRINE), offered as a web application and R package, to integrate pollution and geospatial data relevant to investigators, citizen scientists, and policy makers in the Greater Philadelphia Area. SAPPHIRINE’s capabilities include providing interactive maps and customizable data retrieval to aid in the visual identification of pollution and other factor hotspots as well as hypothesis generation regarding relationships among these factors and health outcomes.

Data for pollution originate from AirCasting and PurpleAir crowdsourced databases (http://aircasting.org/mobile_map, https://www.purpleair.com/sensorlist), and data for crime, Area Deprivation Index, and traffic originate from OpenDataPhilly (https://www.opendataphilly.org/dataset/crime-incidents), Neighborhood Atlas (https://www.neighborhoodatlas.medicine.wisc.edu/), and PennDOT (https://data-pennshare.opendata.arcgis.com/datasets/rmstraffic-traffic-volumes/data), respectively. Data for pollution were also colected with AirBeam sensors in a pilot study by the Himes Lab, funded by [CEET](http://ceet.upenn.edu/). EPA pollution data were downloaded from the EPA Air Data Portal (https://aqs.epa.gov/aqsweb/airdata/download_files.html).

## Installing R package

`sapphirine` is not on CRAN, so you will have to install it directly from Github using `devtools`. 

If you do not have the `devtools` package installed, you will have to run the first line in the code below as well. 

```
# install.packages('devtools')
library(devtools)
devtools::install_github("HimesGroup/sapphirine", subdir = "pkg/sapphirine")
```

## Dependencies
SAPPHIRINE was created using RStudio's Shiny. Running the app locally requires R, with the following packages installed: shiny, lubridate, dplyr, shinyWidgets, leaflet, raster, shinyjs, mapview, data.table, feather, RColorBrewer, DescTools, stringr, tools, gstat, prevR, leafsync.
The package dependences are: DescTools, RColorBrewer, dplyr, gstat, leaflet, lubridate, magrittr, prevR, raster, sp.
