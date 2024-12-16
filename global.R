library(shiny)
library(sf)
library(stars)
library(shinycssloaders)
library(leaflet)
library(leafsync)
library(plotly)

options(rsconnect.max.bundle.size = 3145728000)

## NDVI
ndvi <- readRDS(file.path("sapphirine_data", "RDS", "ndvi.RDS"))

## ADI data
adi <- readRDS(file.path("sapphirine_data", "RDS", "adi.RDS"))

## SVI data
svi <- readRDS(file.path("sapphirine_data", "RDS", "svi.RDS"))

## TRI
tri <- readRDS(file.path("sapphirine_data", "RDS", "tri.RDS"))

## Violation data
violation <- readRDS(file.path("sapphirine_data", "RDS", "violation.RDS"))

## Crime data
crime <- readRDS(file.path("sapphirine_data", "RDS", "crime.RDS"))

## Traffic data
traffic <- readRDS(file.path("sapphirine_data", "RDS", "traffic.RDS"))

## CDC PLACES
places <- readRDS(file.path("sapphirine_data", "RDS", "cdc_places.RDS"))

## EPA air quality data in global scope
airquality <- readRDS(file.path("sapphirine_data", "RDS", "epa_airquality.RDS"))

## satellite
satellite <- readRDS(file.path("sapphirine_data", "RDS", "satellite_airquality.RDS"))

