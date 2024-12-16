server <- function(input, output, session) {
  ndviServer("NDVI")
  adiServer("ADI")
  sviServer("SVI")
  triServer("TRI")
  violationServer("Violation")
  trafficServer("Traffic")
  crimeServer("Crime")
  airqualityServer("Airquality")
  satelliteServer("Satellite")
  placesServer("PLACES")
}

