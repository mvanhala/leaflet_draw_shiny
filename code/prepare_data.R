
# https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/WPI/Pub150bk.pdf
# http://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_pageLabel=msi_portal_page_62&pubCode=0015


library(httr)
library(readr)
library(dplyr)
library(purrr)
library(tibble)
library(htmltools)

countries <- read_csv("data/country_codes.csv", col_types = cols(.default = "c"))

quakes_usgs <- GET("https://earthquake.usgs.gov/fdsnws/event/1/query?format=text&starttime=2016-01-01&endtime=2017-01-01&minmagnitude=5.0")

quakes <- content(quakes_usgs, as = "text") %>%
  read_delim(delim = "|") %>%
  mutate(rad = Magnitude ^ 6) %>%
  select(lat = Latitude, lng = Longitude, magnitude = Magnitude, rad)

ports_raw <- rgdal::readOGR("data/ports", "WPI")

ports <- ports_raw

ports@data <- ports@data %>%
  set_names(tolower(colnames(.))) %>%
  as_tibble %>%
  select(port_name, country, harborsize, harbortype, shelter) %>%
  mutate_if(is.factor, as.character) %>%
  left_join(countries, by = c("country" = "code")) %>%
  mutate(
    harborsize = recode(
      harborsize,
      "V" = "Very small",
      "S" = "Small",
      "M" = "Medium",
      "L" = "Large"
    ),
    shelter = recode(
      shelter, 
      "E" = "Excellent",
      "G" = "Good", 
      "F" = "Fair", 
      "P" = "Poor",
      "N" = "None"
    ),
    harbortype = recode(
      harbortype, 
      "CN" = "Coastal Natural",
      "CB" = "Coastal Breakwater", 
      "CT" = "Coastal Tide Gate",
      "RN" = "River Natural",
      "RB" = "River Basin",
      "N" = "None",
      "RT" = "River Tide Gate",
      "LC" = "Lake or Canal",
      "OR" = "Open Roadstead",
      "TH" = "Typhoon Harbor"
    ),
    txt = paste(
      port_name,
      coalesce(name, "Unknown"),
      paste0("Harbor size: ", harborsize), 
      paste0("Harbor type: ", harbortype),
      paste0("Shelter afforded: ", shelter),
      sep = "<br>"
    ),
    txt = map(txt, HTML)
  )

write_rds(quakes, "ports-earthquakes/data/quakes.rds")
write_rds(ports, "ports-earthquakes/data/ports.rds")


