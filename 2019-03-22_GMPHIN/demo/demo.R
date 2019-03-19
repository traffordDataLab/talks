## Dentists in Bolton ##

# Source: Care Quality Commission
# Publisher URL: http://www.cqc.org.uk/about-us/transparency/using-cqc-data
# Licence: Open Government Licence 3.0

# Geospatial data derive from ONS Open Geography Portal under OGL 3.0

# load libraries ---------------------------
library(tidyverse) ;  library(sf) ; library(leaflet) ; library(htmltools)

# import data ---------------------------
df <- read_csv("https://www.cqc.org.uk/sites/default/files/13_March_2019_CQC_directory.csv", 
               skip = 4, col_names = TRUE) %>% 
  setNames(tolower(names(.)))

postcodes <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Postcodes/ONS_Postcode_Directory_Latest_Centroids/MapServer/0/query?where=UPPER(oslaua)%20like%20'%25E08000001%25'&outFields=pcds,oslaua,osward,long,lat&outSR=4326&f=json") %>% 
  select(postcode = pcds, area_code = oslaua, lon = long, lat)
boundary <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2018_Boundaries_1/MapServer/1/query?where=UPPER(lad18cd)%20like%20'%25E08000001%25'&outFields=lad18cd,lad18nm,long,lat&outSR=4326&f=json")

# tidy data ---------------------------
sf <- df %>%
  filter(str_detect(`service types`, "Dentist") & 
           `local authority` == "Bolton") %>%
  select(name, 
         type = `service types`, 
         address, postcode, 
         telephone = `phone number`, 
         website = `service's website (if available)`, 
         area_name = `local authority`) %>% 
  mutate(telephone = 
           case_when(
             is.na(telephone) ~ "",
             TRUE ~ paste0("0", telephone)
             ),
         website = fct_explicit_na(website, na_level = "")) %>% 
  left_join(., postcodes, by = "postcode") %>% 
  st_as_sf(crs = 4326, coords = c("lon", "lat"))

# visualise data ---------------------------
popup <- ~paste0(
  "<div class='popupContainer'>",
  "<h3>", sf$name, "</h3>",
  "<table class='popupLayout'>",
  "<tr>",
  "<td>Address</td>",
  "<td>", sf$address, "</td>",
  "</tr>",
  "<td>Postcode</td>",
  "<td>", sf$postcode, "</td>",
  "</tr>",
  "</tr>",
  "<td>Telephone</td>",
  "<td>", sf$telephone, "</td>",
  "</tr>",
  "<tr>",
  "<td>Website</td>",
  "<td><a href='", sf$website, "' target='_blank'>", sf$website, "</a></td>",
  "</tr>",
  "</table>",
  "</div>"
)

map <- leaflet(height = "100%", width = "100%") %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(data = boundary, stroke = TRUE, weight = 2, color = "#212121", opacity = 1) %>% 
  addMarkers(data = sf, popup = popup) %>% 
  addControl("<strong>Dentists in Bolton</strong><br /><em>Source: Care Quality Commission</em>", position = 'topright')

browsable(
  tagList(list(
    tags$head(
      tags$style("
                 html, body {height: 100%; margin: 0;}
                 .leaflet-control-layers-toggle {height: 44; width: 44;}
                 .leaflet-bar a, .leaflet-bar a:hover, .leaflet-touch .leaflet-bar a, .leaflet-touch .leaflet-bar a:hover {height: 34px; width: 34px; line-height: 34px;}
                 .info {width: 300px;}
                 .popupContainer{overflow: scroll;}
                 .popupLayout{width: 100%;}
                 .popupLayout td{vertical-align: top; border-bottom: 1px dotted #ccc; padding: 3px;}
                 .popupLayout td:nth-child(1){width: 1%; font-weight: bold; white-space: nowrap;}
                 ")
      ),
    map
      ))
  )
