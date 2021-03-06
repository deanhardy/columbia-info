rm(list=ls())

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(tigris)
library(leaflet)
library(readxl)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
bd = seq(from = 0.5, to = 5, by =0.5) ## define euclidian buffer distances

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/columbia-info')

# schools <- school_districts("SC", 'elementary')
# schools <- st_read(file.path(datadir, 'school-districts/school_districts')) %>%
#   st_transform(4326)
  
# leaflet(schools) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(fillColor = "white",
#               color = "black",
#               weight = 0.5)

#############################################
## generate 0.5 mi buffer rings out from EWS
#############################################
## EWS location
loc <- data.frame(
  place = 'loc',
  longitude = -81.030066,
  latitude = 33.995306)

loc2 <- st_as_sf(loc, coords = c('longitude', 'latitude'), crs = 4326) %>%
  st_transform(utm) # change to NAD83 with units of meters

## loop buffer calculation & outpout as new data frame
df <- NULL

for(i in bd) {
  OUT <- st_buffer(loc2, i * 1609.34) %>%
    st_transform(4326) %>%
    data.frame() %>%
    mutate(id = as.character(i))
  df <-rbind(OUT,df)
}

## reset geometry on loop output & reproject CRS for leaflet mapping
buf <- st_set_geometry(df, 'geometry') %>%
  st_transform(4326)


#############################################
## download demographic data for area
#############################################
library(tidycensus)
# options(tigris_use_cache = TRUE)

# all_vars <- load_variables(2016, 'acs5', cache = TRUE)
var = c(white = "B03002_003E", black = "B03002_004E",
        native_american = "B03002_005E", asian = "B03002_006E",
        hawaiian = "B03002_007E", other = "B03002_008E",
        multiracial = "B03002_009E", latinx = "B03002_012E", tot_pop = "B03002_001E",
        medhhinc = "B19049_001E", agghhinc = "B19025_001E", hu = "B25001_001E", ownocc = "B25003_002E")
CNTY = c('Richland', 'Lexington')

dem <- get_acs(geography = 'block group',
               variables = var,
               state = 'SC',
               county = CNTY,
               year = 2017,
               output = 'wide',
               geometry = TRUE,
               keep_geo_vars = TRUE)

dem2 <- dem %>%
  st_sf() %>%
  st_transform(alb) %>%
  mutate(sqkm_bg = as.numeric(st_area(geometry)) / 1e6,
         propPOC = 1 - (white/tot_pop)) %>%
  dplyr::select(GEOID, ALAND, AWATER, sqkm_bg, tot_pop, white, black, native_american, asian, hawaiian,
                other, multiracial, latinx, propPOC, medhhinc, agghhinc, hu, ownocc) %>%
  mutate(pwhite = round(white/tot_pop, 2), pblack = round(black/tot_pop, 2), pother = round(other/tot_pop, 2), 
         platinx = round(latinx/tot_pop, 2), popden = round(tot_pop/ALAND, 2),
         pland = round((ALAND * 0.000001)/sqkm_bg, 2), pownocc = round(ownocc/hu, 2)) %>%
  st_transform(4326)

## import school locations
scl <- st_read(file.path(datadir, "schools/schools.shp")) %>%
  st_transform(4326) %>%
  filter(CITY %in% c('Columbia', 'West Columbia'), SCHOOL_NAM != 'University of South Carolina') %>%
  mutate(category = ifelse(str_detect(SCHOOL_NAM, 'Elementary'), 'Elementary', 
                           ifelse(str_detect(SCHOOL_NAM, 'Middle'), 'Middle', 
                                  ifelse(str_detect(SCHOOL_NAM, 'High'), 'High', 'Other')))) %>%
  st_cast('POINT') %>%
  rename(SCHOOLID = ID)

## import school report cards
## data from https://screportcards.com/
rc <- read_excel(file.path(datadir, 'schools/report-cards-2018.xlsx'), sheet= 1, skip = 2)

scl2 <- left_join(scl, rc, by = 'SCHOOLID')

#############################################
## create maps
#############################################

factpal <- colorFactor(rainbow(8), buf$id)
bpal <- colorBin('Reds', dem2$medhhinc, 5, pretty = FALSE)
bpal2 <- colorBin('Blues', 100*dem2$pownocc, 5, pretty = FALSE)
pops <- paste("People of Color (%):", round(100*dem2$propPOC, 0), "<br>",
              "Black (%):", 100*dem2$pblack, "<br>",
              "Other race (%):", 100*dem2$pother, "<br>",
              "Latinx (%):", 100*dem2$platinx, "<br>",
              "White (%):", 100*dem2$pwhite, "<br>",
              "Median HH Income (US$):", round(dem2$medhhinc, 0), "<br>",
              "Housing Units (#):", dem2$hu, "<br>",
              "Owner-Occupied HU (%):", 100*dem2$pownocc)

## make custom school icon set
## with help from https://stackoverflow.com/questions/47064921/leaflet-legend-for-addawesomemarkers-function-with-icons
## https://fontawesome.com/
icon.ews <- makeAwesomeIcon(icon = 'university', markerColor = '#73000a', library = 'fa')

IconSet <- awesomeIconList(
  Elementary = makeAwesomeIcon(icon= 'bus', markerColor = 'green', library = "fa"),
  Middle = makeAwesomeIcon(icon= 'bus', markerColor = 'blue', library = "fa"),
  High = makeAwesomeIcon(icon= 'bus', markerColor = 'red', library = "fa"),
  Other = makeAwesomeIcon(icon= 'bus', markerColor = 'gray', library = "fa")
)

## awesome marker legend
## legend html generator:
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'>School Type</h4>"

  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 4px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 10px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

## map
m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  setView(lng = loc$longitude, lat = loc$latitude, zoom = 13) %>%
  addTiles(attribution = '<a href="https://www.census.gov/programs-surveys/acs/"> | US Census American Community Survey 2013-2017</a>') %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addScaleBar('bottomleft') %>%
  addAwesomeMarkers(data = loc, 
                  label = 'EWS',
                  icon = icon.ews) %>%
  addPolygons(data = dem2,
              group = 'Median Household Income',
              fillColor = ~bpal(dem2$medhhinc),
              color = 'grey',
              weight = 1,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolygons(data = dem2,
              group = 'Owner Occupied Housing',
              fillColor = ~bpal2(100*dem2$pownocc),
              fillOpacity = 0.5,
              color = 'grey',
              weight = 1,
              highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolygons(data = dem2,
              group = 'Demographic Info',
              color = 'grey',
              weight = 1,
              fillOpacity = 0,
              highlightOptions = highlightOptions(color = "red", weight = 2,bringToFront = TRUE),
              popup = pops) %>%
  addPolylines(data = buf, color = 'black', weight = 1.5,
               label = buf$id, 
               labelOptions = labelOptions(noHide = T),
               group = 'Miles to EWS') %>%
  addAwesomeMarkers(data = scl2, 
                    group = 'Schools',
                    icon = ~IconSet[category],
                    popup = paste("Name:", scl2$SCHOOL_NAM, "<br>",
                                  "Teacher/Student Ratio:", scl2$TEACH_STUD, "<br>",
                                  "Grades:", scl2$GRADES, "<br>",
                                  "Overall Rating:", scl2$RATE_OVERALL)) %>%
  addLayersControl(baseGroups = c('Open Street Map'),
                   overlayGroups = c('Miles to EWS', 'Schools', 'Median Household Income', 'Owner Occupied Housing'),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend('bottomright',
            group = 'Median Household Income',
            pal = bpal,
            values = dem2$medhhinc,
            title = 'Median HH Income') %>%
  addLegend('bottomright',
            group = 'Owner Occupied Housing',
            pal = bpal2,
            values = 100*dem2$pownocc,
            title = 'Owner Occupied Housing (%)') %>%
  addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft") %>%
  hideGroup(group = 'Owner Occupied Housing')
m

## export static map
# mapshot(m, file = 'ews-buf-distances.png')
        
## export as interactive html map
library(htmlwidgets)
saveWidget(m,
           file="/Users/dhardy/Dropbox/r_data/columbia-info/map.html",
           title = "Columbia, SC Information")
