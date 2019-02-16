rm(list=ls())

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(mapview)

## define variables
utm <- 2150 ## NAD83 17N
alb <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" ## http://spatialreference.org/ref/sr-org/albers-conic-equal-area-for-florida-and-georgia/
bd = seq(from = 0.5, to = 5, by =0.5) ## define buffer distances

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/columbia-info')



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
         pland = round((ALAND * 0.000001)/sqkm_bg, 2)) %>%
  st_transform(4326)

## import school locations
scl <- st_read(file.path(datadir, "schools/schools.shp")) %>%
  st_transform(4326) %>%
  filter(CITY %in% c('Columbia', 'West Columbia')) %>%
  st_cast('POINT')

#############################################
## create maps
#############################################

factpal <- colorFactor(rainbow(8), buf$id)
bpal <- colorBin('Reds', dem2$medhhinc, 5, pretty = FALSE)

m <- leaflet() %>%
  addTiles(group = 'Open Street Map') %>%
  setView(lng = loc$longitude, lat = loc$latitude, zoom = 13) %>%
  addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
  addScaleBar('bottomleft') %>%
  addPolygons(data = dem2,
              group = 'Demographic Info',
              fillColor = ~bpal(dem2$medhhinc),
              color = 'grey',
              weight = 1,
              fillOpacity = 0.5,
              popup = paste("People of Color (%):", 100*dem2$propPOC, "<br>",
                            "Black (%):", 100*dem2$pblack, "<br>",
                            "Other race (%):", 100*dem2$pother, "<br>",
                            "Latinx (%):", 100*dem2$platinx, "<br>",
                            "White (%):", 100*dem2$pwhite, "<br>",
                            "Estimated Median HH Income (US$):", round(dem2$medhhinc, 0), "<br>",
                            "Housing Units (#):", dem2$hu, "<br>",
                            "Owner-Occupied HU (%):", round(100*(dem2$ownocc/dem2$hu), 0))) %>%
  addPolylines(data = buf, color = 'black', weight = 2,
               label = buf$id, 
               labelOptions = labelOptions(noHide = T),
               group = 'Distance to Work') %>%
  addMarkers(data = scl, 
             group = 'Schools',
             label = scl$SCHOOL_NAM) %>%
  addLayersControl(baseGroups = c('Open Street Map'),
                   overlayGroups = c('Distance to Work', 'Schools', 'Demographic Info'),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend('bottomright',
            pal = bpal,
            values = dem2$medhhinc,
            title = 'Median HH Income')
m

## export static map
# mapshot(m, file = 'ews-buf-distances.png')
        
## export as interactive html map
library(htmlwidgets)
saveWidget(m, 
           file="/Users/dhardy/Dropbox/r_data/columbia-info/columbia.html",
           title = "Columbia, SC Information")
