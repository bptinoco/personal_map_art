## first attempt at making a map for megarino
## Brendan P Tinoco

#### Basic Maps Vignette ##
 ## https://cran.r-project.org/web/packages/osmplotr/vignettes/basic-maps.html

## personal art map with R
 ## http://estebanmoro.org/post/2020-10-19-personal-art-map-with-r/
    
## library
    library(osmdata); library(dplyr); library(ggplot2)
    library(tigris); require(sf)

## Box -118.905991,34.106609,-118.164413,34.446638

## 
    min_lon <- -118.905991; max_lon <- -118.164413
    min_lat <- 34.106609; max_lat <- 34.446638
    bbx <- rbind(x=c(min_lon,max_lon),y=c(min_lat,max_lat))
    colnames(bbx) <- c("min","max")
    
## features
    available_tags("highway")
    
## collect all the differences between highways and other roads
    highways <- bbx %>%
        opq() %>%
        add_osm_feature(key = "highway",
                        value = c("motorway", "trunk",
                                  "primary","secondary", 
                                  "tertiary","motorway_link",
                                  "trunk_link","primary_link",
                                  "secondary_link",
                                  "tertiary_link")) %>%
        osmdata_sf()
    
## leggo with just the highways baby
    ggplot() +
        geom_sf(data = highways$osm_lines,
                aes(color = highway),
                size = 0.4,
                alpha = 0.65) +
        theme(legend.position = "none") + theme_void()

## streets
    streets <- bbx %>%
        opq() %>%
        add_osm_feature(key = "highway",
                        value = c("residential", "living street",
                                  "service", "unclassified",
                                  ))
    
## get the county shape files
    counties_CA <- counties(state="CA",cb=T,class="sf",)
    counties_CA <- st_crop(counties_CA,
                           xmin = min_lon, xmax = max_lon,
                           ymin = min_lat, ymax = max_lat)

## map the counties
    ggplot() +
        geom_sf(data = counties_CA, fill = "gray", lwd = 0) +
        coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])),
                 ylim = c(min(bbx[2,]), max(bbx[2,])),
                 expand = FALSE) +
        theme(legend.position = F) + theme_void()
        
## get the water files
    get_water <- function(county_GEOID){
        area_water("CA", county_GEOID, class = "sf")
    }
    water <- do.call(rbind, 
                     lapply(counties_CA$COUNTYFP,get_water))
    water <- st_crop(water,
                     xmin=min_lon,xmax=max_lon,
                     ymin=min_lat,ymax=max_lat)

## take a look
    ggplot() +
        geom_sf(data = counties_CA, lwd = 0) +
        geom_sf(data = water, inherit.aes = F,
                col = "blue") +
        coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])),
                 ylim = c(min(bbx[2,]), max(bbx[2,])),
                 expand = FALSE) +
        theme(legend.position = F) + theme_void()

## cut out the water polygons from the counties polygon
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}
counties_CA <- st_erase(counties_CA,water)
    
## how does it look without the water polygons and no county lines?
    ggplot() +
        geom_sf(data = counties_CA, lwd = 0) +
        coord_sf(xlim = c(min(bbx[1,]), max(bbx[1,])),
                 ylim = c(min(bbx[2,]), max(bbx[2,])),
                 expand = FALSE) +
        theme(legend.position = F) + theme_void()

## 

