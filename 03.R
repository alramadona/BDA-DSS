library(sf)
library(rtweet)
library(dplyr)
library(ggplot2)

ID_area <- st_read("map/IDN_adm1x.shp")
ID_area_df <- ID_area %>% st_set_geometry(NULL)

dat <- read.csv("data/tweetsLoc_0120.csv")
names(dat)
pnts <- select(dat, lon,lat) 
pnts <- pnts[c(1:100),]

names(pnts)
names(pnts) <- c("x","y")
str(pnts)

# create a points collection
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts),
                                     function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

# intersect and extract state name
pnts$region <- apply(st_intersects(ID_area, pnts_sf, sparse = FALSE), 2, 
                     function(col) { 
                       ID_area[which(col), ]$NAME_1
                     })

cek <- cbind(dat[c(1:100),],pnts)
