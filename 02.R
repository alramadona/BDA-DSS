# Exploratory Data Analysis -----------------------------------------------

library(rtweet)
library(ggplot2)
load("data/tweets.RData")

## use lat_lng to recover full information geolocation data
ID_loc <- lat_lng(tweets_streamID)
names(ID_loc)

library(sf)
ID_area <- st_read("map/IDN_adm1x.shp")

ggplot(data = ID_area) +
  geom_sf(fill = NA) +
  geom_point(data = ID_loc, 
             aes(x = lng, y = lat, col = "red"), size = 1)


dat <- read.csv("data/tweetsLoc_0120.csv")
dat$X <- NULL
str(dat)

ggplot(data = ID_area) +
  geom_sf(fill = NA) +
  geom_point(data = dat, 
             aes(x = lon, y = lat, col = PROV), size = 1)


library(dplyr)
datLinks <- dat %>% 
  select(date, move) %>% 
  arrange(date, move) %>% 
  group_by(move) %>% 
  summarize(n=n())

datLinks$r <- sub("-.*", "", datLinks$move)
datLinks$c <- sub(".*-", "", datLinks$move)

datLinks$r <- as.integer(datLinks$r)
datLinks$c <- as.integer(datLinks$c)

datLinks <- arrange(datLinks, r,c)

rc <- cbind(r=rep(1:34, each=34), c=rep(1:34, 34))
datLinks <- merge(rc,datLinks,by=c("r","c"),all.x=T)
datLinks$n[is.na(datLinks$n)] <- 0

datCentroids <- read.csv("data/IDN_prov_centroid.csv") 
names(datCentroids) <- c("ID_area","PROV","lat.o","lon.o")
datCentroids$lat.d <- datCentroids$lat.o
datCentroids$lon.d <- datCentroids$lon.o
names(datCentroids)

datLinks$ID_area <- datLinks$r
datLinks <- merge(datLinks,datCentroids[,c(1,3,4)], by="ID_area", all.x=T)

datLinks$ID_area <- datLinks$c
datLinks <- merge(datLinks,datCentroids[,c(1,5,6)], by="ID_area", all.x=T)

datLinks <- arrange(datLinks, r,c)

# library(networkD3)
# nodes <- dat %>%
#   group_by(PROV, ID_area) %>%
#   summarize(ID_mean = mean(ID_area)) %>%
#   ungroup() %>%
#   arrange(ID_mean)
# 
# nodes$ID_area <- nodes$ID_area-1
# datLinks$origin <- datLinks$r-1
# datLinks$destination <- datLinks$c-1
# 
# # forceNetwork(Links = datLinks, Nodes = nodes, Source = 'origin', Target = 'destination', 
# #              Value = 'n', NodeID = 'PROV', Group = 'ID_area', zoom = TRUE)
# 
# datLinks = datLinks[datLinks['r'] != datLinks['c'],]
# 
# sankeyNetwork(Links = datLinks, Nodes = nodes, Source = 'origin', Target = 'destination', 
#               Value = 'n', NodeID = 'PROV', fontSize = 8)

datLinks <- datLinks[datLinks['r'] != datLinks['c'],]
datLinks <- datLinks[datLinks['r'] == 8,]

library(geosphere)

names(datLinks)
flows <- gcIntermediate(datLinks[,c(8,7)], datLinks[,c(10,9)], sp = TRUE, addStartEnd = TRUE)

range(datLinks$n)

flows$counts <- log(datLinks$n+1)
range(flows$counts)

flows$origins <- datLinks$r
flows$destinations <- datLinks$c

library(leaflet)
library(RColorBrewer)

hover <- paste0(flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolylines(data = flows, weight = ~counts, label = hover, 
               group = ~origins, color = ~pal(origins)) %>%
  addLayersControl(overlayGroups = unique(flows$origins), 
                   options = layersControlOptions(collapsed = FALSE))
