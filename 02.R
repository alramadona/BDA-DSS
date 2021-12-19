# Exploratory Data Analysis -----------------------------------------------

library(rtweet)
library(ggplot2)
load("data/tweets.RData")

names(tweets_streamID)
## use lat_lng to recover full information geolocation data
tweets_streamID <- lat_lng(tweets_streamID)
names(tweets_streamID)

# install.packages("sf")
library(sf)

ID_area <- st_read("map/IDN_adm1x.shp")

ggplot(data = ID_area) +
  geom_sf(fill = NA) +
  geom_point(data = tweets_streamID, 
             aes(x = lng, y = lat, col = "red"), size = 1)


# Mobility Analysis -------------------------------------------------------

dat <- read.csv("data/tweetsLoc_0120.csv")

nrow(dat)
names(dat)
unique(dat$date)

str(dat)
dat$X <- NULL

unique(dat$WoY)

ggplot(data = ID_area) +
  geom_sf(fill = NA) +
  geom_point(data = dat, 
             aes(x = lon, y = lat, col = WoY), size = 1)

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

#datLinks <- datLinks[datLinks['r'] != datLinks['c'],]
datLinks <- datLinks[datLinks['r'] == 8,]

library(geosphere)

names(datLinks)
flows <- gcIntermediate(datLinks[,c(7,6)], datLinks[,c(9,8)], sp = TRUE, addStartEnd = TRUE)

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
