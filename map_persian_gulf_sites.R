# create map of study sites -------------------------------
rm(list=ls()) #remove previous variable assignments

# load libraries
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(grid)

# load site data
pgsites <- read.csv("UAE-Oman survey sites_7.31.19.csv", head=T, stringsAsFactors = F)

# set sites
pg.sites <-data.frame(longitude = pgsites$Longitude..E.
                    , latitude = pgsites$Latitude..N.
                    , Site = pgsites$Site)

# set google api
source("C:/Users/Jeremy/Box Sync/R_functions/Google_maps_API_token.R")

# get basemap 
pg <- get_googlemap(center = c(55.16416, 25.37026), maptype = "terrain"
                    , source = "google", zoom = 6
                    , style='feature:all|element:labels|visibility:off')

# make map of study sites
pgSiteMap <- ggmap(pg) +
  scale_y_continuous(limits = c(24, 28)) +
  scale_x_continuous(limits = c(50, 59)) +
  geom_point(data = pg.sites, mapping = aes(x = longitude, y = latitude)) +
  geom_label_repel(aes(x = longitude, y = latitude, label = Site)
                   , min.segment.length = 0.005, direction='both'
                   , data = pg.sites) + 
  ylab("Latitude") +
  xlab("Longitude") +
  geom_text(aes(x = 52.6, y = 26.2, label = 'Persian Gulf'), colour = I("cadetblue4")) +
  geom_text(aes(x = 58.2, y = 24.5, label = 'Gulf of Oman'), colour = I("cadetblue4"))

# make world map with box inset for study sites
worldmap <- get_googlemap(center = c(55.16416, 25.37026), maptype = "terrain"
                    , source = "google", zoom = 3
                    , style='feature:all|element:labels|visibility:off')

worldmap_with_box <- ggmap(worldmap) +
    geom_hline(yintercept=63.2) +
    geom_vline(xintercept = 111) +
    geom_rect(xmin = 50, xmax = 59, ymin = 24, ymax = 28,
            fill = NA, colour = "black", size = 1) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ylab("") +
  xlab("") 

# plot study site map with world map inset
pgSiteMap + 
  inset(ggplotGrob(worldmap_with_box), xmin = 49, xmax = 52, ymin = 25.5, ymax = 28.5)
