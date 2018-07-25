#### Load packages ####
library(ggplot2)
library(rgdal)
library(raster)
library(dplyr)
library(ggsn)
library(RColorBrewer)

#### Read data ####
bight <- readOGR("Data/Mapping/world_bay_gulf", layer = "world_bay_gulf")
eez <- readOGR("Data/Mapping/eez", layer = "eez")
netherlands_coast <- readOGR("Data/Mapping/netherlands_coast", layer = "world_countries_coasts")
bat <- raster("Data/Mapping/Bathymetry/bat.grd")
belgium <- readOGR("Data/Mapping/belgium", layer = "worldcountries")


#### Fortify and organize ####
#coastline
bightfort <- fortify(bight)
netherlands_coastfort <- fortify(netherlands_coast)
netherlands_coastfort <- netherlands_coastfort[80:5110,]

#bathymetry
batfort <- as(bat, "SpatialPixelsDataFrame")
batfort <- as.data.frame(batfort)

#Belgium
belfort <- fortify(belgium)
belfort[37:43, ]$long <- NA
belfort[37:43, ]$lat <- NA
eezfort <- fortify(eez)
eezfort <- eezfort[c(10:30),]
belnew <- rbind(belfort[1:36,], eezfort, belfort[44:203,])

#remove shapefiles from environment
rm(eez, bight, netherlands_coast, bat, belgium, belfort)

#### Create theme and colours ####
theme_plot <- theme_bw() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16))

col=rev(colorRampPalette(brewer.pal(9,"Blues"))(100))
col <- c(col, rep("white", times = 2))

#### Plot base map ####
base_map <- ggplot() + 
  coord_cartesian(xlim = c(2.2,3.7), ylim = c(51,51.9)) +
  theme_plot +  
  geom_raster(aes(x=x, y=y, fill = layer), data = batfort, interpolate = T)+
  scale_fill_gradientn(colours=col) + 
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "white") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group), size = 0.5, alpha = 0.8) +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.5, alpha = 0.6) 

#### Read station positions ####
station <- read.csv("Data/Positions/Jolien_locaties_zooplankton.csv", stringsAsFactors = F)
colnames(station) <- c("station", "long", "lat")
station$freq <- ifelse(station$station %in% c("ZG02", "215", "120", "130", "230", "330", "780", "710", "700"), "Monthly", "Seasonal")
