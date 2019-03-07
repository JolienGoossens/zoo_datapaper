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
batfort[batfort$layer >=0,]$layer <- 0

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
  theme(panel.background = element_rect(fill = "moccasin"),
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title = element_blank(),
        axis.text = element_text(size = 10))

col <- colorRampPalette(brewer.pal(9,"Blues"))(100)

#### Plot base map with north and scale ####
labs = parse(text=paste0(c(0, 20, 40, 60), "~m"))
base_map <- ggplot() + 
  coord_cartesian(xlim = c(2.2,3.7), ylim = c(51,51.9)) +
  theme_plot +  
  geom_raster(aes(x=x, y=y, fill = -layer), data = batfort, interpolate = T) +
  scale_fill_gradientn(colours=col, name = "Depth (m)", na.value = "transparent",
                       limits=c(0,60), breaks = c(0, 20, 40, 60), labels = labs,
                       guide = guide_colourbar(frame.colour = "black", ticks =F, reverse = T)) + 
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "moccasin", alpha = 0.5) +
  geom_vline(xintercept = seq(2.2, 3.7, 0.2), size = 0.1, colour = "gray20") + 
  geom_hline(yintercept = seq(51, 51.9, 0.2), size = 0.1, colour = "gray20") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.3) +
  scale_x_continuous(breaks = seq(2.2, 3.7, 0.2)) +
  scale_y_continuous(breaks = seq(51, 51.9, 0.2)) +
  north(data = eezfort, anchor = c(x = 3.76, y = 51.92), symbol = 4, scale = 0.12) +
  scalebar(data = eezfort, model = "WGS84",dd2km = T, dist = 10, st.dist = 0.04, anchor = c(x = 3.545, y = 51.89), st.size = 3.5) +
  geom_text(aes(x = c(seq(2.4, 3.7, 0.2)+0.005), y = 50.96, label = seq(2.4, 3.7, 0.2)), size=2.5, hjust = 0, vjust = 0,
            colour = "gray20", fontface = "bold") +
  geom_text(aes(x = 2.13, y = c(seq(51.2, 51.9, 0.2)-0.015), label = seq(51.2, 51.9, 0.2)), size=2.5, hjust = 0, vjust = 0,
            colour = "gray20", fontface = "bold") +
  geom_text(aes(x = (2.2+0.005), y = 50.96, label = "2.2° E"), size=2.5, hjust = 0, vjust = 0, colour = "gray20", fontface = "bold") +
  geom_text(aes(x = 2.13, y = (51-0.015), label = "51° N"), size=2.5, hjust = 0, vjust = 0, colour = "gray20", fontface = "bold") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

#### Read station positions ####
station <- read.csv("Data/Positions/Jolien_locaties_zooplankton.csv", stringsAsFactors = F)
colnames(station) <- c("station", "long", "lat")
station$freq <- ifelse(station$station %in% c("ZG02", "215", "120", "130", "230", "330", "780", "710", "700"), "Monthly", "Seasonal")

map_stations <- base_map +
  geom_point(data = station, aes(x = long, y = lat, shape = freq), size = 4) +
  scale_shape_manual(values = c("Monthly" = 18, "Seasonal" = 20), name = "Stations") +
  geom_text(data = station, aes(x = long, y = lat, label = station), size = 4, vjust = 0, , nudge_y = 0.02) +
  theme(legend.position = c(0.88, 0.14),
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0)),
        legend.key = element_rect(fill = alpha("white", 0)),
        legend.spacing = unit(0.5, "cm"),
        legend.key.height = unit(0.5, "cm"))

# Saved as Map_station_762x605px manually to maintain ratio as seen in window
