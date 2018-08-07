#### Load packages ####
source("Scripts/Organise_countdata.R")
source("Scripts/Map_stations.R")

#### Data organization
# rename months
count$Month <- as.factor(count$Month)
count$Month <- plyr::revalue(count$Month, c(
  "1" = "January",
  "2" = "February",
  "3" = "March",
  "4" = "April",
  "5" = "May",
  "6" = "June",
  "7" = "July",
  "8" = "August",
  "9" = "September",
  "10" = "October",
  "11" = "November",
  "12" = "December"
))

# Summarize the  data
countmonsum <- dplyr::summarise(
  group_by(count, Station, Year, Month),
  Count = sum(Count)
)
countmonsum <- dplyr::summarise(
  group_by(data.frame(countmonsum), Station, Month),
  Years = length(Year),
  Count = sum(Count)
)

colnames(station) <- c("Station", "long", "lat", "freq")
countmonsum <- plyr::join(data.frame(countmonsum), station)

#### Plot months ####
base_map_month <- ggplot() + 
  coord_cartesian(xlim = c(2.2,3.4), ylim = c(51.05,51.9)) +
  theme_plot +
  geom_raster(aes(x=x, y=y, fill = -layer), data = batfort, interpolate = T)+
  scale_fill_gradientn(colours=col, name = "Depth (m)") + 
  geom_polygon(aes(x=long, y=lat, group=group), data = netherlands_coastfort, fill = "white") +
  geom_vline(xintercept = seq(2.2, 3.7, 0.2), size = 0.05, colour = "gray20") + 
  geom_hline(yintercept = seq(51, 51.9, 0.2), size = 0.05, colour = "gray20") +
  geom_path(data = bightfort, aes(x = long, y = lat, group = group), size = 0.3) +
  geom_path(data = belnew, aes(x = long, y = lat, group = group), size = 0.3) +
  scale_x_continuous(breaks = seq(2.2, 3.7, 0.2)) + # no axes, but keep in case changes
  scale_y_continuous(breaks = seq(51, 51.9, 0.2))

# Amount of years 
base_map_month +
  geom_point(data = countmonsum, aes(long, lat, size = Years), alpha = 0.7) +
  facet_wrap(~Month, nrow = 4) + theme(aspect.ratio = 1) +
  theme(strip.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = "none")
