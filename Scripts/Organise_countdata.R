#### Load packages ####
source("Scripts/Map_stations.R")
library(lubridate)
library(dplyr)

#### Read data ####
count <- read.csv2("Data/Count/ZooplanktonCounts.csv", stringsAsFactors = F)

#### Preliminary data treatment ####
count$X_id <- as.factor(count$X_id)
count$Taxon <- as.factor(count$Taxon)
count$Date <- parse_date_time(count$Date, orders = "dmy HM")
count$Month <- month(count$Date)
count$Year <- year(count$Date)

# issue with station names
base_map + 
  geom_point(data = station, aes(x = long, y = lat, shape = freq), size = 4) +
  scale_shape_manual(values = c("Monthly" = 18, "Seasonal" = 20), name = "Stations") +
  geom_text(data = station, aes(x = long, y = lat, label = station), vjust = 0, , nudge_y = 0.02) +
  geom_point(data = count[count$Station == "",], aes(x = Longitude, y = Latitude), colour = "red")

# rename some station levels 
count[count$Station == "lw02",]$Station <- "LW02"
count[count$Station == "lw01",]$Station <- "LW01"
count[count$Station == "",]$Station <- "ZG02" # "" seems to coincide entirely with ZG02
count$Station <- as.factor(count$Station)

#### Timeline with data availability ####
unique(count$Station)
count$Station <- factor(count$Station, levels = c("LW02", "W10", "LW01", "W09", "W07bis", "435", "421", "W08","780", "330", "ZG02", "710", "230", "215", "700", "130", "120"))

# add frequency of station visits
statjoin <- dplyr::select(station, - long, -lat)
colnames(statjoin) <- c("Station", "Freq")
count <- join(count, statjoin)


