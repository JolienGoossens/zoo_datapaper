#### Load packages ####
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

#### Read data ####
count <- read.csv2("Data/Count/ZooplanktonCounts.csv", stringsAsFactors = F)

#### Preliminary data treatment ####
count$X_id <- as.factor(count$X_id)
count$Taxon <- as.factor(count$Taxon)
count$Date <- parse_date_time(count$Date, orders = "dmy")
count$Month <- month(count$Date)
count$Year <- year(count$Date)

# rename some station levels 
#count[count$Station == "lw02",]$Station <- "LW02"
#count[count$Station == "lw01",]$Station <- "LW01"
#count[count$Station == "",]$Station <- "ZG02" # "" seems to coincide entirely with ZG02
count[count$Station == "W07BIS",]$Station <- "W07bis"

count$Station <- as.factor(count$Station)
count$Station <- factor(count$Station, levels = c("LW02", "W10", "LW01", "W09", "W07bis", "435", "421", "W08","780", "330", "ZG02", "710", "230", "215", "700", "130", "120"))

# add frequency of station visits
station <- read.delim("Data/Positions/Jolien_locaties_zooplankton.txt", stringsAsFactors=FALSE)

station <- station %>% dplyr::select(-ID)
colnames(station) <- c("station", "lat", "long", "depth")
station$station <- gsub(" ", "", station$station) #correct typos

station$freq <- ifelse(station$station %in% c("ZG02", "215", "120", "130", "230", "330", "780", "710", "700"), "Monthly", "Seasonal") #define frequency of station visits

statjoin <- dplyr::select(station, - long, -lat, - depth)
colnames(statjoin) <- c("Station", "Freq")
count <- join(count, statjoin)
rm(statjoin)

# make smaller data frame
count$Date2 <- lubridate::date(count$Date)
countsum <- dplyr::summarize(group_by(count, Date2, Year, Month, Station, Freq, Tripaction))
countsum <- as.data.frame(countsum)

#link tripactions to instrument
net <- read.csv("Data/Jo_lien.csv", sep=";", stringsAsFactors = F)
colnames(net) <- c("Tripaction","Instrument")

#net[net$Instrument == "Plankton Net Trawl 200\xb5m/1.0 m diam",]$Instrument <- "Apstein net"
net[net$Instrument == "Plankton Net WP2 200\xb5m/0.70m diam.",]$Instrument <- "WP2 net"
net[net$Instrument == "Plankton Net 10\xb5m",]$Instrument <- "Apstein net"
#net[net$Instrument == "Plankton Net Trawl 100\xb5m/1.0 m diam",]$Instrument <- "Apstein net"
net[net$Instrument == "Plankton Pump",]$Instrument <- "Plankton pump"
net[net$Instrument == "Plankton Net 60\xb5m",]$Instrument <- "Apstein net"
#net[net$Instrument == "Planktonnet 60\xb5m",]$Instrument <- "Apstein net"
net[net$Instrument == "Niskin Bottle 10L",]$Instrument <- "Niskin bottle"
net[net$Instrument == "Planktonnet WP2",]$Instrument <- "WP2 net"

countsum <- join(countsum, net)
#countsum[countsum$Tripaction == 101025,]$Instrument <- "WP2 net" #one missing value, added manually

#### Plot the data availability per station ####
ggplot(data = countsum, aes(x = Date2, y = Station)) +  
  geom_point(aes(shape = Instrument, fill = Instrument), size = 1.5) +
  scale_shape_manual(values = c(21,23,22), limits = c("Plankton pump", "Apstein net", "WP2 net")) + 
  scale_fill_manual(values = c("black", "gray80", "black"), limits = c("Plankton pump", "Apstein net", "WP2 net")) +
  theme(panel.background = element_rect(colour = "grey", fill = "white"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(linetype = "blank")) +
  facet_grid(Freq~., scales = "free_y", space="free_y") +
  theme_bw() +
  theme(strip.text = element_text(colour = "black"),
        strip.background = element_rect(colour="black", fill="gray80")) +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

# Save the plot manually as Station_time_1000x334px.png

ggsave("Plots/zoo_count.pdf", plot = last_plot(), scale = 1.5, width = NA, height = NA, dpi = 600, limitsize = TRUE)
