#### Load packages ####
source("Scripts/Organise_countdata.R") # also sources Map_stations.R

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
