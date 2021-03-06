#### Load packages ####
source("Scripts/Organise_countdata.R")

#### Check available count data per taxa
dplyr::summarize(group_by(count, Year, Month))# Check whether every month is only campaign -> correct

# summarize data in taxa dataframe
taxa <- dplyr::summarize(group_by(count, Year, Month, Taxon),
                         Count = ifelse(sum(Count)==0,0,1))
taxa <- as.data.frame(taxa)
taxa$date <- paste(taxa$Year, taxa$Month, 1, sep = "-")
taxa$date <- parse_date_time(taxa$date, orders = "ymd")
taxa$Count <- as.factor(taxa$Count)

# add instrument types
countsum <- dplyr::summarise(group_by(countsum, Year, Month),
                 Instrument = names(which.max(table(Instrument))))
taxa <- join(taxa, countsum)


#### Organize taxa ####
levels(taxa$Taxon)
taxa$Taxon <- factor(taxa$Taxon, levels = rev(c(
  "Calanoida", #Copepoda
  "Harpacticoida",
  "Amphipoda", #Malacostraca, geen copepode of decapode
  "Cumacea",
  "Mysidacea",
  "Anomura", #Decapoda
  "Brachyura [zoe]",
  "Brachyura [mega]",
  "Caridea [zoe]",
  "Porcellana [zoe]",
  "Cirripedia [cyp]", # andere crustaceen
  "Cirripedia [nau]",
  "Noctiluca", # aparte groepen
  "Ctenophora",  
  "Chaetognatha",
  "Annelida",
  "Mollusca",
  "Echinodermata",
  "Appendicularia",
  "Pisces [larv]",
  "Pisces [egg]",
  "detritus", #overschot
  "artefacts",
  "fibres",
  "others"
)))

# remove non-informing categories (possible to mention in graph)
taxa <- dplyr::filter(taxa, Taxon != "detritus", Taxon != "artefacts", Taxon !="fibres", Taxon != "others")

#### Plot count data per taxa ####
ggplot(data = taxa, aes(x = date, y = Taxon)) +
  geom_point(aes(fill = Count, shape = Instrument), size = 1.5) +
  scale_shape_manual(values = c(21,23,22), limits = c("Plankton pump", "Apstein net", "WP2 net")) + 
  scale_fill_manual(values = c("gray90", "black"), limits = c(0,1)) +
  guides(fill = "none") +
  theme(panel.background = element_rect(colour = "grey", fill = "white"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(linetype = "blank")) +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

# Save plot manually as Taxa_time_1000x360px.png