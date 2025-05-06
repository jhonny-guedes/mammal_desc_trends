# Global trends in mammal species descriptions over three decades 
# Scripts by Matheus de T. Moroti & Jhonny J. M. Guedes
# mmoroti@gmail.com / jhonnyguds@gmail.com

# Packages
# Load and install needed package
needed_packages <- c("tidyverse", # package version 2.0.0
                     "dplyr", # v. 1.1.4
                     "data.table", # v. 1.15.4
                     "ggplot2", # v. 3.5.1
                     "cowplot",
                     "MASS", # to fit the negative binomial models
                     "RColorBrewer",
                     "broom",
                     'sf',
                     'raster',
                     'GGally',
                     'cowplot',
                     'ggpubr',
                     'rstatix'
                     
)
new.packages<-needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(needed_packages, require, character.only = TRUE)

for (i in seq_along(needed_packages)) {
  print(packageVersion(needed_packages[i]))
}

# Clean global environment
rm(list=ls()); gc()

# Set working directory
setwd() # DEFINE YOUR WORKING DIRECTORY (THE FOLDER WITH FILES NEEDED TO REPLICATE THE FINDING OF THIS STUDY)

# 1) Load and understand the dataset ----

# # Correlation with TypeSeries and N. Specimens
#local_directory <-  file.path("C:", "Users", "mmoro",
#                              "OneDrive", "mammals_desc")
#typeseries <- readxl::read_excel(
#  file.path(local_directory,
#            "RawDataToCompile_Mammalia.xlsx"), sheet = "SpeciesName")

#data <- fread("Dataset.csv", na.strings = '') # 1032 species
load("Dataset.Rdata")
names(data)
dim(data)

# Pearson correlation between number of specimens and number of taxa compared
mydata_cor <- typeseries %>% 
  filter(!is.na(N.Specimens) & !is.na(N.TypeSeries))

cor.test(log(mydata_cor$N.TypeSeries),
         log(mydata_cor$N.Specimens), method = "pearson")

# We have 30 columns in this dataset, each one explained below:
# SpeciesName: Binomial name.
# Genus: Taxonomic genus to which a species belongs.
# Family: Txonomic family to which a species belongs.
# Order: Taxonomic order to which a species belong ("Crocodylia", "Sauria", "Serpentes", "Testudines").
# Authority: Author(s) involved in the species description.
# Year: Year in which the species was formally described.
# TaxonomicReview: Informs if the description was based (or not) on a taxonomic review (0 = No, 1 = Yes).
# N_authors: Informs the number of authors per description.
# N.Countries: Informs the number of countries (based on author's affiliations) involved in the description.
# Log10BodyMass_g: Maximum body mass per species (log10-transformed).
# SppRichPerGenus: The per-genus species richness based on the year of each species' description.
# Morphometrics: Informs the number of morphometric measurements used in the description.
# Osteology: Informs the number of osteological measurements used in the description.
# Dentition: Binary variable informing whether dentition data was provided in the description.
# InternalAnatomy: Binary variable informing whether data on internal anatomy was provided in the description.
# ShapeDescription: Binary variable informing whether any aspects of the species shape was described.
# Trichology: Binary variable informing whether trichology data was provided in the description.
# Coloration: Binary variable informing whether color data was provided in the description.
# Karyotype: Binary variable informing whether karyotype data was provided in the description.
# Molecular: Binary variable informing whether the authors used molecular data in the description.
# MolMethod: Informs the molecular method used in the description (i.e., mtDNA, nucDNA, multiLoci, SNPs)
# N.Genes: Informs the total number of genes sequenced when molecular data was used.
# N.Specimens: Informs the number of specimens of the new species used in the description.
# TaxaComparedExamined: Informs the number of taxa the authors analysed/inspected for comparisons with the new species.
# TaxaCompared: Informs the number of taxa mentioned in the text during comparisons with the new species.
# N.Pages: The number of pages (METHODS and RESULTS sections only) of the article divided by the number of described species. The page is divided into 4 quadrants, meaning 1 page is composed of 4x0.25 parts.
# N_evidencesI: The number of evidence types used in descriptions. Morphometrics, ostelogy and genes sequenced are treated as continuous characters, while the others are binary (this represent a unequal-weight metric).
# N_evidencesII: The number of evidence types used in descriptions. All variables treated as binary (0 or 1), thus having equal weight.


# Check the number of species per Order
data %>% 
  dplyr::group_by(Order) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n))
# Order                n
# Rodentia           421
# Chiroptera         280
# Eulipotyphla       120
# Primates           110
# Didelphimorphia     23
# Artiodactyla        18
# Diprotodontia       14
# Afrosoricida        10
# Dasyuromorphia       8
# Lagomorpha           8
# Peramelemorphia      5
# Pilosa               4
# Carnivora            3
# Macroscelidea        3
# Paucituberculata     2
# Hyracoidea           1
# Microbiotheria       1
# Monotremata          1

table(data$Order)
prop.table(table(data$Order)) * 100

# Check amounts of missing data among response variables and get other basic stats
names(data)

# Pearson correlation between number of specimens and number of taxa compared
mydata_cor <- data %>% 
  filter(!is.na(N.Specimens) & !is.na(TaxaComparedExamined))
cor.test(data$TaxaCompared, data$TaxaComparedExamined, method = "pearson")

# All mammals
summary(data[ , c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
                  "N.Pages", "N_evidencesI", "N_evidencesII")])

# Mammals without rodents and bats
summary(
  data[data$Order != "Chiroptera" & data$Order != "Rodentia",
       c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
         "N.Pages", "N_evidencesI", "N_evidencesII")]
)

# Only rodentia
summary(
  data[data$Order == "Rodentia" ,
       c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
         "N.Pages", "N_evidencesI", "N_evidencesII")]
)

# Only Chiroptera
summary(
  data[data$Order == "Chiroptera" ,
       c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
                    "N.Pages", "N_evidencesI", "N_evidencesII")]
)

rm(list=setdiff(ls(),c("data"))); gc() # clean workspace

# Make a backup
mydata <- data

# Check mean and variance across response variables
names(mydata)
mean(mydata$N_evidencesI, na.rm = T); var(mydata$N_evidencesI, na.rm = T) # 24; 112
mean(mydata$N_evidencesII, na.rm = T); var(mydata$N_evidencesII, na.rm = T) # 4.9; 1.2
mean(mydata$N.Pages, na.rm = T); var(mydata$N.Pages, na.rm = T) # 9.8; 162
mean(mydata$N.Specimens, na.rm = T); var(mydata$N.Specimens, na.rm = T) # 19; 1450
mean(mydata$TaxaCompared, na.rm = T); var(mydata$TaxaCompared, na.rm = T) # 5; 29
# The variance is lower than the mean only for the number of evidences II; much higher for the others.

# Check for skewed distributions and kurtosis among predictors (transform data if necessary).
names(mydata)
e1071::skewness(mydata$N_authors); e1071::kurtosis(mydata$N_authors) # 2.9 and 15.1
e1071::skewness(mydata$N.Countries, na.rm = T); e1071::kurtosis(mydata$N.Countries, na.rm = T) # 2.5 and 12.1
e1071::skewness(mydata$Year); e1071::kurtosis(mydata$Year) # -0.37 and -0.88
e1071::skewness(mydata$SppRichPerGenus, na.rm = T); e1071::kurtosis(mydata$SppRichPerGenus, na.rm = T) # 2.6 and 6.62
e1071::skewness(mydata$Log10BodyMass_g, na.rm = T); e1071::kurtosis(mydata$Log10BodyMass_g, na.rm = T) # 1.3 and 2.7
# Conclusion: log10 transform no. of authors, no. of countries, and species richness per genus

mydata$N_authors <- log10(mydata$N_authors)
mydata$N.Countries <- log10(mydata$N.Countries)
mydata$SppRichPerGenus <- log10(mydata$SppRichPerGenus + 1)
e1071::skewness(mydata$N_authors); e1071::kurtosis(mydata$N_authors) # much better [0.13 and -0.25]
e1071::skewness(mydata$N.Countries, na.rm = T); e1071::kurtosis(mydata$N.Countries, na.rm = T) # much better [0.5 and -0.47]
e1071::skewness(mydata$SppRichPerGenus, na.rm = T); e1071::kurtosis(mydata$SppRichPerGenus, na.rm = T) # much better [0.11 and -0.23]

### Before standardizing the data, make descriptive plots of the response and predictor variables.
names(mydata)

# Define variables
vars <- c("N_evidencesII", "N.Pages", "N.Specimens", "TaxaCompared", 
          "Year", "Log10BodyMass_g", "N_authors", "N.Countries", "SppRichPerGenus", "TaxonomicReview")
# remove "N_evidencesI"
# change taxonomic review to categorical
mydata$TaxonomicReview <- ifelse(mydata$TaxonomicReview==1, yes = 'Yes', no = 'No')

# Define the custom x-axis labels
custom_labels <- c("N. of evidence", "N. of pages", "N. of specimens",
                   "N. of taxa compared", "Year of description", "Body mass (log10)", 
                   "N. of authors (log10)", "N. of countries (log 10)",
                   "N. of species/genus (log10)", "Taxonomic review")
# remove "N_evidencesI"
# Initialize a list to store the plots
plot_list <- list()

# Loop through each variable and create a plot based on its type
for (i in seq_along(vars)) {
  var <- vars[i]
  label <- custom_labels[i]
  
  # Data subset with complete cases
  new_data <- mydata[complete.cases(Year, Log10BodyMass_g, N_authors,
                                    SppRichPerGenus, TaxonomicReview), ]
  
  if (is.numeric(mydata[[var]])) {  # Continuous variables
    p <- ggplot(new_data, aes_string(x = var)) +
      geom_histogram(color = "black", fill = 'grey50', alpha = 0.5, na.rm = TRUE) +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0))) +
      {if(i %in% c(1, 4, 7, 10)) labs(x = label, y = "N. of species")} +
      {if( ! (i %in% c(1, 4, 7, 10))) labs(x = label, y = NULL)} +
      theme_classic() +
      theme(axis.title = element_text(size = 7, face = 'bold'),
            axis.text = element_text(size = 6),
            legend.position = 'none')
    
  } else {  # Categorical variables
    p <- ggplot(new_data, aes_string(x = var)) +
      geom_bar(color = "black", alpha = 0.7, na.rm = TRUE) +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0))) +
      labs(x = label, y = NULL) +
      theme_classic() +
      theme(axis.title = element_text(size = 7, face = 'bold'),
            axis.text = element_text(size = 6),
            legend.position = 'none')
  }
  
  # Add the plot to the list
  plot_list[[var]] <- p
  rm(new_data, p)
}

# Combine the plots into a multi-panel plot using cowplot
multi_panel_plot <- plot_grid(plotlist = plot_list, ncol = 2, labels = 'auto', 
                              align = 'v', label_size = 8); multi_panel_plot

# Save the figure
ggsave(paste0(getwd(), "/figures/Figure1.DescriptivePlot.pdf"), 
       plot=multi_panel_plot, width=6, height=10, units="in", dpi = "print", cairo_pdf)
ggsave(paste0(getwd(), "/figures/Figure1.DescriptivePlot.jpg"),
       plot=multi_panel_plot, width=6, height=7, units="in", dpi = "print")
ggsave(paste0(getwd(), "/figures/Figure1.DescriptivePlot.tiff"),
       plot=multi_panel_plot, width=6, height=7, units="in", dpi = "print")
rm(multi_panel_plot, plot_list, label, vars, var, i, custom_labels)

# 2) Map mammal species described in the last three decades ----
# Load a world map with country subdivisions
#world_map <- st_read("~/Documents/Rasters and shapefiles/shapefiles/gadm36_cea.shp")
#world_map <- world_map %>% st_transform(crs = "+proj=eqearth") # change CRS to equal area
#plot(world_map$geometry)

# set local directory, change directory as needed
local_directory <- file.path("D:/", "repos", "mammal_desc_trends", "shapefiles") 

# Load shapefile of biogeographical realms
wwf_realms<-sf::read_sf(file.path(local_directory, "wwf_simplified", "wwf_simplified.shp"))  
wwf_realms <- wwf_realms %>% st_transform(crs = "+proj=eqearth") # change CRS to equal area
plot(wwf_realms$geometry)

# Load a shapefile depicting 'world limits'
world_limit <- sf::st_read(file.path(local_directory, "world_limit", "world_limit.shp"))
world_limit <- world_limit %>% st_transform(crs = "+proj=eqearth") # change CRS to equal area

# Load the dataset, then convert the geographical coordinates to an sf object
# load("Dataset.Rdata") # 1032 species
points_sf <- sf::st_as_sf(data [ !is.na(data$Latitude) & ! is.na(data$Longitude) , c('SpeciesName', 'Latitude', 'Longitude')],
                          coords = c("Longitude", "Latitude"), crs = st_crs("+proj=longlat +datum=WGS84"))

# Transform data to the same projection as the realms map
points_sf <- st_transform(points_sf, crs = crs(wwf_realms))
compareCRS(wwf_realms, points_sf) # true

# get aspect ratio of the spatial object for controlling white space when saving the plot
#plot_ratio <- tmaptools::get_asp_ratio(wwf_realms) # will mutiply width in ggsave

# Define colors for each biogeographic realm (Pastel1 from RColorBrewer)
levels(as.factor(wwf_realms$wwf_realm))
#MyBiogeoColors<-c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc")
MyBiogeoColors<-c("grey50", "grey40", "grey70", "grey80", "grey60", "grey90")
names(MyBiogeoColors)<-c("Afrotropic", "Australasia", "IndoMalay", "Neartic", "Neotropic", "Paleartic")

# Build the plot:
MyMap <- ggplot2::ggplot() +
  
  # Add polygon boundaries for the wwf realms:
  geom_sf(data = wwf_realms, aes(fill=wwf_realm), colour="black", size=0.1) +
  geom_sf(data=world_limit, fill=NA, colour="black", linewidth=0.3)+
  
  # Add type-localities of species described:
  geom_sf(data = points_sf, color = "white", size = 2, shape = 20) +  # Larger white point for outline
  geom_sf(data = points_sf, color = "black", size = 1.3, shape = 20, alpha = 0.5) +  # Plot points
  
  # Inform the filling colors for each biogeographical realm:
  scale_fill_manual(values=MyBiogeoColors) +
  
  # Specify other aesthetics:
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        plot.background=element_rect(fill="white"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),  # top, right, bottom, left 
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),  # top, right, bottom, left
        panel.border = element_blank(),
        legend.position = "none"); MyMap

# Compute the proportion of species described per order per realm 
# Perform a spatial join to get the biogeographic realm for each point
points_sf <- st_join(points_sf, wwf_realms["wwf_realm"])
# Add data to the main dataset
data <- left_join(data, st_drop_geometry(points_sf), by = 'SpeciesName')
colSums(is.na(data)) # ok

PropPerRealm <- data %>%
  dplyr::filter(!is.na(wwf_realm)) %>% # filter out rows without realm information
  dplyr::group_by(wwf_realm, Order) %>% # group by realm and order
  dplyr::summarise(SppRichness = n()) %>% # compute species richness per realm and order
  dplyr::group_by(wwf_realm) %>% # compute the total species richness per realm
  dplyr::mutate(TotalSpp = sum(SppRichness),
                Prop = SppRichness / TotalSpp)

# Same as above, but at the global level:
PropTotal <- data %>%
  dplyr::filter(!is.na(wwf_realm)) %>%
  dplyr::group_by(Order) %>%
  dplyr::summarise(wwf_realm = "Global",
                   SppRichness = n()) %>%
  dplyr::mutate(TotalSpp = sum(SppRichness),
                Prop = SppRichness / TotalSpp)

# Bind the datasets in a single one:
PropPerRealm <- rbind(PropPerRealm, PropTotal); rm(PropTotal)
#PropPerRealm <- PropPerRealm %>%
# group_by(wwf_realm) %>%
# group orders as 'new taxa' if prop < 0.05
#mutate(NewOrder = ifelse(Prop <= 0.05, yes = 'Other taxa', no = Order))

# Apply the global classification to create NewOrder
GlobalData <- PropPerRealm %>%
  filter(wwf_realm == "Global") %>%
  mutate(NewOrder = ifelse(Prop <= 0.05, "Other taxa", Order))

# Create a mapping from Order to NewOrder based on GlobalData
classification <- GlobalData[ , c("Order", "NewOrder")]

# Join this classification back to the main dataset and apply it to all realms
PropPerRealm <- PropPerRealm %>% left_join(classification, by = "Order")
# PropPerRealm has the same classification across all realms.
# Use this `NewOrder` for consistent coloring in the plots.

# Summarize data to get only one column for 'new taxa' per realm
PropPerRealm <- PropPerRealm %>%
  group_by(wwf_realm, NewOrder) %>%
  summarise(SppRichness = sum(SppRichness),
            TotalSpp = median(TotalSpp),
            Prop = sum(Prop)) %>%
  arrange(desc(Prop)) 

# Define colors for each realm
levels(as.factor(PropPerRealm$NewOrder))
MyColors <- c(
  "Chiroptera" = "#7fc97f",
  "Eulipotyphla" = "#beaed4",
  "Other taxa" = "#fdc086",
  "Primates" = "#ffff99",
  "Rodentia" = "#386cb0"
)
names(MyColors)<-c("Chiroptera", "Eulipotyphla", "Other taxa", "Primates", "Rodentia")

levels(as.factor(PropPerRealm$wwf_realm))
PropPerRealm$wwf_realm <- factor(PropPerRealm$wwf_realm,
                                 labels = c("Afrotropic", "Australasia", "Global", "IndoMalay",
                                            "Nearctic", "Neotropic", "Palearctic"))

# Remake MyMap, colouring species points based on the colours of their respective Orders
points_sf <- points_sf %>% left_join(data[ , c('SpeciesName', 'Order')]) # extract Order from 'data' 
points_sf <- points_sf %>% left_join(classification, by = "Order")

MyMap2 <- ggplot2::ggplot() +
  
  # Add polygon boundaries for the wwf realms:
  geom_sf(data = wwf_realms, aes(fill=wwf_realm), colour="black", size=0.1) +
  geom_sf(data=world_limit, fill=NA, colour="black", linewidth=0.3)+
  
  # Add type-localities of species described:
  geom_sf(data = points_sf, color = "white", size = 2, shape = 20) +  # Larger white point for outline
  geom_sf(data = points_sf, aes(fill = NewOrder), color = "black", size = 1.3, shape = 21, alpha = 0.7) +  # Inner color based on NewOrder
  
  # Inform the filling colors:
  scale_fill_manual(values = c(MyBiogeoColors, MyColors)) +
  
  # Specify other aesthetics:
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(), 
        plot.background=element_rect(fill="white"),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),  # top, right, bottom, left 
        panel.spacing = unit(c(0, 0, 0, 0), "cm"),  # top, right, bottom, left
        panel.border = element_blank(),
        legend.position = "none"); MyMap2

# Create one donut plot per biogeographical realm:
MyPlot <- list()

for(i in 1:nlevels(as.factor(PropPerRealm$wwf_realm))) {
  
  # Filter the dataset to include one biogeographical realm:
  FilteredData <- PropPerRealm[PropPerRealm$wwf_realm == levels(as.factor(PropPerRealm$wwf_realm))[i],]
  #FilteredData <- FilteredData %>% mutate(Order = fct_reorder(Order, Prop, .desc = TRUE))
  FilteredData <- FilteredData %>% mutate(NewOrder = fct_reorder(NewOrder, Prop, .desc = TRUE))
  
  # Get the color for the current realm
  current_realm <- levels(as.factor(PropPerRealm$wwf_realm))[i]
  #current_color <- MyColors[current_realm]
  #outer_color <- MyBiogeoColors[current_realm]
  
  # Store each plot as a list element:
  MyPlot[[i]] <- ggplot(FilteredData, aes(x = 2, y = Prop, fill = NewOrder)) +
    
    # Use geom_bar for donut segments
    #geom_bar(stat = "identity", width = 1, color = "black", fill = current_color) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    
    # Add the outer ring with the color of the biogeographic realm
    #{
    #  if(i !=3)
    #    geom_bar(aes(x = 2.5), stat = "identity", width = 0.1, fill = outer_color, color = 'black')
    #} +
    
    #scale_fill_brewer(type = 'qual', palette = 'Set3') +
    scale_fill_manual(values = MyColors) +
    
    # Text labels based on condition
    {
      if (i == 3) { # Global level; add Order names
        geom_text(aes(x = 2, label = paste0(scales::percent(Prop, accuracy = 1), "\n", NewOrder)), 
                  position = position_stack(vjust = 0.5), size = 2.8, fontface = "bold")
      } else { # Other realms; only display percentage
        geom_text(aes(x = 2, label = scales::percent(Prop, accuracy = 1)), 
                  position = position_stack(vjust = 0.5), size = 2.8, fontface = "bold")
      }
    } +
    
    #geom_segment(
    #  aes(y = cumsum(Prop) - Prop / 2, yend = cumsum(Prop) - Prop / 2, x = 2, xend = 2.75),
    #  size = 0.3,
    #  color = "black"
    #) +
    
    # Add text labels for proportions inside segments
    #geom_text(aes(x = 2.95, label = paste0(scales::percent(Prop), "\n", NewOrder)), 
    #          position = position_stack(vjust = 0.5), size = 2)+
    
  # Add TotalSpp value in the center of the donut
  annotate("text", x = 0.5, y = 0, fontface = 'bold', size = 2.5, hjust = 0.5,
           label = paste("N =", FilteredData$TotalSpp[1], "\n", current_realm))+
    
    # Use coord_polar to make it circular, with a hole in the center
    coord_polar(theta = "y") +
    
    # Create a hole in the center (adjust xlim for size of hole)
    xlim(0.5, 2.7) +
    
    # Define axis and theme aesthetics
    labs(x = "", y = "") +
    theme_void() +  # simplify the plot for a clean look
    
    theme(
      legend.position = "none",
      plot.margin = unit(c(-0.5, -1, -0.5, -1), "cm"),
      panel.background = element_blank(),
      plot.background = element_rect(fill='transparent', color=NA) #transparent plot bg
    )
}

# Print each plot if desired
print(MyPlot[[3]])

# Set the biogeographical realm illustrated in each plot:
names(MyPlot) <- levels(as.factor(PropPerRealm$wwf_realm))

library(grid)

# Define a function to place a plot as a donut at a specific location
add_donut <- function(donut_plot, x_pos, y_pos, donut_size) {
  annotation_custom(
    grob = ggplotGrob(donut_plot),
    xmin = x_pos - donut_size,
    xmax = x_pos + donut_size,
    ymin = y_pos - donut_size,
    ymax = y_pos + donut_size
  )
}

# Plot map and add donuts
FinalPlot <- MyMap2 +
  add_donut(MyPlot[["Nearctic"]], x_pos = -13067530, y_pos = -7342217 + 11200000, donut_size = 2400000) +
  add_donut(MyPlot[["Neotropic"]], -8500530, -7342217 + 2400000, 2300000) +
  add_donut(MyPlot[["Palearctic"]], -3500000, -7342217 + 11200000, 2400000) +
  add_donut(MyPlot[["Afrotropic"]], -1000000, -7342217 + 4200000, 2400000) +
  add_donut(MyPlot[["IndoMalay"]], 14000530, -7342217 + 10000000, 2400000) +
  add_donut(MyPlot[["Australasia"]], 8000000, -7342217 + 4200000, 2400000) +
  add_donut(MyPlot[["Global"]], -13500530, -7342217 + 5400000, 4000000); FinalPlot

ggsave(filename="figures/Figure1_Map.png", plot=FinalPlot, width=12, height=8, units="in", bg="white", limitsize=F)
ggsave(filename="figures/Figure1_Map.pdf", plot=FinalPlot, width=12, height=8, units="in", bg="white", limitsize=F)

# 3) Temporal trends in robustness of publications - based on annual means ----
# Four metrics will be analyzed:
# i)   number of evidence types (there are 2 versions of this variable: equal- and unequal-weight; see main text)
# ii)  number of pages per publication (includes only methods and results).
# iii) number of specimens examined,
# iv)  number of taxa the new species was compared to.

# Create a backup
mydata <- data
names(mydata)

#------------------------------------------------------------#
# Check correlation between response variables
#------------------------------------------------------------#

# Select predictor variables to check for correlation
cor(mydata[ , c("N_evidencesII", "N.Pages", "N.Specimens", "TaxaCompared")], 
    method = "spearman", use = "complete.obs")
# N_evidencesI N_evidencesII    N.Pages N.Specimens TaxaCompared N.Countries
#N_evidencesI    1.00000000    0.34952539 0.23596197  0.11357590   0.07747519  0.01208026
#N_evidencesII   0.34952539    1.00000000 0.38465652  0.11492334   0.05396673  0.11547902
#N.Pages         0.23596197    0.38465652 1.00000000  0.03614598   0.04296941  0.05002470
#N.Specimens     0.11357590    0.11492334 0.03614598  1.00000000   0.07105905  0.04690342
#TaxaCompared    0.07747519    0.05396673 0.04296941  0.07105905   1.00000000  0.08262026
#N.Countries     0.01208026    0.11547902 0.05002470  0.04690342   0.08262026  1.00000000
# low correlation among response variables (all below 0.45)

# Define custom labels
custom_labels <- c("N_evidencesII" = "N. of evidence",
                   "N.Pages" = "N. of pages",
                   "N.Specimens" = "N. of specimens",
                   "TaxaCompared" = "N. taxa compared")

# Create the ggpairs plot with custom labels
p <- ggpairs(
  mydata, 
  columns = c(28,26,23,25), 
  upper = list(continuous = wrap("cor", method = "spearman")),
  lower = list(continuous = wrap("points", alpha = 0.5)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
  labeller = as_labeller(custom_labels) # apply custom labels
) + 
  theme(
    axis.text = element_text(size = 8),        
    strip.text = element_text(size = 7, face = 'bold')
  ); p

# Save the image
dir.create('figures') # create folder to store images
ggsave(paste0(getwd(), "/figures/FigureS1.ResponseCorr.pdf"), plot=p, width=7, height=5, units="in", dpi = "print")
ggsave(paste0(getwd(), "/figures/FigureS1.ResponseCorr.png"), plot=p, width=7, height=5, units="in", dpi = "print", bg = 'white')
rm(p) # clean workspace

#------------------------------------------------------------#
# Make correlation plots between response variables and year
#------------------------------------------------------------#

# Select response, explanatory (year), and grouping variables
names(mydata)
new_dat <- mydata[ , c("SpeciesName", "Order", "Year","N_evidencesI", "N_evidencesII",
                       "N.Pages", "N.Specimens", "TaxaCompared", "N.Countries")]

# Get summary values for plotting
create_data <- function(data) {
  yearly_means <- data %>% 
    group_by(Year) %>% 
    summarise(N_evidencesI_avg = mean(N_evidencesI, na.rm = T),
              N_evidencesI_sd = sd(N_evidencesI, na.rm = T),
              N_evidencesI_nspp = sum( ! is.na(N_evidencesI)),
              
              N_evidencesII_avg = mean(N_evidencesII, na.rm = T),
              N_evidencesII_sd = sd(N_evidencesII, na.rm = T),
              N_evidencesII_nspp = sum( ! is.na(N_evidencesII)),
              
              N_Pages_avg = mean(N.Pages, na.rm = T),
              N_Pages_sd = sd(N.Pages, na.rm = T),
              N_Pages_nspp = sum( ! is.na(N.Pages)),
              
              N_specimens_avg = mean(N.Specimens, na.rm = T),
              N_specimens_sd = sd(N.Specimens, na.rm = T),
              N_specimens_nspp = sum( ! is.na(N.Specimens)),
              
              N_taxacomp_avg = mean(TaxaCompared, na.rm = T),
              N_taxacomp_sd = sd(TaxaCompared, na.rm = T),
              N_taxacomp_nspp = sum( ! is.na(TaxaCompared)),
              
              N_countries_avg = mean(N.Countries, na.rm = T),
              N_countries_sd = sd(N.Countries, na.rm = T),
              N_countries_nspp = sum( ! is.na(N.Countries)) ) %>%
    
    mutate(N_evidencesI_se = N_evidencesI_sd / sqrt(N_evidencesI_nspp),
           N_evidencesII_se = N_evidencesII_sd / sqrt(N_evidencesII_nspp),
           N_Pages_se = N_Pages_sd / sqrt(N_Pages_nspp),
           N_specimens_se = N_specimens_sd / sqrt(N_specimens_nspp),
           N_taxacomp_se = N_taxacomp_sd / sqrt(N_taxacomp_nspp),
           N_countries_se = N_countries_sd / sqrt (N_countries_nspp))
  return(yearly_means)
}

all_yearly_means <- create_data(new_dat) %>%
  mutate(Order = "All mammals")

rodentia_yearly_means <- new_dat %>%
  filter(Order == "Rodentia") %>%
  create_data() %>%
  mutate(Order = "Rodents") 

chiroptera_yearly_means <- new_dat %>%
  filter(Order == "Chiroptera") %>%
  create_data() %>%
  mutate(Order = "Bats")

allwithout_yearly_means <- new_dat %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  create_data() %>%
  mutate(Order = "Non-rodents & non-bats")

##  Check the % increase/decrease in robustness metrics between the 
# first 5 years of the series (1990-94) and last 5-years (2018-22);
# this may avoid the impact of outliers if using a single year.

# Number of evidence (non-equal weighted version as there are more variation in the data)
taxa <- all_yearly_means #para nao precisar repetir o codigo abaixo,
# so mudar o obj passado para 'taxa'. 

df90to94 <- apply(taxa[taxa$Year %in% 1990:1994, 'N_evidencesI_avg'], 2, mean)
df18to22 <- apply(taxa[taxa$Year %in% 2018:2022, 'N_evidencesI_avg'], 2, mean)
(df18to22 - df90to94) / df90to94 * 100 # from 22.3 to 26.4 (increased in 18.1%)

# Number of pages
df90to94 <- apply(taxa[taxa$Year %in% 1990:1994, 'N_Pages_avg'], 2, mean)
df18to22 <- apply(taxa[taxa$Year %in% 2018:2022, 'N_Pages_avg'], 2, mean)
(df18to22 - df90to94) / df90to94 * 100 # from 11.5 to 9.27 (decreased in 19.2%)

# Number of specimens
df90to94 <- apply(taxa[taxa$Year %in% 1990:1994, 'N_specimens_avg'], 2, mean)
df18to22 <- apply(taxa[taxa$Year %in% 2018:2022, 'N_specimens_avg'], 2, mean)
(df18to22 - df90to94) / df90to94 * 100 # from 13.1 to 22.3 (increased in 69.9%)

# Number of taxa compared
df90to94 <- apply(taxa[taxa$Year %in% 1990:1994, 'N_taxacomp_avg'], 2, mean)
df18to22 <- apply(taxa[taxa$Year %in% 2018:2022, 'N_taxacomp_avg'], 2, mean)
(df18to22 - df90to94) / df90to94 * 100 # from 4.13 to 6.27 (increased in 48.5%)

# Number of countries involved
df90to94 <- apply(taxa[taxa$Year %in% 1990:1994, 'N_countries_avg'], 2, mean)
df18to22 <- apply(taxa[taxa$Year %in% 2018:2022, 'N_countries_avg'], 2, mean)
(df18to22 - df90to94) / df90to94 * 100 # from 4.13 to 6.27 (increased in 84.8%)

# Join correlations between Orders for plot
yearly_means <- bind_rows(
  all_yearly_means,
  rodentia_yearly_means,
  chiroptera_yearly_means,
  allwithout_yearly_means
) %>%
  mutate(Order = factor(Order, levels = c(
    "All mammals",
    "Non-rodents & non-bats",
    "Bats",
    "Rodents"
  ))) 

# Function to create the plot
breaks = seq(from = 1990, to = 2022, by = 4)
create_plot <- function(data, y_label, mean, se, total_tests, nrow = nrow,
                        show_titles = TRUE, show_x_labels = TRUE) {
  
  # Definir cores para cada grupo
  order_colors <- c("Rodents" = "#386cb0",
                    "Bats" = "#7fc97f",
                    "Non-rodents & non-bats" = "#ff3352",
                    "All taxa" = "black")
  
  # Calcular correlações
  cor_results <- data %>%
    group_by(Order) %>%
    summarise(
      rho = cor.test(Year, !!enquo(mean), method = "spearman")$estimate,
      p_value = cor.test(Year, !!enquo(mean), method = "spearman")$p.value
    ) %>%
    mutate(
      bonferroni_p = pmin(p_value * total_tests, 1),
      bonferroni_p_label = ifelse(bonferroni_p < 0.001, "<0.001", format(round(bonferroni_p, 3), nsmall = 3))
    )
  
  # Preparar breaks e labels do eixo X
  years <- sort(unique(data$Year))  # Anos únicos e ordenados
  n_years <- length(years)
  
  # Definir 4 breaks (usando anos existentes)
  if (n_years > 4) {
    idx <- round(seq(1, n_years, length.out = 4))  # Índices para 4 anos equidistantes
    x_breaks <- years[idx]
  } else {
    x_breaks <- years  # Menos de 4 anos: usar todos
  }
  
  # Definir labels (se show_x_labels = TRUE)
  x_labels <- if (show_x_labels) {
    as.character(x_breaks)
  } else {
    rep("", length(x_breaks))  # Labels vazios
  }
  
  # Criar o gráfico
  p <- ggplot(data, aes(x = Year, y = !!enquo(mean), color = Order)) + 
    geom_pointrange(aes(
      ymin = !!enquo(mean) - !!enquo(se), 
      ymax = !!enquo(mean) + !!enquo(se)),
      size = 0.3, alpha = 0.9
    ) +
    geom_smooth(method = "lm", fullrange = FALSE, aes(fill = Order), alpha = 0.2) +
    xlab(NULL) + 
    ylab(y_label) +
    scale_x_continuous(
      breaks = x_breaks,  # 4 ticks baseados nos anos
      labels = x_labels,  # Labels controlados por show_x_labels
      expand = expansion(mult = 0.02)  # Reduz espaço nas bordas
    ) +
    scale_color_manual(values = order_colors) +
    scale_fill_manual(values = order_colors) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title = element_text(size = 10, face = "bold"),
      axis.line = element_line(colour = "black"),
      axis.ticks.x = element_line(),  # Garante ticks visíveis
      axis.text = element_text(size = 8, colour = "black"),
      axis.text.x = element_text(
        angle = 0, 
        hjust = 0.6, 
        vjust = 0  # Ajuste fino para alinhamento
      ),
      legend.position = "none",
      strip.text = element_text(size = ifelse(show_titles, 10, 0))
    ) +
    facet_wrap(~Order, scale = "free_y", nrow = nrow) +
    geom_text(
      data = cor_results,
      aes(
        x = min(data$Year, na.rm = TRUE), 
        y = Inf, 
        label = paste("rs =", round(rho, 3), "\nBonferroni p =", bonferroni_p_label)
      ),
      hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
    )
  
  return(p)
}

# Create plots for each variable and taxonomic order
figB <- create_plot(yearly_means, "N. of evidence", #  II
                    mean = N_evidencesII_avg, 
                    se = N_evidencesII_se, 5, nrow = 1,
                    show_titles = TRUE, show_x_labels = FALSE); figB
figC <- create_plot(yearly_means, "N. of pages",
                    mean = N_Pages_avg,
                    se = N_Pages_se, 5, nrow = 1,
                    show_titles = FALSE, show_x_labels = FALSE); figC
figD <- create_plot(yearly_means, "N. of specimens",
                    mean = N_specimens_avg, 
                    se = N_specimens_se, 5, nrow = 1,
                    show_titles = FALSE, show_x_labels = FALSE); figD
figE <- create_plot(yearly_means, "N. of taxa compared",
                    mean = N_taxacomp_avg,
                    se = N_taxacomp_se, 5, nrow = 1,
                    show_titles = FALSE, show_x_labels = TRUE); figE
figE <- figE + xlab("Year of description")

#figA <- create_plot(yearly_means, "N. of evidence I", 
#                    mean = N_evidencesI_avg,
#                    se = N_evidencesI_se, 5,
#                    show_titles = TRUE, show_x_labels = FALSE); figA
# Add x-axis title

# Arrange plots in a grid
#fig <- ggpubr::ggarrange(figB, figC, figD, figE,
#                         ncol = 1, nrow = 4, 
#                         labels = "auto",
#                         font.label = list(size = 12, color = "black"),
#                         align = "hv"); fig
fig <- cowplot::plot_grid(figB, figC, figD, figE,
                          ncol = 1, nrow = 4, labels = "auto"); fig
# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure2.TemporalTrends.pdf"),
       plot=fig, width=12, height=10, units="in", dpi = "print", cairo_pdf)

# Join figure with section
figF <- create_plot(yearly_means, "N. of countries involved",
                    mean = N_countries_avg, 
                    se = N_countries_se, 5, nrow = 4,
                    show_titles = FALSE, show_x_labels = TRUE); figF

figF <- figF + xlab("Year of description"); FigF

ggsave(paste0(getwd(), "/figures/FigureAux.Nofcountries.pdf"),
 plot=figF, width=4, height=10, units="in", dpi = "print", cairo_pdf)
# This figure was exported to the software InkScape for minor aesthetic adjustments

# 4) Publication Robustness by Mammal Order ----
# Get summary values for plotting
# Select response, explanatory (year), and grouping variables
names(mydata)
new_dat <- mydata[ , c("SpeciesName","Order","N_evidencesI", "N_evidencesII", 
                       "N.Pages", "N.Specimens", "TaxaCompared", "N.Countries")]

# Supondo que seu dataframe se chame df
df_long <- new_dat %>%
  pivot_longer(cols = c("N_evidencesI", "N_evidencesII", "N.Pages", 
                        "N.Specimens", "TaxaCompared", "N.Countries"),
               names_to = "Variable",
               values_to = "Value") %>%
  group_by(Variable, Order) %>%
  filter(!all(is.na(Value))) %>%
  mutate(mediana = median(Value, na.rm = TRUE)) %>%
  ungroup()

plot_boxplot <- function(df_long, variable, title) {
  plot <- df_long %>%
    filter(Variable == variable) %>%
    ggplot(aes(x = reorder(Order, -mediana), y = Value, fill = Order)) + 
    #geom_violin(width = 1.4, , alpha=0.2) +
    geom_boxplot(color="black", alpha=0.2) +
    #geom_jitter(color="gray", size=0.4, alpha=0.9) +
    coord_flip() +
    theme_ipsum() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 11),
      axis.text.x = element_text(hjust = 1),
      panel.grid = element_blank(),  
      panel.background = element_blank(),  
      axis.ticks = element_blank()  
    ) + 
    ggtitle(title) +
    xlab("") + 
    ylab("")
}

figA <- plot_boxplot(df_long, "N_evidencesI", "N. of evidence I"); figA
figB <- plot_boxplot(df_long, "N_evidencesII", "N. of evidence II"); figB
figC <- plot_boxplot(df_long, "N.Pages", "N. of pages"); figC
figD <- plot_boxplot(df_long, "N.Specimens", "N. of specimens"); figD
figE <- plot_boxplot(df_long, "TaxaCompared", "N. of taxa compared"); figE
figF <- plot_boxplot(df_long, "N.Countries", "N. of countries involved"); figF

# Arrange plots in a grid
fig <- ggpubr::ggarrange(figA, figB, figC, figD, figE, figF,
                         ncol = 2, nrow = 3, labels = "auto", 
                         font.label = list(size = 12,color = "black"),
                         align = "hv"); fig

# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure2.OrderRobustness.pdf"),
       plot=fig, width=14, height=18, units="in", dpi = "print", cairo_pdf)

# 5) Temporal trends in robustness of publications - based on GLMs.----
rm(list=setdiff(ls(),c("data"))); gc() # clean workspace

# Make a backup
mydata <- data

# Standardize continuous predictors (mean = 0, sd =1) in order to make them comparable
mydata$year.z <- scale(mydata$Year) 
mydata$logBodyMass.z <- scale(mydata$Log10BodyMass_g)
mydata$logN_authors.z <- scale(mydata$N_authors) 
mydata$logN_countries.z <- scale(mydata$N.Countries) 
mydata$logGenusRichness.z <- scale(mydata$SppRichPerGenus) 

# Remove species with missing values on predictor variables
mydata <- mydata[ complete.cases(year.z, logBodyMass.z, logN_authors.z, logN_countries.z,
                                 logGenusRichness.z, TaxonomicReview) , ] 
# n = 820 species with complete data on predictor variables

# Change taxonomic review to categorical
mydata$TaxonomicReview <- ifelse(mydata$TaxonomicReview==1, yes = 'Yes', no = 'No')

# Check multicolinearity among continuous response variables
usdm::vif(mydata[ , year.z:logGenusRichness.z])
#         Variables      VIF
#             year.z 1.384506
#      logBodyMass.z 1.112525
#     logN_authors.z 1.916089
#   logN_countries.z 1.710078
# logGenusRichness.z 1.059974
# Conclusion: keep all variables into the model as VIFs are low (< 2)

# Check sample size per realm and order
#mydata %>% group_by(wwf_realm) %>% summarise(n = n())
# wwf_realm       n
# Afrotropic    198
# Australasia    88
# IndoMalay     124
# Neartic         7
# Neotropic     313
# Paleartic      55
# NA             35

#mydata %>% group_by(NewOrder) %>% summarise(n = n()) %>% arrange(desc(n))
# NewOrder         n
# Rodentia       330
# Chiroptera     222
# Eulipotyphla    98
# Primates        95
# Other taxa      75

# Sample size per response variable
colSums( ! is.na(mydata[ , c("N_evidencesI", "N_evidencesII", "N.Pages", "N.Specimens", "TaxaCompared")]))
# N_evidencesI & N_evidencesII = 820 species  
# N.Pages = 807 species
# N.Specimens = 805 species
# TaxaCompared = 810 species

# Create an empty data frame to store model results
results <- data.frame()

# Function to extract model results and add to the results data frame
extract_model_results <- function(model, response_name) {
  tidy_model <- broom::tidy(model)
  tidy_model <- tidy_model %>%
    mutate(response = response_name) %>%
    dplyr::select(response, term, estimate, std.error, p.value) %>%
    dplyr::mutate(lower95 = estimate - 1.96 * std.error, # compute lower 95% CI
                  upper95 = estimate + 1.96 * std.error) # compute upper 95% CI
  return(tidy_model)
}

# As bats and rodents represent the most diverse orders, let's run GLMs separetely for them.
data %>% group_by(Order) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Make a backup
levels(as.factor(data$Order))

## All mammals ----

#------------------------------------------------------------#
# Model the number of evidence I
#------------------------------------------------------------#

## Set model formula
#form <- as.formula(N_evidencesI ~ 
#                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
#                     logGenusRichness.z + TaxonomicReview)
#
## Fit a GLM
#mod.evi.nb <- glm.nb(formula = form, data = mydata[ ! is.na(mydata$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#mod.evi.gau <- glm(formula = form, data = mydata[ ! is.na(mydata$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#
## Compare models using AIC
#AIC(mod.evi.nb, mod.evi.gau)
##             df      AIC
## mod.evi.nb   8 6107.960
## mod.evi.gau  8 6132.008
## Models have similar fit, but the negative binomial model is slightly better
#
## Check model output
#summary(mod.evi.nb)  
## Results
##                     Estimate Std. Error z value Pr(>|t|)    
## (Intercept)        3.193723   0.016844 189.611  < 2e-16 ***
##year.z              0.040294   0.017748   2.270  0.02319 *  
##logN_authors.z     -0.010874   0.020640  -0.527  0.59831    
##logN_countries.z   -0.016223   0.019524  -0.831  0.40600    
##logBodyMass.z      -0.043015   0.016171  -2.660  0.00781 ** 
##GenusRichness.z     0.009696   0.015504   0.625  0.53171    
##TaxonomicReviewYes -0.090686   0.037806  -2.399  0.01645 *
#
## NOTE: results from the neg. binomial model is qualitatively the same as the gaussian model.
#
## Compute R2
#performance::r2(mod.evi.nb) # Nagelkerke's R2: 0.045
#
## Extract and store model results
#results <- bind_rows(results, extract_model_results(mod.evi.nb, "N. evidence I"))
#
## Save model output for latter checking phylogenetic correlation in model residuals
#save(mod.evi.nb, file = 'mod.evi.I.Rdata')

#------------------------------------------------------------#
# Model the number of evidence II
#------------------------------------------------------------#

# Set model formula
form <- as.formula(N_evidencesII ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit a GLM 
mod.evi2.nb <- glm.nb(formula = form, data = mydata[ ! is.na(mydata$N_evidencesII) , ] ) # remove rows with NAs on the response variable
mod.evi2.gau <- glm(formula = form, data = mydata[ ! is.na(mydata$N_evidencesII) , ] ) # remove rows with NAs on the response variable

# Compare models using AIC
AIC(mod.evi2.nb, mod.evi2.gau)
# df      AIC
# mod.evi2.nb   8 3033.138
# mod.evi2.gau  8 2415.438
# The gaussian model is much better

# Check model output
summary(mod.evi2.gau)  
# Results
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         4.968151   0.041203 120.578  < 2e-16 ***
# year.z              0.202483   0.043292   4.677 3.41e-06 ***
# logN_authors.z     -0.024241   0.050312  -0.482  0.63007    
# logN_countries.z    0.003436   0.047690   0.072  0.94258    
# logBodyMass.z      -0.242285   0.039175  -6.185 9.85e-10 ***
# GenusRichness.z    -0.057458   0.037908  -1.516  0.12998    
# TaxonomicReviewYes -0.254164   0.091606  -2.775  0.00566 **

# Compute R2
evidences_r2 <-performance::r2(mod.evi2.gau) # R2: 0.105

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.evi2.gau, "N. evidence"))

# Save model output for latter checking phylogenetic correlation in model residuals
save(mod.evi2.gau, file = 'model_outputs/mod.evi.II.Rdata')

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Set a full model formula
mydata$LogN.Pages <- log10(mydata$N.Pages) # transform it 'out' of the model, otherwise there will be an error when calculating R2
form <- as.formula(LogN.Pages ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages <- glm(formula = form, family = 'gaussian', data = mydata[ !is.na(mydata$N.Pages) , ])

# Check results
summary(mod.pages) 

# Results:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.850279   0.013172  64.550  < 2e-16 ***
# year.z              0.012389   0.013871   0.893   0.3720    
# logN_authors.z      0.029726   0.016033   1.854   0.0641 .  
# logN_countries.z    0.001682   0.015199   0.111   0.9119    
# logBodyMass.z      -0.060256   0.012530  -4.809 1.81e-06 ***
# GenusRichness.z    -0.064925   0.012109  -5.361 1.08e-07 ***
# TaxonomicReviewYes -0.043812   0.029228  -1.499   0.1343

# Compute R2
pages_r2 <- performance::r2(mod.pages) # R2: 0.067

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.pages, "N. pages"))

# save model output
save(mod.pages, file = 'model_outputs/mod.pages.Rdata')

#------------------------------------------------------------#
# Number of specimens 
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(N.Specimens ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit the model 
mod.ts <- glm.nb(formula = form, data = mydata[ !is.na(mydata$N.Specimens) , ])

# Check results
summary(mod.ts) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.830773   0.046271  61.179  < 2e-16 ***
# year.z              0.214269   0.048229   4.443 8.88e-06 ***
# logN_authors.z      0.005795   0.056209   0.103   0.9179    
# logN_countries.z   -0.121326   0.053291  -2.277   0.0228 *  
# logBodyMass.z      -0.002750   0.044548  -0.062   0.9508    
# GenusRichness.z     0.211751   0.042540   4.978 6.43e-07 ***
# TaxonomicReviewYes -0.004824   0.102291  -0.047   0.9624 

# Get R2
nspecimens_r2 <- performance::r2(mod.ts) # Nagelkerke's R2: 0.104

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.ts, "N. specimens"))

# Save model output
save(mod.ts, file = 'model_outputs/mod.ts.Rdata')

#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(TaxaCompared ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit the model 
mod.tcom <- glm.nb(formula = form, data = mydata[ !is.na(mydata$TaxaCompared) , ])

# Check results
summary(mod.tcom) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.72249    0.03188  54.034  < 2e-16 ***
# year.z              0.11168    0.03351   3.333  0.00086 ***
# logN_authors.z     -0.06351    0.03890  -1.633  0.10253    
# logN_countries.z    0.06326    0.03677   1.720  0.08540 .  
# logBodyMass.z       0.01813    0.03049   0.595  0.55209    
# GenusRichness.z     0.22290    0.02942   7.575 3.58e-14 ***
# TaxonomicReviewYes -0.06620    0.07177  -0.922  0.35633

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom) # Nagelkerke's R2: 0.163

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.tcom, "N. taxa compared"))

# Save model output
save(mod.tcom, file = 'model_outputs/mod.tcom.Rdata')

# Round numbers 
results[,c(3:5)] <- round(results[,c(3:5)], digits = 3)
View(results)
# Save as xlsx
dir.create('tables') # create folder to store model results
writexl::write_xlsx(results, 'tables/model_outputs.xlsx')
fwrite(results, file = 'model_outputs/model_outs.csv')

# Extract R2
results_r2 <- tibble(
  name = c("N. evidence", "N. pages", "N. specimens", "N. taxa compared"),
  value = c(evidences_r2$R2, pages_r2$R2, nspecimens_r2$R2_Nagelkerke, taxacompared_r2$R2_Nagelkerke),
  metric = c("R2", "R2", "R2_Nagelkerke", "R2_Nagelkerke"),
  group = "All mammals"
)
fwrite(results_r2, file = 'model_outputs/r2_allmammals.csv')

# Clean workspace
rm(list=setdiff(ls(),c("data","mydata","extract_model_results"))); gc()

## Non-bats & non-rodents ----
mammals_without <- mydata %>%
  filter(Order != "Rodentia" & Order != "Chiroptera")
results <- data.frame()
#------------------------------------------------------------#
# Model the number of evidence I
#------------------------------------------------------------#
#
## Set model formula
#form <- as.formula(N_evidencesI ~ 
#                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
#                     logGenusRichness.z + TaxonomicReview)
#
## Fit a GLM
#mod.evi.nb <- glm.nb(formula = form, data = mydata[ ! is.na(mydata$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#mod.evi.gau <- glm(formula = form, data = mydata[ ! is.na(mydata$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#
## Compare models using AIC
#AIC(mod.evi.nb, mod.evi.gau)
##             df      AIC
## mod.evi.nb   8 6107.960
## mod.evi.gau  8 6132.008
## Models have similar fit, but the negative binomial model is slightly better
#
## Check model output
#summary(mod.evi.nb)  
## Results
##                     Estimate Std. Error z value Pr(>|t|)    
## (Intercept)        3.193723   0.016844 189.611  < 2e-16 ***
##year.z              0.040294   0.017748   2.270  0.02319 *  
##logN_authors.z     -0.010874   0.020640  -0.527  0.59831    
##logN_countries.z   -0.016223   0.019524  -0.831  0.40600    
##logBodyMass.z      -0.043015   0.016171  -2.660  0.00781 ** 
##GenusRichness.z     0.009696   0.015504   0.625  0.53171    
##TaxonomicReviewYes -0.090686   0.037806  -2.399  0.01645 *
#
## NOTE: results from the neg. binomial model is qualitatively the same as the gaussian model.
#
## Compute R2
#performance::r2(mod.evi.nb) # Nagelkerke's R2: 0.045
#
## Extract and store model results
#results <- bind_rows(results, extract_model_results(mod.evi.nb, "N. evidence I"))
#
## Save model output for latter checking phylogenetic correlation in model residuals
#save(mod.evi.nb, file = 'mod.evi.I.Rdata')

#------------------------------------------------------------#
# Model the number of evidence II
#------------------------------------------------------------#
nrow(mammals_without) # without NA's 268 spp

# Set model formula
form <- as.formula(N_evidencesII ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit a GLM 
mod.evi2.nb.without <- glm.nb(formula = form,
                              data = mammals_without[ ! is.na(mammals_without$N_evidencesII) , ] ) # remove rows with NAs on the response variable
mod.evi2.gau.without <- glm(formula = form, 
                            data = mammals_without[ ! is.na(mammals_without$N_evidencesII) , ] ) # remove rows with NAs on the response variable

# Compare models using AIC
AIC(mod.evi2.nb.without, mod.evi2.gau.without)
# df      AIC
# mod.evi2.nb   8 966
# mod.evi2.gau  8 779.98
# The gaussian model is much better

# Check model output
summary(mod.evi2.gau.without)  
# Results
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         4.27771    0.07479  57.195  < 2e-16 ***
#  year.z              0.18989    0.07597   2.500 0.013044 *  
#  logN_authors.z     -0.07012    0.06285  -1.116 0.265586    
#logN_countries.z    0.07192    0.08001   0.899 0.369554    
#logBodyMass.z      -0.20040    0.05851  -3.425 0.000714 ***
#  logGenusRichness.z  0.12481    0.04679   2.667 0.008129 ** 
#  TaxonomicReview    -0.04817    0.16448  -0.293 0.769846 

# Compute R2
evidences_r2 <- performance::r2(mod.evi2.gau.without) # R2: 0.18

# Extract and store model results
results <- bind_rows(results,
                     extract_model_results(mod.evi2.gau.without,
                                           "N. evidence II"))

# Save model output for latter checking phylogenetic correlation in model residuals
save(mod.evi2.gau.without, file = 'model_outputs/mod.evi.II.without.Rdata')

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Set a full model formula
mammals_without$LogN.Pages <- log10(mammals_without$N.Pages) # transform it 'out' of the model, otherwise there will be an error when calculating R2
form <- as.formula(LogN.Pages ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages.without <- glm(formula = form, family = 'gaussian', data = mammals_without[ !is.na(mammals_without$N.Pages) , ])

# Check results
summary(mod.pages.without) 

# Results:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         0.78631    0.02229  35.269  < 2e-16 ***
#  year.z              0.05771    0.02277   2.534 0.011860 *  
#  logN_authors.z      0.01241    0.01881   0.659 0.510188    
#logN_countries.z   -0.03370    0.02400  -1.404 0.161409    
#logBodyMass.z      -0.06016    0.01758  -3.423 0.000721 ***
#  logGenusRichness.z -0.04871    0.01398  -3.484 0.000581 ***
#  TaxonomicReview    -0.16066    0.04984  -3.223 0.001432 ** 

# Compute R2
pages_r2 <- performance::r2(mod.pages.without) # R2: 0.15

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.pages.without, "N. pages"))

# save model output
save(mod.pages.without, file = 'model_outputs/mod.pages.without.Rdata')

#------------------------------------------------------------#
# Number of specimens 
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(N.Specimens ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit the model 
mod.ts.without <- glm.nb(formula = form, data = mammals_without[ !is.na(mammals_without$N.Specimens) , ])

# Check results
summary(mod.ts.without) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.830773   0.046271  61.179  < 2e-16 ***
# year.z              0.214269   0.048229   4.443 8.88e-06 ***
# logN_authors.z      0.005795   0.056209   0.103   0.9179    
# logN_countries.z   -0.121326   0.053291  -2.277   0.0228 *  
# logBodyMass.z      -0.002750   0.044548  -0.062   0.9508    
# GenusRichness.z     0.211751   0.042540   4.978 6.43e-07 ***
# TaxonomicReviewYes -0.004824   0.102291  -0.047   0.9624 

# Get R2
nspecimens_r2 <- performance::r2(mod.ts.without) # Nagelkerke's R2: 0.24

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.ts.without, "N. specimens"))

# Save model output
save(mod.ts.without, file = 'model_outputs/mod.ts.without.Rdata')

#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(TaxaCompared ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit the model 
mod.tcom.without <- glm.nb(formula = form, data = mammals_without[ !is.na(mammals_without$TaxaCompared) , ])

# Check results
summary(mod.tcom.without) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.72249    0.03188  54.034  < 2e-16 ***
# year.z              0.11168    0.03351   3.333  0.00086 ***
# logN_authors.z     -0.06351    0.03890  -1.633  0.10253    
# logN_countries.z    0.06326    0.03677   1.720  0.08540 .  
# logBodyMass.z       0.01813    0.03049   0.595  0.55209    
# GenusRichness.z     0.22290    0.02942   7.575 3.58e-14 ***
# TaxonomicReviewYes -0.06620    0.07177  -0.922  0.35633

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom.without) # Nagelkerke's R2: 0.294

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.tcom.without, "N. taxa compared"))

# Save model output
save(mod.tcom.without, file = 'model_outputs/mod.tcom.without.Rdata')

# Round numbers 
results[,c(3:5)] <- round(results[,c(3:5)], digits = 3)

# Save as xlsx
dir.create('tables') # create folder to store model results
writexl::write_xlsx(results, 'tables/model_outputs_without.xlsx')
fwrite(results, file = 'model_outputs/model_outs_without.csv')

# Extract R2
results_r2 <- tibble(
  name = c("N. evidence", "N. pages", "N. specimens", "N. taxa compared"),
  value = c(evidences_r2$R2, pages_r2$R2, nspecimens_r2$R2_Nagelkerke, taxacompared_r2$R2_Nagelkerke),
  metric = c("R2", "R2", "R2_Nagelkerke", "R2_Nagelkerke"),
  group = "Non-bats & non-rodents"
)
fwrite(results_r2, file = 'model_outputs/r2_without.csv')

# Clean workspace
rm(list=setdiff(ls(),c("data","mydata", "extract_model_results"))); gc()

## Rodents ----
# Subset data
rodents <- data[ data$Order == 'Rodentia' , ] # n = 421

# Check for skewed distributions and kurtosis among predictors (transform data if necessary).
names(rodents)
e1071::skewness(rodents$N_authors); e1071::kurtosis(rodents$N_authors) # 0.97 and 0.52
e1071::skewness(rodents$N.Countries, na.rm = T); e1071::kurtosis(rodents$N.Countries, na.rm = T) # 1.53 and 3.78
e1071::skewness(rodents$Year); e1071::kurtosis(rodents$Year) # -0.26 and -1.07
e1071::skewness(rodents$SppRichPerGenus, na.rm = T); e1071::kurtosis(rodents$SppRichPerGenus, na.rm = T) # 1.76 and 2.68
# Conclusion: log10 transform no. of countries and species richness per genus [body mass is already transformed]
rodents$N.Countries <- log10(rodents$N.Countries)
rodents$SppRichPerGenus <- log10(rodents$SppRichPerGenus + 1)

# Change taxonomic review to categorical
rodents$TaxonomicReview <- ifelse(rodents$TaxonomicReview==1, yes = 'Yes', no = 'No')

# Standardize continuous predictors (mean = 0, sd =1) in order to make them comparable
rodents$year.z <- scale(rodents$Year) 
rodents$logBodyMass.z <- scale(rodents$Log10BodyMass_g)
rodents$N_authors.z <- scale(rodents$N_authors) 
rodents$logN_countries.z <- scale(rodents$N.Countries) 
rodents$logGenusRichness.z <- scale(rodents$SppRichPerGenus) 

# Remove species with missing values on predictor variables
rodents <- rodents[ complete.cases(year.z, logBodyMass.z, N_authors.z, logN_countries.z,
                                   logGenusRichness.z, TaxonomicReview) , ] 
# n = 330 species with complete data on predictor variables

# Check multicolinearity among predictor variables
usdm::vif(rodents[ , year.z:logGenusRichness.z])
#          Variables      VIF
#             year.z 1.364027
#      logBodyMass.z 1.014609
#        N_authors.z 1.545971
#   logN_countries.z 1.372408
# logGenusRichness.z 1.020982
# Low multicolinearity (VIFs < 2)

# Sample size per response variable
colSums( ! is.na(rodents[ , c("N_evidencesI", "N_evidencesII", "N.Pages", "N.Specimens", "TaxaCompared")]))
# N_evidencesI & N_evidencesII = 330 species  
# N.Pages = 324 species
# N.Specimens = 326 species
# TaxaCompared = 327 species

# Create an empty data frame to store model results
results <- data.frame()

#------------------------------------------------------------#
# Model the number of evidence I
#------------------------------------------------------------#

## Set model formula
#form <- as.formula(N_evidencesI ~ 
#                     year.z + N_authors.z + logN_countries.z + logBodyMass.z + 
#                     logGenusRichness.z + TaxonomicReview)
#
## Fit a GLM
#mod.evi.nb <- glm.nb(formula = form, data = rodents[ ! is.na(rodents$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#mod.evi.gau <- glm(formula = form, data = rodents[ ! is.na(rodents$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#
## Compare models using AIC
#AIC(mod.evi.nb, mod.evi.gau)
##             df      AIC
## mod.evi.nb   8 2304.634
## mod.evi.gau  8 2261.916
## Models have similar fit, but the gaussian model is slightly better
#
## Check model output
#summary(mod.evi.gau)  
## Results
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        26.1734     0.4513  57.992   <2e-16 ***
## year.z               0.1963     0.4653   0.422   0.6734    
## N_authors.z          0.7182     0.5259   1.366   0.1730    
## logN_countries.z     0.5977     0.4730   1.264   0.2073    
## logBodyMass.z        0.6997     0.4012   1.744   0.0821 .  
## logGenusRichness.z   0.1058     0.4028   0.263   0.7929    
## TaxonomicReviewYes  -2.0115     1.0334  -1.946   0.0525 .
#
## Compute R2
#performance::r2(mod.evi.gau) # 0.047
#
## Extract and store model results
#results <- bind_rows(results, extract_model_results(mod.evi.gau, "N. evidence I"))
#
## Save model output for latter checking phylogenetic correlation in model residuals
#save(mod.evi.gau, file = 'model_outputs/mod.evi.I.rodents.Rdata')

#------------------------------------------------------------#
# Model the number of evidence II
#------------------------------------------------------------#

# Set model formula
form <- as.formula(N_evidencesII ~ 
                     year.z + N_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit a GLM 
mod.evi2.nb <- glm.nb(formula = form, data = rodents[ ! is.na(rodents$N_evidencesII) , ] ) 
mod.evi2.gau <- glm(formula = form, data = rodents[ ! is.na(rodents$N_evidencesII) , ] ) 

# Compare models using AIC
AIC(mod.evi2.nb, mod.evi2.gau)
# df      AIC
# mod.evi2.nb   8 1238.3914
# mod.evi2.gau  8 938.0764
# The gaussian model is much better

# Check model output
summary(mod.evi2.gau)  
# Results
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         5.35771    0.06073  88.228  < 2e-16 ***
# year.z              0.13011    0.06260   2.078  0.03846 *  
# N_authors.z        -0.00801    0.07076  -0.113  0.90995    
# logN_countries.z    0.03301    0.06364   0.519  0.60432    
# logBodyMass.z      -0.03911    0.05398  -0.724  0.46932    
# logGenusRichness.z -0.02408    0.05420  -0.444  0.65705    
# TaxonomicReviewYes -0.41226    0.13904  -2.965  0.00325 **

# Compute R2
evidences_r2 <- performance::r2(mod.evi2.gau) # R2: 0.051

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.evi2.gau, "N. evidence II"))

# Save model output for latter checking phylogenetic correlation in model residuals
save(mod.evi2.gau, file = 'model_outputs/mod.evi.II.rodents.Rdata')

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Set a full model formula
rodents$LogN.Pages <- log10(rodents$N.Pages) # transform it 'out' of the model, otherwise there will be an error when calculating R2
form <- as.formula(LogN.Pages ~ 
                     year.z + N_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages <- glm(formula = form, family = 'gaussian', data = rodents[ !is.na(rodents$N.Pages) , ])

# Check results
summary(mod.pages) 

# Results:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.9163981  0.0198647  46.132  < 2e-16 ***
# year.z             -0.0096724  0.0205112  -0.472    0.638    
# N_authors.z         0.0122686  0.0230885   0.531    0.596    
# logN_countries.z    0.0316764  0.0207096   1.530    0.127    
# logBodyMass.z      -0.0056922  0.0176284  -0.323    0.747    
# logGenusRichness.z -0.0779224  0.0176596  -4.412  1.4e-05 ***
# TaxonomicReviewYes -0.0008657  0.0451024  -0.019    0.985

# Compute R2
pages_r2 <- performance::r2(mod.pages) # R2: 0.072

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.pages, "N. pages"))

# save model output
save(mod.pages, file = 'model_outputs/mod.pages.rodents.Rdata')

#------------------------------------------------------------#
# Number of specimens 
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(N.Specimens ~ 
                     year.z + N_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit the model 
mod.ts <- glm.nb(formula = form, data = rodents[ !is.na(rodents$N.Specimens) , ])

# Check results
summary(mod.ts) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.91854    0.07020  41.576  < 2e-16 ***
# year.z              0.20001    0.07204   2.776   0.0055 ** 
# N_authors.z        -0.08404    0.08209  -1.024   0.3059    
# logN_countries.z    0.02310    0.07345   0.314   0.7532    
# logBodyMass.z      -0.05405    0.06222  -0.869   0.3850    
# logGenusRichness.z  0.24800    0.06310   3.930 8.49e-05 ***
# TaxonomicReviewYes -0.23273    0.16158  -1.440   0.1498

# Get R2
nspecimens_r2 <- performance::r2(mod.ts) # 0.113

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.ts, "N. specimens"))

# Save model output
save(mod.ts, file = 'model_outputs/mod.ts.rodents.Rdata')

#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(TaxaCompared ~ 
                     year.z + N_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview)

# Fit the model 
mod.tcom <- glm.nb(formula = form, data = rodents[ !is.na(rodents$TaxaCompared) , ])

# Check results
summary(mod.tcom) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.58253    0.05404  29.283  < 2e-16 ***
# year.z              0.02992    0.05521   0.542    0.588    
# N_authors.z        -0.05179    0.06268  -0.826    0.409    
# logN_countries.z    0.07092    0.05616   1.263    0.207    
# logBodyMass.z       0.02218    0.04760   0.466    0.641    
# logGenusRichness.z  0.20832    0.04893   4.258 2.07e-05 ***
# TaxonomicReviewYes  0.07998    0.12275   0.652    0.515

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom) # R2: 0.099

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.tcom, "N. taxa compared"))

# Save model output
save(mod.tcom, file = 'model_outputs/mod.tcom.rodents.Rdata')

# Round numbers 
results[,c(3:7)] <- round(results[,c(3:7)], digits = 3)

# Save as xlsx
writexl::write_xlsx(results, 'tables/model_outputs_rodents.xlsx')
fwrite(results, file = 'model_outputs/model_outs_rodents.csv')

# Extract R2
results_r2 <- tibble(
  name = c("N. evidence", "N. pages", "N. specimens", "N. taxa compared"),
  value = c(evidences_r2$R2, pages_r2$R2, nspecimens_r2$R2_Nagelkerke, taxacompared_r2$R2_Nagelkerke),
  metric = c("R2", "R2", "R2_Nagelkerke", "R2_Nagelkerke"),
  group = "Rodents"
)
fwrite(results_r2, file = 'model_outputs/r2_rodents.csv')

# Clean workspace
rm(list=setdiff(ls(),c("data", "extract_model_results"))); gc()

## Bats ----
# Subset data
bats <- data[ data$Order == 'Chiroptera' , ] # n = 280

# Check for skewed distributions and kurtosis among predictors (transform data if necessary).
names(bats)
e1071::skewness(bats$N_authors); e1071::kurtosis(bats$N_authors) # 1.60 and 3.04
e1071::skewness(bats$N.Countries, na.rm = T); e1071::kurtosis(bats$N.Countries, na.rm = T) # 1.51 and 2.79
e1071::skewness(bats$Year); e1071::kurtosis(bats$Year) # -0.55 and -0.42
e1071::skewness(bats$SppRichPerGenus, na.rm = T); e1071::kurtosis(bats$SppRichPerGenus, na.rm = T) # 1.36 and 0.42
# Conclusion: log10 transform no. of authors and no. of countries [body mass is already transformed]
bats$N_authors <- log10(bats$N_authors)
bats$N.Countries <- log10(bats$N.Countries)

# Change taxonomic review to categorical
bats$TaxonomicReview <- ifelse(bats$TaxonomicReview==1, yes = 'Yes', no = 'No')

# Standardize continuous predictors (mean = 0, sd =1) in order to make them comparable
bats$year.z <- scale(bats$Year) 
bats$logBodyMass.z <- scale(bats$Log10BodyMass_g)
bats$logN_authors.z <- scale(bats$N_authors) 
bats$logN_countries.z <- scale(bats$N.Countries) 
bats$GenusRichness.z <- scale(bats$SppRichPerGenus) 

# Remove species with missing values on predictor variables
bats <- bats[ complete.cases(year.z, logBodyMass.z, logN_authors.z, logN_countries.z,
                             GenusRichness.z, TaxonomicReview) , ] 
# n = 222 species with complete data on predictor variables

# Check multicolinearity among predictor variables
usdm::vif(bats[ , year.z:GenusRichness.z])
#          Variables      VIF
#             year.z 1.420017
#      logBodyMass.z 1.060475
#     logN_authors.z 2.321813
#   logN_countries.z 1.980449
#    GenusRichness.z 1.045915
# Low multicolinearity (VIFs < 3)

# Sample size per response variable
colSums( ! is.na(bats[ , c("N_evidencesI", "N_evidencesII", "N.Pages", "N.Specimens", "TaxaCompared")]))
# N_evidencesI & N_evidencesII = 222 species  
# N.Pages = 221 species
# N.Specimens = 220 species
# TaxaCompared = 222 species

# Create an empty data frame to store model results
results <- data.frame()

#------------------------------------------------------------#
# Model the number of evidence I
#------------------------------------------------------------#

# Set model formula
#form <- as.formula(N_evidencesI ~ 
#                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
#                     GenusRichness.z + TaxonomicReview)
#
## Fit a GLM
#mod.evi.nb <- glm.nb(formula = form, data = bats[ ! is.na(bats$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#mod.evi.gau <- glm(formula = form, data = bats[ ! is.na(bats$N_evidencesI) , ] ) # remove rows with NAs on the response variable
#
## Compare models using AIC
#AIC(mod.evi.nb, mod.evi.gau)
##             df      AIC
## mod.evi.nb   8 1542.676
## mod.evi.gau  8 1578.516
## Models have similar fit, but the negative binomial model is slightly better
#
## Check model output
#summary(mod.evi.nb)  
## Results
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        3.191952   0.024669 129.389   <2e-16 ***
## year.z             -0.062948   0.025596  -2.459   0.0139 *  
## logN_authors.z      0.020158   0.032335   0.623   0.5330    
## logN_countries.z    0.016072   0.030248   0.531   0.5952    
## logBodyMass.z      -0.036805   0.022736  -1.619   0.1055    
## GenusRichness.z     0.003591   0.025006   0.144   0.8858    
## TaxonomicReviewYes -0.057745   0.054888  -1.052   0.2928
#
## Compute R2
#performance::r2(mod.evi.nb) # 0.078
#
## Extract and store model results
#results <- bind_rows(results, extract_model_results(mod.evi.nb, "N. evidence I"))
#
## Save model output for latter checking phylogenetic correlation in model residuals
#save(mod.evi.nb, file = 'model_outputs/mod.evi.I.bats.Rdata')

#------------------------------------------------------------#
# Model the number of evidence II
#------------------------------------------------------------#

# Set model formula
form <- as.formula(N_evidencesII ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     GenusRichness.z + TaxonomicReview)

# Fit a GLM 
mod.evi2.nb <- glm.nb(formula = form, data = bats[ ! is.na(bats$N_evidencesII) , ] ) 
mod.evi2.gau <- glm(formula = form, data = bats[ ! is.na(bats$N_evidencesII) , ] ) 

# Compare models using AIC
AIC(mod.evi2.nb, mod.evi2.gau)
# df      AIC
# mod.evi2.nb   8 820.3346
# mod.evi2.gau  8 516.2483
# The gaussian model is much better

# Check model output
summary(mod.evi2.gau)  
# Results
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         5.215489   0.057850  90.155   <2e-16 ***
# year.z              0.119287   0.060050   1.986   0.0483 *  
# logN_authors.z      0.189710   0.075603   2.509   0.0128 *  
# logN_countries.z    0.006906   0.070749   0.098   0.9223    
# logBodyMass.z      -0.045654   0.052054  -0.877   0.3814    
# GenusRichness.z    -0.034633   0.058647  -0.591   0.5554    
# TaxonomicReviewYes -0.012132   0.127558  -0.095   0.9243

# Compute R2
evidences_r2 <- performance::r2(mod.evi2.gau) # R2: 0.135

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.evi2.gau, "N. evidence II"))

# Save model output for latter checking phylogenetic correlation in model residuals
save(mod.evi2.gau, file = 'model_outputs/mod.evi.II.bats.Rdata')

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Set a full model formula
bats$LogN.Pages <- log10(bats$N.Pages) # transform it 'out' of the model, otherwise there will be an error when calculating R2
form <- as.formula(LogN.Pages ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     GenusRichness.z + TaxonomicReview)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages <- glm(formula = form, family = 'gaussian', data = bats[ !is.na(bats$N.Pages) , ])

# Check results
summary(mod.pages) 

# Results:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         0.866935   0.025286  34.285  < 2e-16 ***
# year.z             -0.028601   0.026293  -1.088  0.27792    
# logN_authors.z      0.107835   0.032947   3.273  0.00124 ** 
# logN_countries.z    0.014006   0.030835   0.454  0.65013    
# logBodyMass.z       0.008744   0.022693   0.385  0.70040    
# GenusRichness.z    -0.027576   0.025567  -1.079  0.28199    
# TaxonomicReviewYes  0.071381   0.055631   1.283  0.20084

# Compute R2
pages_r2 <- performance::r2(mod.pages) # R2: 0.099

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.pages, "N. pages"))

# save model output
save(mod.pages, file = 'model_outputs/mod.pages.bats.Rdata')

#------------------------------------------------------------#
# Number of specimens 
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(N.Specimens ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     GenusRichness.z + TaxonomicReview)

# Fit the model 
mod.ts <- glm.nb(formula = form, data = bats[ !is.na(bats$N.Specimens) , ])

# Check results
summary(mod.ts) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         2.59587    0.09012  28.805  < 2e-16 ***
# year.z              0.01687    0.09303   0.181  0.85613    
# logN_authors.z      0.36841    0.11707   3.147  0.00165 ** 
# logN_countries.z   -0.30374    0.10957  -2.772  0.00557 ** 
# logBodyMass.z       0.17565    0.07984   2.200  0.02781 *  
# GenusRichness.z    -0.24168    0.09178  -2.633  0.00846 ** 
# TaxonomicReviewYes  0.58824    0.19576   3.005  0.00266 **

# Get R2
nspecimens_r2 <- performance::r2(mod.ts) # 0.156

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.ts, "N. specimens"))

# Save model output
save(mod.ts, file = 'model_outputs/mod.ts.bats.Rdata')

#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(TaxaCompared ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     GenusRichness.z + TaxonomicReview)

# Fit the model 
mod.tcom <- glm.nb(formula = form, data = bats[ !is.na(bats$TaxaCompared) , ])

# Check results
summary(mod.tcom) 

# Results
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         1.88614    0.05485  34.389  < 2e-16 ***
# year.z              0.10315    0.05797   1.779  0.07519 .  
# logN_authors.z      0.12318    0.07204   1.710  0.08729 .  
# logN_countries.z   -0.01683    0.06677  -0.252  0.80105    
# logBodyMass.z       0.01993    0.04961   0.402  0.68795    
# GenusRichness.z     0.14248    0.05405   2.636  0.00839 ** 
# TaxonomicReviewYes -0.14236    0.12364  -1.151  0.24953

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom) # R2: 0.172

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.tcom, "N. taxa compared"))

# Save model output
save(mod.tcom, file = 'model_outputs/mod.tcom.bats.Rdata')

# Round numbers 
results[,c(3:7)] <- round(results[,c(3:7)], digits = 3)

# Save as xlsx
writexl::write_xlsx(results, 'tables/model_outputs_bats.xlsx')
fwrite(results, file = 'model_outputs/model_outs_bats.csv')

# Extract R2
results_r2 <- tibble(
  name = c("N. evidence", "N. pages", "N. specimens", "N. taxa compared"),
  value = c(evidences_r2$R2, pages_r2$R2, nspecimens_r2$R2_Nagelkerke, taxacompared_r2$R2_Nagelkerke),
  metric = c("R2", "R2", "R2_Nagelkerke", "R2_Nagelkerke"),
  group = "Bats"
)
fwrite(results_r2, file = 'model_outputs/r2_bats.csv')

# Clean workspace
rm(list = ls()); gc()

# 6) Create a plot with model coefficients and CI intervals.----
# Load
rm(list = ls()); gc()
results_all <- fread('model_outputs/model_outs.csv')
results_without <- fread('model_outputs/model_outs_without.csv')
results_rodents <- fread('model_outputs/model_outs_rodents.csv')
results_bats <- fread('model_outputs/model_outs_bats.csv')

r2_all <- fread('model_outputs/r2_allmammals.csv')
r2_without <- fread('model_outputs/r2_without.csv')
r2_rodents <- fread('model_outputs/r2_rodents.csv')
r2_bats <- fread('model_outputs/r2_bats.csv')

# Add column informing taxon coverage
results_all$group <- 'All mammals'
results_without$group <- 'Non-bats & non-rodents'
results_rodents$group <- 'Rodents'
results_bats$group <- 'Bats'

# Combine datasets
results <- rbind(results_all, results_without, 
                 results_bats, results_rodents) %>%
  mutate(group = factor(group,
                                  levels = c(
                                    "All mammals",
                                    "Non-bats & non-rodents",
                                    "Bats",
                                    "Rodents")))

# Reorder predictors and fix names
levels(as.factor(results$term))
results <- results[ ! results$term=="(Intercept)", ] # remove intercept
results$term <- factor(results$term, 
                       levels = c("GenusRichness.z","logBodyMass.z","logGenusRichness.z", "logN_authors.z",  
                                  "logN_countries.z","N_authors.z","TaxonomicReviewYes", "year.z"),
                       labels = c("Number of\nspecies/genus", "Body mass", "Number of\nspecies/genus", "Number of\nauthors", 
                                  "Number of\ncountries", "Number of\nauthors", "Taxonomic\nreview", "Year of\ndescription"))

levels(as.factor(results$response))

# Make the plot
#p <- ggplot(results, aes(x = response, y = estimate, shape = response, ymin = lower95, ymax = upper95))+
#  # Add rectangular boxes in the background (to improve contrast):
#  geom_rect(xmin=0, xmax=11, ymin=-Inf, ymax=+Inf, fill="grey95", color="transparent") + # Socioeconomic
#  geom_pointrange(aes(col = response), size = 0.3)+ # shape = Class
#  scale_shape_manual(values=c(0,1,2,4,5))+
#  scale_color_brewer(type = 'qual', palette = 'Set2')+
#  geom_errorbar(aes(ymin=lower95, ymax=upper95, col = response), width=0.1)+
#  geom_hline(yintercept = 0, linetype = 2)+
#  labs(x=NULL, y="Model Coefficients (CI 95%)")+
#  scale_x_discrete(limits = rev(levels(as.factor(results$response))))+
#  facet_grid(term ~ group, scales = 'free_x', switch = "y") +  # Use facet_grid and switch strip position to the left
#  theme(panel.grid.minor = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.background = element_blank(),
#        axis.title = element_text(size=12, face="bold"),
#        axis.text = element_text(size=10),
#        axis.line = element_line(colour="black"),
#        axis.ticks.y.left = element_blank(),
#        axis.text.x=element_text(),
#        axis.text.y = element_blank(),
#        plot.background=element_rect(fill = "white"),
#        strip.background = element_blank(),
#        strip.placement = "outside",
#        strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 10),
#        legend.background = element_rect(colour = 'black', fill = 'white'),
#        legend.key = element_blank(),
#        legend.title = element_blank(),
#        legend.position = "top")+
#  coord_flip(); p

# save figure as pdf
#ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs.png"), plot=p, width=10, height=6, units="in", dpi = 'print')
#ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs.jpg"), plot=p, width=10, height=6, units="in", dpi = 'print')
#ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs.pdf"), plot=p, width=10, height=6, units="in", dpi = 'print', cairo_pdf)

MyColors <- c("#8e0152", "#bf812d", "#4d4d4d", "#d6604d")
names(MyColors) <- c("N. evidence II","N. pages","N. specimens","N. taxa compared")

# Define background data to apply unique colors to each 'group' column
background_data <- data.frame(
  group = unique(results$group),  # Each unique 'group' will get its own color
  fill = c("#bfbbbc","#fa8e9f","#bbedbb", "#7d9cc7")  # Define a color for each 'group'
)

p <- ggplot(results, aes(x = response, y = estimate, shape = response, ymin = lower95, ymax = upper95)) +
  # Add a geom_rect layer for each 'group' level with specific fill color, without legend
  geom_rect(data = background_data, 
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = fill),
            inherit.aes = FALSE, color = "transparent", show.legend = FALSE, alpha = 0.3) +
  geom_pointrange(aes(col = response), size = 0.2) +
  scale_shape_manual(values = c(0, 1, 2, 4, 5)) +
  scale_color_manual(values = MyColors) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95, col = response), width = 0.1) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = NULL, y = "Model Coefficients (CI 95%)") +
  scale_x_discrete(limits = rev(levels(as.factor(results$response)))) +
  facet_grid(term ~ group, scales = 'free_x', switch = "y") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    axis.ticks.y.left = element_blank(),
    axis.text.x = element_text(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white"),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 10),
    legend.background = element_rect(colour = 'white', fill = 'white'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = "top") +
  coord_flip() +
  scale_fill_identity(); p  # Use fill colors directly without a scale

ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs2.png"), plot=p, width=10, height=6, units="in", dpi = 'print')
ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs2.jpg"), plot=p, width=10, height=6, units="in", dpi = 'print')
ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs2.pdf"), plot=p, width=10, height=6, units="in", dpi = 'print', cairo_pdf)

# Inset plot with R2
background_colors <- c(
  "All mammals" = "black",
  "Non-bats & non-rodents" = "#ff3352",
  "Bats" = "#7fc97f",
  "Rodents" = "#386cb0"
)

results_r2 <- rbind(r2_all, r2_without, r2_rodents, r2_bats) %>%
  mutate(
    group = factor(group, levels = names(background_colors)),
    name = factor(name, levels = c(
      "N. taxa compared",
      "N. specimens",
      "N. pages",
      "N. evidence"
    )))

# Plot
inset_plot <- ggplot(results_r2, aes(x = name, y = value, fill = group)) +
  # Barras
  geom_col(position = position_dodge(width = 0.9), width = 0.7, alpha = 0.3,
           color = "black", size = 0.2) +
  
  # Ajuste de escala para o eixo X após o coord_flip
  scale_x_discrete(expand = c(0, 0)) +  # Evita o espaçamento extra
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.1), limits = c(0, 0.3), expand = c(0, 0)) +  # Definindo limite superior no eixo Y (horizontal)
  scale_fill_manual(values = background_colors, guide = "none") +
  
  # Layout
  facet_wrap(~ group, ncol = 1) +
  coord_flip() +
  labs(x = NULL, y = "Model R²") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    axis.line.x = element_line(color = "black", size = 0.5),  # Linha do eixo X após coord_flip
    axis.ticks.x = element_line(color = "black", size = 0.5),  
    axis.line.y = element_line(color = "black", size = 0.5),  # Linha do eixo Y (horizontal após coord_flip)
    axis.ticks.y = element_line(color = "black", size = 0.5),  # Marcas no eixo Y (horizontal após coord_flip)
    legend.position = "none"
  ); inset_plot

ggsave(paste0(getwd(), "/figures/Figure.aux2.pdf"), plot=inset_plot,
       width=4, height=6, units="in", dpi = 'print', cairo_pdf)

# 7) Check phylogenetic correlation in model residuals.----
# Load additional packages
needed_packages <- c('foreach', # for looping construct (package version 1.5.2)
                     'doParallel', # for parallel computing (v. 1.0.17)
                     'fuzzyjoin',
                     'tidyverse',
                     'data.table',
                     # for phylogenetic analysis:
                     'geiger', # (v. 2.0.10)
                     'phytools', # (v. 1.2.0)
                     "phylobase", # (v. 0.8.10)
                     "phylosignal") # (v. 1.3)
new.packages<-needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(needed_packages, require, character.only = TRUE)

# Load main dataset
load("Dataset.Rdata")

data <- data %>%
  drop_na(Year, Log10BodyMass_g, N_authors, N.Countries, 
          SppRichPerGenus, TaxonomicReview) # 820 linhas

# Sample size per response variable
colSums( ! is.na(data[ , c("N_evidencesI", "N_evidencesII", "N.Pages", "N.Specimens", "TaxaCompared")]))
# N_evidencesI & N_evidencesII = 820 species  
# N.Pages = 807 species
# N.Specimens = 805 species
# TaxaCompared = 810 species
# OKAY; match the numbers on modelling procedures

# Make sure that all species in the tree are also in the main dataset, and vice-versa
# First, let's standardize names and then use fuzzy logic to identify potential small
# misspellings that may lead to the inadvertdly exclusion of some species
data$SpeciesName <- stringr::str_to_sentence(data$SpeciesName)
data$SpeciesName <- gsub(' ', '_', data$SpeciesName) 
data$SpeciesName <- stringr::str_trim(data$SpeciesName) # remove whitespaces from end and begining

# The phylogenetic trees provided are 100-randomly selected  phylogenies from 
# Upham et al. (2019). PLoS Biology, 17(12), 1–44. https://doi.org/10.1371/journal.pbio.3000494
mammal_tree <- ape::read.nexus(
  'phylogeny/output.nex')

# Standardize scientific names on trees
for (i in seq_along(mammal_tree)) {
  mammal_tree[[i]]$tip.label <- stringr::str_to_sentence(mammal_tree[[i]]$tip.label)
  mammal_tree[[i]]$tip.label <- gsub(' ', '_', mammal_tree[[i]]$tip.label) 
  mammal_tree[[i]]$tip.label <- stringr::str_trim(mammal_tree[[i]]$tip.label)
}

# Use fuzzy logic to find name mismatches due to minor misspellings
spp_on_tree <- mammal_tree[[1]]$tip.label
spp_on_data <- data$SpeciesName

diff_tree <- as.data.frame(setdiff(spp_on_tree, spp_on_data)) 
colnames(diff_tree)[1] <- 'SpeciesName'
diff_mydata <- as.data.frame(setdiff(spp_on_data, spp_on_tree))
colnames(diff_mydata)[1] <- 'SpeciesName'

fuzzy_match <- stringdist_join(diff_mydata, diff_tree, 
                               by='SpeciesName', # match based on spp names
                               mode='left', # use left join
                               method = "jw", # use jw distance metric
                               max_dist=99, 
                               distance_col='dist'); rm(diff_mydata, diff_tree, spp_on_data, spp_on_tree)
# Usually, most distances < 0.07 represent the same species slightly misspelled.
fuzzy_match <- arrange(fuzzy_match, dist) # arrange by increasing value
fuzzy_match <- fuzzy_match[ ! fuzzy_match$dist > 0.07, ]; print(fuzzy_match) # 45 species
# Remove species that are not the same
fuzzy_match <- fuzzy_match[ - c(10:13) , ] 
# 9 species to fix names that are written slightly different but represent the same species
spp_to_fix <- c(t(fuzzy_match[ , 'SpeciesName.x'])) # store misspelled species name into a vector 
corrected_spp <- c(t(fuzzy_match[ , 'SpeciesName.y']))

# Iteratively update names
for (i in seq_along(spp_to_fix)) {
  data[data$SpeciesName == spp_to_fix[i], 'SpeciesName'] <- corrected_spp[i]
}
rm(fuzzy_match, spp_to_fix, corrected_spp)

# Add species names as rows in the main dataset
data <- as.data.frame(data)
rownames(data) <- data[ , 'SpeciesName']

# Load model residuals, then create distinct datasets for each response to account for NAs
# evidence I
#load("model_outputs/mod.evi.I.Rdata") ; evi_residsI <- resid(mod.evi.nb)
#load("model_outputs/mod.evi.I.bats.Rdata") ; evi_residsI_bats <- resid(mod.evi.nb)
#load("model_outputs/mod.evi.I.rodents.Rdata") ; evi_residsI_rodents <- resid(mod.evi.gau)

# evidence II
load("model_outputs/mod.evi.II.Rdata") ; evi_residsII <- resid(mod.evi2.gau)
load("model_outputs/mod.evi.II.without.Rdata") ; evi_residsII_without <- resid(mod.evi2.gau.without)
load("model_outputs/mod.evi.II.bats.Rdata") ; evi_residsII_bats <- resid(mod.evi2.gau)
load("model_outputs/mod.evi.II.rodents.Rdata") ; evi_residsII_rodents <- resid(mod.evi2.gau)

# Pages
load("model_outputs/mod.pages.Rdata") ; pag_resids <- resid(mod.pages)
load("model_outputs/mod.pages.without.Rdata") ; pag_resids_without <- resid(mod.pages.without)
load("model_outputs/mod.pages.bats.Rdata") ; pag_resids_bats <- resid(mod.pages)
load("model_outputs/mod.pages.rodents.Rdata") ; pag_resids_rodents <- resid(mod.pages)

# N. Specimens
load("model_outputs/mod.ts.Rdata") ; ts_resids <- resid(mod.ts)
load("model_outputs/mod.ts.without.Rdata") ; ts_resids_without <- resid(mod.ts.without)
load("model_outputs/mod.ts.bats.Rdata") ; ts_resids_bats <- resid(mod.ts)
load("model_outputs/mod.ts.rodents.Rdata") ; ts_resids_rodents <- resid(mod.ts)

# Taxa compared
load("model_outputs/mod.tcom.Rdata") ; tcom_resids <- resid(mod.tcom)
load("model_outputs/mod.tcom.without.Rdata") ; tcom_resids_without <- resid(mod.tcom.without)
load("model_outputs/mod.tcom.bats.Rdata") ; tcom_resids_bats <- resid(mod.tcom)
load("model_outputs/mod.tcom.rodents.Rdata") ; tcom_resids_rodents <- resid(mod.tcom)

# clean workspace
rm(mod.evi.gau, mod.evi.nb, mod.evi2.gau, mod.pages, mod.ts, mod.tcom)

# Add residual values into datasets (for all mammals as well as bats and rodents separately)
#evi_datI <- as.data.frame(cbind( data[ ! is.na(data$N_evidencesI) , ], evi_residsI)); rm(evi_residsI)
#evi_datI_bats <- as.data.frame(cbind( data[ data$Order == 'Chiroptera' & (! is.na(data$N_evidencesI)) , ], evi_residsI_bats)); rm(evi_residsI_bats)
#evi_datI_rodents <- as.data.frame(cbind( data[ data$Order == 'Rodentia' & (! is.na(data$N_evidencesI)) , ], evi_residsI_rodents)); rm(evi_residsI_rodents)

# N. evidences II
evi_datII <- as.data.frame(
  cbind( 
    data[ ! is.na(data$N_evidencesII) , ], evi_residsII
    )
  ); rm(evi_residsII)
evi_datII_bats <- as.data.frame(
  cbind(
    data[ data$Order == 'Chiroptera' & (! is.na(data$N_evidencesII)) , ],
    evi_residsII_bats
    )
  ); rm(evi_residsII_bats)
evi_datII_without <- as.data.frame(
  cbind(
    data[ data$Order != 'Chiroptera' & data$Order != 'Rodentia' & (! is.na(data$N_evidencesII)) , ]
    , evi_residsII_without
    )
  ); rm(evi_residsII_without)
evi_datII_rodents <- as.data.frame(
  cbind(
    data[data$Order == "Rodentia" & !is.na(data$N_evidencesII), ],
    evi_residsII_rodents
  )
); rm(evi_residsII_rodents)

# N. Pages
pages_dat <- as.data.frame(
  cbind(
    data[!is.na(data$N.Pages), ],
    pag_resids
  )
); rm(pag_resids)
pages_dat_bats <- as.data.frame(
  cbind(
    data[data$Order == "Chiroptera" & !is.na(data$N.Pages), ],
    pag_resids_bats
  )
); rm(pag_resids_bats)
pages_dat_without <- as.data.frame(
  cbind(
    data[
      data$Order != "Chiroptera" &
        data$Order != "Rodentia" &
        !is.na(data$N.Pages),
    ],
    pag_resids_without
  )
); rm(pag_resids_without)
pages_dat_rodents <- as.data.frame(
  cbind(
    data[data$Order == "Rodentia" & !is.na(data$N.Pages), ],
    pag_resids_rodents
  )
); rm(pag_resids_rodents)

# N. Specimens (ts)
ts_dat <- as.data.frame(
  cbind(
    data[!is.na(data$N.Specimens), ],
    ts_resids
  )
); rm(ts_resids)
ts_dat_without <- as.data.frame(
  cbind(
    data[
      data$Order != "Chiroptera" &
        data$Order != "Rodentia" &
        !is.na(data$N.Specimens),
    ],
    ts_resids_without
  )
); rm(ts_resids_without)
ts_dat_bats <- as.data.frame(
  cbind(
    data[data$Order == "Chiroptera" & !is.na(data$N.Specimens), ],
    ts_resids_bats
  )
); rm(ts_resids_bats)
ts_dat_rodents <- as.data.frame(
  cbind(
    data[data$Order == "Rodentia" & !is.na(data$N.Specimens), ],
    ts_resids_rodents
  )
); rm(ts_resids_rodents)

# Taxa compared (tcom)
tcom_dat <- as.data.frame(
  cbind(
    data[!is.na(data$TaxaCompared), ],
    tcom_resids
  )
); rm(tcom_resids)
tcom_dat_without <- as.data.frame(
  cbind(
    data[
      data$Order != "Chiroptera" &
        data$Order != "Rodentia" &
        !is.na(data$TaxaCompared),
    ],
    tcom_resids_without
  )
); rm(tcom_resids_without)
tcom_dat_bats <- as.data.frame(
  cbind(
    data[data$Order == "Chiroptera" & !is.na(data$TaxaCompared), ],
    tcom_resids_bats
  )
); rm(tcom_resids_bats)
tcom_dat_rodents <- as.data.frame(
  cbind(
    data[data$Order == "Rodentia" & !is.na(data$TaxaCompared), ],
    tcom_resids_rodents
  )
); rm(tcom_resids_rodents)

## All mammals  ----

##------------------------------------------------------------#
## Number of evidence I
##------------------------------------------------------------#
#
## Check if we have the same spp in our data as in the tree
#obj <- geiger::name.check(mammal_tree[[1]], evi_datI)
#
## Drop species not present on the tree
#evi_datI <- evi_datI[ ! evi_datI$SpeciesName %in% obj$data_not_tree , ]
#
## Remove species present on the tree but not on the dataset
#phylo_tree_evi<-list()
#
## Remove spp that are in the phylogeny but not in the dataset
#for (i in 1:100) {
#  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
#} 
#name.check(phylo_tree_evi[[1]], evi_datI); rm(obj) # OK = all species on phylogeny matching those on the data frame
#
## Prepare workspace for parallel computing:
#cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK') # selecting half of all available cores
#registerDoParallel(cl)
#getDoParWorkers()
#
#{
#  PhyCorr_evi_I<-foreach(i = 1:100, 
#                         .export = 'rbind',
#                         .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
#                           
#                           # Select one trimmed fully-sampled tree:
#                           my_tree<-phylo_tree_evi[[i]]
#                           
#                           # Create a phylo4 object including GLMM model residuals:
#                           phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datI$evi_residsI))
#                           
#                           # Compute the phylogenetic correlogram:
#                           phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
#                                                                  dist.phylo="patristic", n.points=14, ci.bs=100)
#                           
#                           correlogram_data<-as.data.frame(phy.cor[[1]])
#                           names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
#                           correlogram_data$Iter<-i
#                           correlogram_data$N_class<-1:14
#                           correlogram_data
#                           
#                         }
#  stopCluster(cl) # terminate cluster
#}
#
## Extract the average correlogram output across iterations:
#PhyCorr_evi_I<- as.data.table(rbindlist(PhyCorr_evi_I))
#AvgPhyCorr_evi_I<-PhyCorr_evi_I[, .(Distance=mean(dist.class, na.rm=T),
#                                    Lower_CI=mean(lower_ci, na.rm=T),
#                                    Upper_CI=mean(upper_ci, na.rm=T),
#                                    MoranI_coef=mean(coef, na.rm=T)),
#                                by = .(N_class)]
#
## Export the results:
#dir.create('PhyloCorr')
#save(PhyCorr_evi_I, AvgPhyCorr_evi_I, file="PhyloCorr/PhyloCorr_evi_I.Rdata")
#rm(evi_datI, PhyCorr_evi_I, AvgPhyCorr_evi_I) # clean workspace
#
#------------------------------------------------------------#
# Number of evidence II
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], evi_datII)

# Drop species not present on the tree
evi_datII <- evi_datII[ ! evi_datII$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_evi<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_evi[[1]], evi_datII); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_evi_II<-foreach(i = 1:100, 
                          .export = 'rbind',
                          .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                            
                            # Select one trimmed fully-sampled tree:
                            my_tree<-phylo_tree_evi[[i]]
                            
                            # Create a phylo4 object including GLMM model residuals:
                            phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datII$evi_residsII))
                            
                            # Compute the phylogenetic correlogram:
                            phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                   dist.phylo="patristic", n.points=14, ci.bs=100)
                            
                            correlogram_data<-as.data.frame(phy.cor[[1]])
                            names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                            correlogram_data$Iter<-i
                            correlogram_data$N_class<-1:14
                            correlogram_data
                            
                          }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_evi_II<-as.data.table(rbindlist(PhyCorr_evi_II))
AvgPhyCorr_evi_II<-PhyCorr_evi_II[, .(Distance=mean(dist.class, na.rm=T),
                                      Lower_CI=mean(lower_ci, na.rm=T),
                                      Upper_CI=mean(upper_ci, na.rm=T),
                                      MoranI_coef=mean(coef, na.rm=T)),
                                  by = .(N_class)]

# Export the results:
save(PhyCorr_evi_II, AvgPhyCorr_evi_II, file="PhyloCorr/PhyloCorr_evi_II.Rdata")
rm(evi_datII, PhyCorr_evi_II, AvgPhyCorr_evi_II) # clean workspace

#------------------------------------------------------------#
# Number of specimens
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], ts_dat)

# Drop species not present on the tree
ts_dat <- ts_dat[ ! ts_dat$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_ts<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_ts[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_ts[[1]], ts_dat); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_ts<-foreach(i = 1:100, 
                      .export = 'rbind',
                      .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                        
                        # Select one trimmed fully-sampled tree:
                        my_tree<-phylo_tree_ts[[i]]
                        
                        # Create a phylo4 object including GLMM model residuals:
                        phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=ts_dat$ts_resids))
                        
                        # Compute the phylogenetic correlogram:
                        phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                               dist.phylo="patristic", n.points=14, ci.bs=100)
                        
                        correlogram_data<-as.data.frame(phy.cor[[1]])
                        names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                        correlogram_data$Iter<-i
                        correlogram_data$N_class<-1:14
                        correlogram_data
                        
                      }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_ts<-as.data.table(rbindlist(PhyCorr_ts))
AvgPhyCorr_ts<-PhyCorr_ts[, .(Distance=mean(dist.class, na.rm=T),
                              Lower_CI=mean(lower_ci, na.rm=T),
                              Upper_CI=mean(upper_ci, na.rm=T),
                              MoranI_coef=mean(coef, na.rm=T)),
                          by = .(N_class)]

# Export the results:
save(PhyCorr_ts, AvgPhyCorr_ts, file="PhyloCorr/PhyloCorr_ts.Rdata")
rm(ts_dat, PhyCorr_ts, AvgPhyCorr_ts) # clean workspace

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], pages_dat)

# Drop species not present on the tree
pages_dat <- pages_dat[ ! pages_dat$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_pages<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_pages[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_pages[[1]], pages_dat); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_pages<-foreach(i = 1:100, 
                         .export = 'rbind',
                         .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                           
                           # Select one trimmed fully-sampled tree:
                           my_tree<-phylo_tree_pages[[i]]
                           
                           # Create a phylo4 object including GLMM model residuals:
                           phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=pages_dat$pag_resids))
                           
                           # Compute the phylogenetic correlogram:
                           phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                  dist.phylo="patristic", n.points=14, ci.bs=100)
                           
                           correlogram_data<-as.data.frame(phy.cor[[1]])
                           names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                           correlogram_data$Iter<-i
                           correlogram_data$N_class<-1:14
                           correlogram_data
                           
                         }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_pages<-as.data.table(rbindlist(PhyCorr_pages))
AvgPhyCorr_pages<-PhyCorr_pages[, .(Distance=mean(dist.class, na.rm=T),
                                    Lower_CI=mean(lower_ci, na.rm=T),
                                    Upper_CI=mean(upper_ci, na.rm=T),
                                    MoranI_coef=mean(coef, na.rm=T)),
                                by = .(N_class)]

# Export the results:
save(PhyCorr_pages, AvgPhyCorr_pages, file="PhyloCorr/PhyloCorr_pages.Rdata")
rm(pages_dat, PhyCorr_pages, AvgPhyCorr_pages) # clean workspace


#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], tcom_dat)

# Drop species not present on the tree
tcom_dat <- tcom_dat[ ! tcom_dat$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_tcom<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_tcom[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_tcom[[1]], tcom_dat); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_tcom<-foreach(i = 1:100, 
                        .export = 'rbind',
                        .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                          
                          # Select one trimmed fully-sampled tree:
                          my_tree<-phylo_tree_tcom[[i]]
                          
                          # Create a phylo4 object including GLMM model residuals:
                          phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=tcom_dat$tcom_resids))
                          
                          # Compute the phylogenetic correlogram:
                          phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                 dist.phylo="patristic", n.points=14, ci.bs=100)
                          
                          correlogram_data<-as.data.frame(phy.cor[[1]])
                          names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                          correlogram_data$Iter<-i
                          correlogram_data$N_class<-1:14
                          correlogram_data
                          
                        }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_tcom<-as.data.table(rbindlist(PhyCorr_tcom))
AvgPhyCorr_tcom<-PhyCorr_tcom[, .(Distance=mean(dist.class, na.rm=T),
                                  Lower_CI=mean(lower_ci, na.rm=T),
                                  Upper_CI=mean(upper_ci, na.rm=T),
                                  MoranI_coef=mean(coef, na.rm=T)),
                              by = .(N_class)]

# Export the results:
save(PhyCorr_tcom, AvgPhyCorr_tcom, file="PhyloCorr/PhyloCorr_tcom.Rdata")
rm(tcom_dat, PhyCorr_tcom, AvgPhyCorr_tcom) # clean workspace

## Non-bats & non-rodents ----

#------------------------------------------------------------#
# Number of evidence I
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
#obj <- geiger::name.check(mammal_tree[[1]], evi_datI)
#
## Drop species not present on the tree
#evi_datI <- evi_datI[ ! evi_datI$SpeciesName %in% obj$data_not_tree , ]
#
## Remove species present on the tree but not on the dataset
#phylo_tree_evi<-list()
#
## Remove spp that are in the phylogeny but not in the dataset
#for (i in 1:100) {
#  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
#} 
#name.check(phylo_tree_evi[[1]], evi_datI); rm(obj) # OK = all species on phylogeny matching those on the data frame
#
## Prepare workspace for parallel computing:
#cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK') # selecting half of all available cores
#registerDoParallel(cl)
#getDoParWorkers()
#
#{
#  PhyCorr_evi_I<-foreach(i = 1:100, 
#                         .export = 'rbind',
#                         .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
#                           
#                           # Select one trimmed fully-sampled tree:
#                           my_tree<-phylo_tree_evi[[i]]
#                           
#                           # Create a phylo4 object including GLMM model residuals:
#                           phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datI$evi_residsI))
#                           
#                           # Compute the phylogenetic correlogram:
#                           phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
#                                                                  dist.phylo="patristic", n.points=14, ci.bs=100)
#                           
#                           correlogram_data<-as.data.frame(phy.cor[[1]])
#                           names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
#                           correlogram_data$Iter<-i
#                           correlogram_data$N_class<-1:14
#                           correlogram_data
#                           
#                         }
#  stopCluster(cl) # terminate cluster
#}
#
## Extract the average correlogram output across iterations:
#PhyCorr_evi_I<- as.data.table(rbindlist(PhyCorr_evi_I))
#AvgPhyCorr_evi_I<-PhyCorr_evi_I[, .(Distance=mean(dist.class, na.rm=T),
#                                    Lower_CI=mean(lower_ci, na.rm=T),
#                                    Upper_CI=mean(upper_ci, na.rm=T),
#                                    MoranI_coef=mean(coef, na.rm=T)),
#                                by = .(N_class)]
#
## Export the results:
#dir.create('PhyloCorr')
#save(PhyCorr_evi_I, AvgPhyCorr_evi_I, file="PhyloCorr/PhyloCorr_evi_I.Rdata")
#rm(evi_datI, PhyCorr_evi_I, AvgPhyCorr_evi_I) # clean workspace

#------------------------------------------------------------#
# Number of evidence II
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], evi_datII_without)

# Drop species not present on the tree
evi_datII_without <- evi_datII_without[ ! evi_datII_without$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_evi<- list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_evi[[1]], evi_datII_without); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_evi_II_without<-foreach(i = 1:100, 
                          .export = 'rbind',
                          .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                            
                            # Select one trimmed fully-sampled tree:
                            my_tree<-phylo_tree_evi[[i]]
                            
                            # Create a phylo4 object including GLMM model residuals:
                            phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datII_without$evi_residsII_without))
                            
                            # Compute the phylogenetic correlogram:
                            phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                   dist.phylo="patristic", n.points=14, ci.bs=100)
                            
                            correlogram_data<-as.data.frame(phy.cor[[1]])
                            names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                            correlogram_data$Iter<-i
                            correlogram_data$N_class<-1:14
                            correlogram_data
                            
                          }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_evi_II_without<-as.data.table(rbindlist(PhyCorr_evi_II_without))
AvgPhyCorr_evi_II_without<-PhyCorr_evi_II_without[, .(Distance=mean(dist.class, na.rm=T),
                                      Lower_CI=mean(lower_ci, na.rm=T),
                                      Upper_CI=mean(upper_ci, na.rm=T),
                                      MoranI_coef=mean(coef, na.rm=T)),
                                  by = .(N_class)]

# Export the results:
save(PhyCorr_evi_II_without, AvgPhyCorr_evi_II_without,
     file="PhyloCorr/PhyloCorr_evi_II_without.Rdata")
rm(evi_datII_without, PhyCorr_evi_II_without, AvgPhyCorr_evi_II_without) # clean workspace

#------------------------------------------------------------#
# Number of specimens
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], ts_dat_without)

# Drop species not present on the tree
ts_dat_without <- ts_dat_without[ ! ts_dat_without$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_ts<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_ts[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_ts[[1]], ts_dat); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_ts_without<-foreach(i = 1:100, 
                      .export = 'rbind',
                      .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                        
                        # Select one trimmed fully-sampled tree:
                        my_tree<-phylo_tree_ts[[i]]
                        
                        # Create a phylo4 object including GLMM model residuals:
                        phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=ts_dat_without$ts_resids))
                        
                        # Compute the phylogenetic correlogram:
                        phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                               dist.phylo="patristic", n.points=14, ci.bs=100)
                        
                        correlogram_data<-as.data.frame(phy.cor[[1]])
                        names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                        correlogram_data$Iter<-i
                        correlogram_data$N_class<-1:14
                        correlogram_data
                        
                      }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_ts_without<-as.data.table(rbindlist(PhyCorr_ts_without))
AvgPhyCorr_ts_without<-PhyCorr_ts_without[, .(Distance=mean(dist.class, na.rm=T),
                              Lower_CI=mean(lower_ci, na.rm=T),
                              Upper_CI=mean(upper_ci, na.rm=T),
                              MoranI_coef=mean(coef, na.rm=T)),
                          by = .(N_class)]

# Export the results:
save(PhyCorr_ts_without, AvgPhyCorr_ts_without, file="PhyloCorr/PhyloCorr_ts_without.Rdata")
rm(ts_dat_without, PhyCorr_ts_without, AvgPhyCorr_ts_without) # clean workspace

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], pages_dat_without)

# Drop species not present on the tree
pages_dat_without <- pages_dat_without[ ! pages_dat_without$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_pages<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_pages[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_pages[[1]], pages_dat_without); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_pages_without<-foreach(i = 1:100, 
                         .export = 'rbind',
                         .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                           
                           # Select one trimmed fully-sampled tree:
                           my_tree<-phylo_tree_pages[[i]]
                           
                           # Create a phylo4 object including GLMM model residuals:
                           phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=pages_dat_without$pag_resids_without))
                           
                           # Compute the phylogenetic correlogram:
                           phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                  dist.phylo="patristic", n.points=14, ci.bs=100)
                           
                           correlogram_data<-as.data.frame(phy.cor[[1]])
                           names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                           correlogram_data$Iter<-i
                           correlogram_data$N_class<-1:14
                           correlogram_data
                           
                         }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_pages_without<-as.data.table(rbindlist(PhyCorr_pages_without))
AvgPhyCorr_pages_without<-PhyCorr_pages_without[, .(Distance=mean(dist.class, na.rm=T),
                                    Lower_CI=mean(lower_ci, na.rm=T),
                                    Upper_CI=mean(upper_ci, na.rm=T),
                                    MoranI_coef=mean(coef, na.rm=T)),
                                by = .(N_class)]

# Export the results:
save(PhyCorr_pages_without, AvgPhyCorr_pages_without, file="PhyloCorr/PhyloCorr_pages_without.Rdata")
rm(pages_dat_without, PhyCorr_pages_without, AvgPhyCorr_pages_without) # clean workspace


#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], tcom_dat_without)

# Drop species not present on the tree
tcom_dat_without <- tcom_dat_without[ ! tcom_dat_without$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_tcom<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_tcom[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_tcom[[1]], tcom_dat_without); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_tcom_without<-foreach(i = 1:100, 
                        .export = 'rbind',
                        .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                          
                          # Select one trimmed fully-sampled tree:
                          my_tree<-phylo_tree_tcom[[i]]
                          
                          # Create a phylo4 object including GLMM model residuals:
                          phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=tcom_dat_without$tcom_resids_without))
                          
                          # Compute the phylogenetic correlogram:
                          phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                 dist.phylo="patristic", n.points=14, ci.bs=100)
                          
                          correlogram_data<-as.data.frame(phy.cor[[1]])
                          names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                          correlogram_data$Iter<-i
                          correlogram_data$N_class<-1:14
                          correlogram_data
                          
                        }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_tcom_without<-as.data.table(rbindlist(PhyCorr_tcom_without))
AvgPhyCorr_tcom_without<-PhyCorr_tcom_without[, .(Distance=mean(dist.class, na.rm=T),
                                  Lower_CI=mean(lower_ci, na.rm=T),
                                  Upper_CI=mean(upper_ci, na.rm=T),
                                  MoranI_coef=mean(coef, na.rm=T)),
                              by = .(N_class)]

# Export the results:
save(PhyCorr_tcom_without, AvgPhyCorr_tcom_without, file="PhyloCorr/PhyloCorr_tcom_without.Rdata")
rm(tcom_dat_without, PhyCorr_tcom_without, AvgPhyCorr_tcom_without) # clean workspace

## Bats ----

#------------------------------------------------------------#
# Number of evidence I
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], evi_datI_bats)

# Drop species not present on the tree
evi_datI_bats <- evi_datI_bats[ ! evi_datI_bats$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_evi<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_evi[[1]], evi_datI_bats); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK') # selecting half of all available cores
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_evi_I_bats<-foreach(i = 1:100, 
                              .export = 'rbind',
                              .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                
                                # Select one trimmed fully-sampled tree:
                                my_tree<-phylo_tree_evi[[i]]
                                
                                # Create a phylo4 object including GLMM model residuals:
                                phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datI_bats$evi_residsI_bats))
                                
                                # Compute the phylogenetic correlogram:
                                phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                       dist.phylo="patristic", n.points=14, ci.bs=100)
                                
                                correlogram_data<-as.data.frame(phy.cor[[1]])
                                names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                correlogram_data$Iter<-i
                                correlogram_data$N_class<-1:14
                                correlogram_data
                                
                              }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_evi_I_bats<-as.data.table(rbindlist(PhyCorr_evi_I_bats))
AvgPhyCorr_evi_I_bats<-PhyCorr_evi_I_bats[, .(Distance=mean(dist.class, na.rm=T),
                                              Lower_CI=mean(lower_ci, na.rm=T),
                                              Upper_CI=mean(upper_ci, na.rm=T),
                                              MoranI_coef=mean(coef, na.rm=T)),
                                          by = .(N_class)]

# Export the results:
save(PhyCorr_evi_I_bats, AvgPhyCorr_evi_I_bats, file="PhyloCorr/PhyloCorr_evi_I_bats.Rdata")
rm(evi_datI_bats, PhyCorr_evi_I_bats, AvgPhyCorr_evi_I_bats) # clean workspace

#------------------------------------------------------------#
# Number of evidence II
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], evi_datII_bats)

# Drop species not present on the tree
evi_datII_bats <- evi_datII_bats[ ! evi_datII_bats$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_evi<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_evi[[1]], evi_datII_bats); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_evi_II_bats<-foreach(i = 1:100, 
                               .export = 'rbind',
                               .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                 
                                 # Select one trimmed fully-sampled tree:
                                 my_tree<-phylo_tree_evi[[i]]
                                 
                                 # Create a phylo4 object including GLMM model residuals:
                                 phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datII_bats$evi_residsII_bats))
                                 
                                 # Compute the phylogenetic correlogram:
                                 phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                        dist.phylo="patristic", n.points=14, ci.bs=100)
                                 
                                 correlogram_data<-as.data.frame(phy.cor[[1]])
                                 names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                 correlogram_data$Iter<-i
                                 correlogram_data$N_class<-1:14
                                 correlogram_data
                                 
                               }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_evi_II_bats<-as.data.table(rbindlist(PhyCorr_evi_II_bats))
AvgPhyCorr_evi_II_bats<-PhyCorr_evi_II_bats[, .(Distance=mean(dist.class, na.rm=T),
                                                Lower_CI=mean(lower_ci, na.rm=T),
                                                Upper_CI=mean(upper_ci, na.rm=T),
                                                MoranI_coef=mean(coef, na.rm=T)),
                                            by = .(N_class)]

# Export the results:
save(PhyCorr_evi_II_bats, AvgPhyCorr_evi_II_bats, file="PhyloCorr/PhyloCorr_evi_II_bats.Rdata")
rm(evi_datII_bats, PhyCorr_evi_II_bats, AvgPhyCorr_evi_II_bats) # clean workspace

#------------------------------------------------------------#
# Number of specimens
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], ts_dat_bats)

# Drop species not present on the tree
ts_dat_bats <- ts_dat_bats[ ! ts_dat_bats$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_ts<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_ts[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_ts[[1]], ts_dat_bats); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_ts_bats<-foreach(i = 1:100, 
                           .export = 'rbind',
                           .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                             
                             # Select one trimmed fully-sampled tree:
                             my_tree<-phylo_tree_ts[[i]]
                             
                             # Create a phylo4 object including GLMM model residuals:
                             phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=ts_dat_bats$ts_resids_bats))
                             
                             # Compute the phylogenetic correlogram:
                             phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                    dist.phylo="patristic", n.points=14, ci.bs=100)
                             
                             correlogram_data<-as.data.frame(phy.cor[[1]])
                             names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                             correlogram_data$Iter<-i
                             correlogram_data$N_class<-1:14
                             correlogram_data
                             
                           }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_ts_bats<-as.data.table(rbindlist(PhyCorr_ts_bats))
AvgPhyCorr_ts_bats<-PhyCorr_ts_bats[, .(Distance=mean(dist.class, na.rm=T),
                                        Lower_CI=mean(lower_ci, na.rm=T),
                                        Upper_CI=mean(upper_ci, na.rm=T),
                                        MoranI_coef=mean(coef, na.rm=T)),
                                    by = .(N_class)]

# Export the results:
save(PhyCorr_ts_bats, AvgPhyCorr_ts_bats, file="PhyloCorr/PhyloCorr_ts_bats.Rdata")
rm(ts_dat_bats, PhyCorr_ts_bats, AvgPhyCorr_ts_bats) # clean workspace

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], pages_dat_bats)

# Drop species not present on the tree
pages_dat_bats <- pages_dat_bats[ ! pages_dat_bats$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_pages<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_pages[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_pages[[1]], pages_dat_bats); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_pages_bats<-foreach(i = 1:100, 
                              .export = 'rbind',
                              .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                
                                # Select one trimmed fully-sampled tree:
                                my_tree<-phylo_tree_pages[[i]]
                                
                                # Create a phylo4 object including GLMM model residuals:
                                phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=pages_dat_bats$pag_resids_bats))
                                
                                # Compute the phylogenetic correlogram:
                                phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                       dist.phylo="patristic", n.points=14, ci.bs=100)
                                
                                correlogram_data<-as.data.frame(phy.cor[[1]])
                                names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                correlogram_data$Iter<-i
                                correlogram_data$N_class<-1:14
                                correlogram_data
                                
                              }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_pages_bats<-as.data.table(rbindlist(PhyCorr_pages_bats))
AvgPhyCorr_pages_bats<-PhyCorr_pages_bats[, .(Distance=mean(dist.class, na.rm=T),
                                              Lower_CI=mean(lower_ci, na.rm=T),
                                              Upper_CI=mean(upper_ci, na.rm=T),
                                              MoranI_coef=mean(coef, na.rm=T)),
                                          by = .(N_class)]

# Export the results:
save(PhyCorr_pages_bats, AvgPhyCorr_pages_bats, file="PhyloCorr/PhyloCorr_pages_bats.Rdata")
rm(pages_dat_bats, PhyCorr_pages_bats, AvgPhyCorr_pages_bats) # clean workspace


#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], tcom_dat_bats)

# Drop species not present on the tree
tcom_dat_bats <- tcom_dat_bats[ ! tcom_dat_bats$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_tcom<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_tcom[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_tcom[[1]], tcom_dat_bats); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_tcom_bats<-foreach(i = 1:100, 
                             .export = 'rbind',
                             .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                               
                               # Select one trimmed fully-sampled tree:
                               my_tree<-phylo_tree_tcom[[i]]
                               
                               # Create a phylo4 object including GLMM model residuals:
                               phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=tcom_dat_bats$tcom_resids_bats))
                               
                               # Compute the phylogenetic correlogram:
                               phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                      dist.phylo="patristic", n.points=14, ci.bs=100)
                               
                               correlogram_data<-as.data.frame(phy.cor[[1]])
                               names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                               correlogram_data$Iter<-i
                               correlogram_data$N_class<-1:14
                               correlogram_data
                               
                             }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_tcom_bats<-as.data.table(rbindlist(PhyCorr_tcom_bats))
AvgPhyCorr_tcom_bats<-PhyCorr_tcom_bats[, .(Distance=mean(dist.class, na.rm=T),
                                            Lower_CI=mean(lower_ci, na.rm=T),
                                            Upper_CI=mean(upper_ci, na.rm=T),
                                            MoranI_coef=mean(coef, na.rm=T)),
                                        by = .(N_class)]

# Export the results:
save(PhyCorr_tcom_bats, AvgPhyCorr_tcom_bats, file="PhyloCorr/PhyloCorr_tcom_bats.Rdata")
rm(tcom_dat_bats, PhyCorr_tcom_bats, AvgPhyCorr_tcom_bats) # clean workspace



## Rodents ----

#------------------------------------------------------------#
# Number of evidence I
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], evi_datI_rodents)

# Drop species not present on the tree
evi_datI_rodents <- evi_datI_rodents[ ! evi_datI_rodents$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_evi<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_evi[[1]], evi_datI_rodents); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK') # selecting half of all available cores
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_evi_I_rodents<-foreach(i = 1:100, 
                                 .export = 'rbind',
                                 .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                   
                                   # Select one trimmed fully-sampled tree:
                                   my_tree<-phylo_tree_evi[[i]]
                                   
                                   # Create a phylo4 object including GLMM model residuals:
                                   phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datI_rodents$evi_residsI_rodents))
                                   
                                   # Compute the phylogenetic correlogram:
                                   phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                          dist.phylo="patristic", n.points=14, ci.bs=100)
                                   
                                   correlogram_data<-as.data.frame(phy.cor[[1]])
                                   names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                   correlogram_data$Iter<-i
                                   correlogram_data$N_class<-1:14
                                   correlogram_data
                                   
                                 }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_evi_I_rodents<-as.data.table(rbindlist(PhyCorr_evi_I_rodents))
AvgPhyCorr_evi_I_rodents<-PhyCorr_evi_I_rodents[, .(Distance=mean(dist.class, na.rm=T),
                                                    Lower_CI=mean(lower_ci, na.rm=T),
                                                    Upper_CI=mean(upper_ci, na.rm=T),
                                                    MoranI_coef=mean(coef, na.rm=T)),
                                                by = .(N_class)]

# Export the results:
save(PhyCorr_evi_I_rodents, AvgPhyCorr_evi_I_rodents, file="PhyloCorr/PhyloCorr_evi_I_rodents.Rdata")
rm(evi_datI_rodents, PhyCorr_evi_I_rodents, AvgPhyCorr_evi_I_rodents) # clean workspace

#------------------------------------------------------------#
# Number of evidence II
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], evi_datII_rodents)

# Drop species not present on the tree
evi_datII_rodents <- evi_datII_rodents[ ! evi_datII_rodents$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_evi<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_evi[[1]], evi_datII_rodents); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_evi_II_rodents<-foreach(i = 1:100, 
                                  .export = 'rbind',
                                  .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                    
                                    # Select one trimmed fully-sampled tree:
                                    my_tree<-phylo_tree_evi[[i]]
                                    
                                    # Create a phylo4 object including GLMM model residuals:
                                    phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datII_rodents$evi_residsII_rodents))
                                    
                                    # Compute the phylogenetic correlogram:
                                    phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                           dist.phylo="patristic", n.points=14, ci.bs=100)
                                    
                                    correlogram_data<-as.data.frame(phy.cor[[1]])
                                    names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                    correlogram_data$Iter<-i
                                    correlogram_data$N_class<-1:14
                                    correlogram_data
                                    
                                  }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_evi_II_rodents<-as.data.table(rbindlist(PhyCorr_evi_II_rodents))
AvgPhyCorr_evi_II_rodents<-PhyCorr_evi_II_rodents[, .(Distance=mean(dist.class, na.rm=T),
                                                      Lower_CI=mean(lower_ci, na.rm=T),
                                                      Upper_CI=mean(upper_ci, na.rm=T),
                                                      MoranI_coef=mean(coef, na.rm=T)),
                                                  by = .(N_class)]

# Export the results:
save(PhyCorr_evi_II_rodents, AvgPhyCorr_evi_II_rodents, file="PhyloCorr/PhyloCorr_evi_II_rodents.Rdata")
rm(evi_datII_rodents, PhyCorr_evi_II_rodents, AvgPhyCorr_evi_II_rodents) # clean workspace

#------------------------------------------------------------#
# Number of specimens
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], ts_dat_rodents)

# Drop species not present on the tree
ts_dat_rodents <- ts_dat_rodents[ ! ts_dat_rodents$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_ts<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_ts[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_ts[[1]], ts_dat_rodents); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_ts_rodents<-foreach(i = 1:100, 
                              .export = 'rbind',
                              .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                
                                # Select one trimmed fully-sampled tree:
                                my_tree<-phylo_tree_ts[[i]]
                                
                                # Create a phylo4 object including GLMM model residuals:
                                phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=ts_dat_rodents$ts_resids_rodents))
                                
                                # Compute the phylogenetic correlogram:
                                phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                       dist.phylo="patristic", n.points=14, ci.bs=100)
                                
                                correlogram_data<-as.data.frame(phy.cor[[1]])
                                names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                correlogram_data$Iter<-i
                                correlogram_data$N_class<-1:14
                                correlogram_data
                                
                              }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_ts_rodents<-as.data.table(rbindlist(PhyCorr_ts_rodents))
AvgPhyCorr_ts_rodents<-PhyCorr_ts_rodents[, .(Distance=mean(dist.class, na.rm=T),
                                              Lower_CI=mean(lower_ci, na.rm=T),
                                              Upper_CI=mean(upper_ci, na.rm=T),
                                              MoranI_coef=mean(coef, na.rm=T)),
                                          by = .(N_class)]

# Export the results:
save(PhyCorr_ts_rodents, AvgPhyCorr_ts_rodents, file="PhyloCorr/PhyloCorr_ts_rodents.Rdata")
rm(ts_dat_rodents, PhyCorr_ts_rodents, AvgPhyCorr_ts_rodents) # clean workspace

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], pages_dat_rodents)

# Drop species not present on the tree
pages_dat_rodents <- pages_dat_rodents[ ! pages_dat_rodents$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_pages<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_pages[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_pages[[1]], pages_dat_rodents); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_pages_rodents<-foreach(i = 1:100, 
                                 .export = 'rbind',
                                 .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                   
                                   # Select one trimmed fully-sampled tree:
                                   my_tree<-phylo_tree_pages[[i]]
                                   
                                   # Create a phylo4 object including GLMM model residuals:
                                   phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=pages_dat_rodents$pag_resids_rodents))
                                   
                                   # Compute the phylogenetic correlogram:
                                   phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                          dist.phylo="patristic", n.points=14, ci.bs=100)
                                   
                                   correlogram_data<-as.data.frame(phy.cor[[1]])
                                   names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                   correlogram_data$Iter<-i
                                   correlogram_data$N_class<-1:14
                                   correlogram_data
                                   
                                 }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_pages_rodents<-as.data.table(rbindlist(PhyCorr_pages_rodents))
AvgPhyCorr_pages_rodents<-PhyCorr_pages_rodents[, .(Distance=mean(dist.class, na.rm=T),
                                                    Lower_CI=mean(lower_ci, na.rm=T),
                                                    Upper_CI=mean(upper_ci, na.rm=T),
                                                    MoranI_coef=mean(coef, na.rm=T)),
                                                by = .(N_class)]

# Export the results:
save(PhyCorr_pages_rodents, AvgPhyCorr_pages_rodents, file="PhyloCorr/PhyloCorr_pages_rodents.Rdata")
rm(pages_dat_rodents, PhyCorr_pages_rodents, AvgPhyCorr_pages_rodents) # clean workspace


#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#

# Check if we have the same spp in our data as in the tree
obj <- geiger::name.check(mammal_tree[[1]], tcom_dat_rodents)

# Drop species not present on the tree
tcom_dat_rodents <- tcom_dat_rodents[ ! tcom_dat_rodents$SpeciesName %in% obj$data_not_tree , ]

# Remove species present on the tree but not on the dataset
phylo_tree_tcom<-list()

# Remove spp that are in the phylogeny but not in the dataset
for (i in 1:100) {
  phylo_tree_tcom[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
} 
name.check(phylo_tree_tcom[[1]], tcom_dat_rodents); rm(obj) # OK = all species on phylogeny matching those on the data frame

# Prepare workspace for parallel computing:
cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK')
registerDoParallel(cl)
getDoParWorkers()

{
  PhyCorr_tcom_rodents<-foreach(i = 1:100, 
                                .export = 'rbind',
                                .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
                                  
                                  # Select one trimmed fully-sampled tree:
                                  my_tree<-phylo_tree_tcom[[i]]
                                  
                                  # Create a phylo4 object including GLMM model residuals:
                                  phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=tcom_dat_rodents$tcom_resids_rodents))
                                  
                                  # Compute the phylogenetic correlogram:
                                  phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
                                                                         dist.phylo="patristic", n.points=14, ci.bs=100)
                                  
                                  correlogram_data<-as.data.frame(phy.cor[[1]])
                                  names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
                                  correlogram_data$Iter<-i
                                  correlogram_data$N_class<-1:14
                                  correlogram_data
                                  
                                }
  stopCluster(cl) # terminate cluster
}

# Extract the average correlogram output across iterations:
PhyCorr_tcom_rodents<-as.data.table(rbindlist(PhyCorr_tcom_rodents))
AvgPhyCorr_tcom_rodents<-PhyCorr_tcom_rodents[, .(Distance=mean(dist.class, na.rm=T),
                                                  Lower_CI=mean(lower_ci, na.rm=T),
                                                  Upper_CI=mean(upper_ci, na.rm=T),
                                                  MoranI_coef=mean(coef, na.rm=T)),
                                              by = .(N_class)]

# Export the results:
save(PhyCorr_tcom_rodents, AvgPhyCorr_tcom_rodents, file="PhyloCorr/PhyloCorr_tcom_rodents.Rdata")
rm(tcom_dat_rodents, PhyCorr_tcom_rodents, AvgPhyCorr_tcom_rodents) # clean workspace

# 8) Make phylogenetic correlograms.----

# All mammals
#load('PhyloCorr/PhyloCorr_evi_I.Rdata')
load('PhyloCorr/PhyloCorr_evi_II.Rdata')
load('PhyloCorr/PhyloCorr_ts.Rdata')
load('PhyloCorr/PhyloCorr_pages.Rdata')
load('PhyloCorr/PhyloCorr_tcom.Rdata')
# Non-bats & non-rodents
#load('PhyloCorr/PhyloCorr_evi_I_without.Rdata')
load('PhyloCorr/PhyloCorr_evi_II_without.Rdata')
load('PhyloCorr/PhyloCorr_ts_without.Rdata')
load('PhyloCorr/PhyloCorr_pages_without.Rdata')
load('PhyloCorr/PhyloCorr_tcom_without.Rdata')
# Bats
#load('PhyloCorr/PhyloCorr_evi_I_bats.Rdata')
load('PhyloCorr/PhyloCorr_evi_II_bats.Rdata')
load('PhyloCorr/PhyloCorr_ts_bats.Rdata')
load('PhyloCorr/PhyloCorr_pages_bats.Rdata')
load('PhyloCorr/PhyloCorr_tcom_bats.Rdata')
# Rodents
#load('PhyloCorr/PhyloCorr_evi_I_rodents.Rdata')
load('PhyloCorr/PhyloCorr_evi_II_rodents.Rdata')
load('PhyloCorr/PhyloCorr_ts_rodents.Rdata')
load('PhyloCorr/PhyloCorr_pages_rodents.Rdata')
load('PhyloCorr/PhyloCorr_tcom_rodents.Rdata')

# Combine and create column to differentiate responses
# Run one at a time
# All mammals
Corr_list <- list(AvgPhyCorr_evi_II, AvgPhyCorr_ts, AvgPhyCorr_pages, AvgPhyCorr_tcom)
# Non-bats & non-rodents
Corr_list <- list(AvgPhyCorr_evi_II_without, AvgPhyCorr_ts_without,
                  AvgPhyCorr_pages_without, AvgPhyCorr_tcom_without)
# Bats
Corr_list <- list(AvgPhyCorr_evi_II_bats, AvgPhyCorr_ts_bats,
                  AvgPhyCorr_pages_bats, AvgPhyCorr_tcom_bats)
# Rodents
Corr_list <- list(AvgPhyCorr_evi_II_rodents, AvgPhyCorr_ts_rodents,
                  AvgPhyCorr_pages_rodents, AvgPhyCorr_tcom_rodents)

# Create a vector to add a new column informing the region in the datasets
response <- c('N. evidence II', 'N. preserved\nspecimens', 'N. pages', 'N. taxa\ncompared')

for (i in seq_along(Corr_list)) {
  Corr_list[[i]]$Response <- response[i]
}

Corr_list <- rbindlist(Corr_list) # convert to dataframe

# Make the plot
MyCorrelogram <- ggplot(Corr_list, aes(x = Distance, y = MoranI_coef, colour = Response)) +
  geom_point(aes(shape = Response, colour = Response))+
  geom_linerange(aes(ymin = Lower_CI, ymax = Upper_CI))+
  geom_line()+
  geom_hline(yintercept=0, linetype="dashed", color="black") +
  ylim(c(-0.25, 0.25)) +
  ylab("Moran's I - GLM residuals") +
  xlab("Phylogenetic distance (mya)") +
  theme(panel.grid.minor = element_blank(), # remove minor gridlines
        panel.grid.major = element_blank(), # remove major gridlines
        panel.background = element_blank(), # white background
        axis.line = element_line(colour="black"), # axis lines aesthetitcs
        axis.text.y = element_text(hjust=0.5, vjust=0.5, angle=0, size=6),
        axis.text.x = element_text(hjust=0.5, vjust=0.5, angle=0, size=6),
        axis.ticks.y=element_blank(),
        axis.title.y=element_text(size=8, colour="black", face="bold", margin=margin(t=0, r=5, b=0, l=0)), # margin between axis.title and axis.values
        axis.title.x=element_text(size=8, colour="black", face="bold", margin=margin(t=5, r=0, b=0, l=0)), # margin between axis.title and axis.values
        legend.position=c(.6,.8),
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.background=element_blank(),
        panel.spacing=unit(0,"null")); MyCorrelogram

# Save to disk
ggsave(paste0(getwd(), "/figures/FigureS2.PhyloCorrelogram_rodents.pdf"),
       plot=MyCorrelogram, width=5, height=4, units="in", bg = 'transparent', dpi = "print")
ggsave(paste0(getwd(), "/figures/FigureS2.PhyloCorrelogram_rodents.jpg"),
       plot=MyCorrelogram, width=5, height=4, units="in", bg = 'white', dpi = "print")

rm(list = ls()); gc() # clean workspace and garbage collection

# 9) Explore temporal trends in the use of molecular data on Mammal description.----
# Load dataset
#mydata <- fread("Dataset.csv", na.strings = '')
load("Dataset.Rdata")
names(data)
dim(data)

mydata <- data
summary(mydata$Molecular) # 30 NAs
table(mydata$Molecular)

mydata <- mydata %>%
  filter(!is.na(Molecular)) %>%
  mutate(MolMethod = na_if(MolMethod, ""),
         MolMethod = case_when(
           MolMethod == "Allozyme" ~ "Allozymes",  # padroniza para "Allozymes"
           TRUE ~ MolMethod  
         )) %>% 
  mutate(TaxonomicReview = replace_na(TaxonomicReview, 0))

# Ordenando para ter a mesma cor em todos os graficos bar plot
mol_levels <- mydata %>%
  filter(!is.na(MolMethod)) %>%
  distinct(MolMethod) %>%
  pull(MolMethod) %>%
  sort()
levels(as.factor(mydata$MolMethod)) # 13 levels

# Transforme em fator no próprio dataframe
mydata$MolMethod <- as.character(mydata$MolMethod)
mydata$MolMethod[is.na(mydata$MolMethod)] <- "NA"
mydata$MolMethod <- factor(mydata$MolMethod, levels = mol_levels)

# Exemplo com 12 cores + 1 cinza (ajuste conforme o número de níveis)
extended_colors <- c(brewer.pal(n = length(mol_levels) - 1, name = "Paired"), "#D3D3D3")
names(extended_colors) <- mol_levels

all_mammals <- mydata %>%
  group_by(Year, MolMethod) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = MolMethod)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "N. of species", fill = "Molecular Methods") +
  scale_x_discrete(breaks = seq(1990, 2023, 5), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  # scale_fill_hue() +  
  scale_fill_manual(values = extended_colors) +  # Use "Paired" colors and grey for NAs
  theme_classic() +
  theme(axis.title = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(size = 6),            # Reduce legend title size
        legend.text = element_text(size = 5),             # Reduce legend text size
        legend.key.size = unit(0.5, "lines"),             # Reduce the size of the legend keys
        legend.spacing = unit(0.5, "lines"),              # Reduce the spacing between legend items
        legend.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),  # Reduce margin around the legend
        legend.position = 'bottom'); all_mammals

without_molecular <- mydata %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  group_by(Year, MolMethod) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = MolMethod)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "", fill = "Molecular Methods") +
  scale_x_discrete(breaks = seq(1990, 2023, 5), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  # scale_fill_hue() +  
  scale_fill_manual(values = extended_colors) +  # Use "Paired" colors and grey for NAs
  theme_classic() +
  theme(axis.title = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(size = 6),            # Reduce legend title size
        legend.text = element_text(size = 5),             # Reduce legend text size
        legend.key.size = unit(0.5, "lines"),             # Reduce the size of the legend keys
        legend.spacing = unit(0.5, "lines"),              # Reduce the spacing between legend items
        legend.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),  # Reduce margin around the legend
        legend.position = 'none'); without_molecular

bats_molecular <- mydata %>%
  filter(Order == "Chiroptera") %>%
  group_by(Year, MolMethod) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = MolMethod)) +
  geom_bar(stat = "identity") +
  labs(x = "Year of description", y = "N. of species", fill = "Molecular Methods") +
  scale_x_discrete(breaks = seq(1990, 2023, 5), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  # scale_fill_hue() +  
  scale_fill_manual(values = extended_colors) +  # Use "Paired" colors and grey for NAs
  theme_classic() +
  theme(axis.title = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(size = 6),            # Reduce legend title size
        legend.text = element_text(size = 5),             # Reduce legend text size
        legend.key.size = unit(0.5, "lines"),             # Reduce the size of the legend keys
        legend.spacing = unit(0.5, "lines"),              # Reduce the spacing between legend items
        legend.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),  # Reduce margin around the legend
        legend.position = 'none'); bats_molecular

rodents_molecular <- mydata %>%
  filter(Order == "Rodentia") %>%
  group_by(Year, MolMethod) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = MolMethod)) +
  geom_bar(stat = "identity") +
  labs(x = "Year of description", y = "", fill = "Molecular Methods") +
  scale_x_discrete(breaks = seq(1990, 2023, 5), expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  # scale_fill_hue() +  
  scale_fill_manual(values = extended_colors) +  # Use "Paired" colors and grey for NAs
  theme_classic() +
  theme(axis.title = element_text(face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(size = 6),            # Reduce legend title size
        legend.text = element_text(size = 5),             # Reduce legend text size
        legend.key.size = unit(0.5, "lines"),             # Reduce the size of the legend keys
        legend.spacing = unit(0.5, "lines"),              # Reduce the spacing between legend items
        legend.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "lines"),  # Reduce margin around the legend
        legend.position = 'none'); rodents_molecular

fig <- cowplot::plot_grid(all_mammals, without_molecular, bats_molecular, rodents_molecular,
                          ncol = 2, nrow = 2, align = "v", labels = "auto"); fig

ggsave(paste0(getwd(), "/figures/FigureS3.MolecularMethods.pdf"), 
       plot=fig, width=9, height=6, units="in", dpi = "print", cairo_pdf())

# Obtain proportion
df_prop <- mydata %>%
  filter(!is.na(Molecular)) %>%
  group_by(Year, Molecular) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

df_prop_without <- mydata %>%
  filter(!is.na(Molecular)) %>%
  filter(Order != "Rodentia" & Order != "Chiroptera") %>%
  group_by(Year, Molecular) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

df_prop_bats <- mydata %>%
  filter(!is.na(Molecular)) %>%
  filter(Order == "Chiroptera") %>%
  group_by(Year, Molecular) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

df_prop_rodents <- mydata %>%
  filter(!is.na(Molecular)) %>%
  filter(Order == "Rodentia") %>%
  group_by(Year, Molecular) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

# all mammals 
inset <- df_prop %>%
  filter(Molecular == 1) %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") + 
  labs(x = NULL, y = "Prop. spp. described\nwith molecular") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2022, by = 4))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 7),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)); inset

# non-bats & non-rodents
inset_without <- df_prop_without %>%
  filter(Molecular == 1) %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "#ff3352") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#ff3352") + 
  labs(x = NULL, y = "") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2022, by = 4))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 7),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)); inset_without

# bats
inset_bats <- df_prop_bats %>%
  filter(Molecular == 1) %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "#7fc97f") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#7fc97f") + 
  labs(x = NULL, y = "Prop. spp. described\nwith molecular") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2022, by = 4))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 7),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)); inset_bats

# rodents
inset_rodents <- df_prop_rodents %>%
  filter(Molecular == 1) %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "#386cb0") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.0, color = "#386cb0") + 
  labs(x = NULL, y = "") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2022, by = 4))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 7),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)); inset_rodents

fig <- cowplot::plot_grid(inset, inset_without, inset_bats, inset_rodents,
                   ncol = 2, nrow = 2, align = "v", labels = "auto"); fig
# p_grob <- ggplotGrob(p) # convert the main plot to a grob to retain the formatting

# Create the final plot using ggdraw and draw_plot
#final_plot <- ggdraw() +
#  draw_grob(p_grob) +  # use draw_grob to add the main plot
#  draw_plot(inset, .08, .62, width = .3, height = .3); final_plot # add the inset plot

# Save the plot
ggsave(paste0(getwd(), "/figures/Figure3.MolecularMethods.pdf"), plot=fig, 
       width=7, height=5, units="in", dpi = "print", cairo_pdf())

### Plot the proportion of species described with molecular data per taxa ###
levels(as.factor(mydata$Molecular))
mydata$Molecular <- ifelse(mydata$Molecular==1, yes = 'yes', no = 'no')

levels(as.factor(mydata$TaxonomicReview))
mydata$TaxonomicReview <- ifelse(mydata$TaxonomicReview==1, yes = 'yes', no = 'no')

# Get basic statists
prop_mol <- mydata %>% 
  group_by(Order) %>%
  count(Molecular) %>%
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too

mol_per_family <- mydata %>% 
  group_by(Order, Family) %>%
  count(Molecular) %>% 
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too

prop_rev <- mydata %>% 
  group_by(Order) %>%
  count(TaxonomicReview) %>% 
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too

# Save table
fwrite(mol_per_family, "tables/TableS1.prop_molec_per_fam.csv"); rm(mol_per_family)

# Prepare for plotting:
# Order the bars according to the proportion of spp. 
# described with molecular analysis
load("Dataset.Rdata")
mydata <- data

# Spp countries
names(mydata)
nrow(mydata)
nrow(SppCountries)

SppCountries <- mydata %>%
  mutate(
    Molecular_cat = case_when(
      Molecular == 0 ~ "no",
      Molecular == 1 & N.Countries == 1 ~ "One country",
      Molecular == 1 & N.Countries == 2 ~ "Two countries",
      Molecular == 1 & N.Countries >= 3 ~ "Three or more countries"
    )) %>%
  mutate(Molecular_cat = factor(Molecular_cat,
                                levels = c(
                                  "no",
                                  "Three or more countries",
                                  "Two countries",
                                  "One country")))
# Spp richness
SppRichness <- mydata %>%
  group_by(Order) %>%
  summarise(nTot = n()) 

p <- SppCountries %>%
  drop_na(N.Countries) %>%
  left_join(SppRichness, by = "Order") %>%
  mutate(Order = paste0(Order, "\n(n=", nTot, ")"),
         Order = fct_reorder(Order, Molecular == 1, .fun = mean,
                             .na_rm = TRUE, .desc = FALSE)) %>%
  ggplot(aes(x = Order, fill = Molecular_cat)) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey50") +
  coord_flip() +
  scale_fill_manual(values = c(
    "One country" = "#c7c7c7",
    "Two countries" = "#919190",
    "Three or more countries" = "#4d4d4d",
    "no" = "white"
  )) +
  scale_y_continuous(breaks = seq(0, 1, .25), expand = expansion(mult = c(0, .1))) +
  labs(y = "Proportion of species described\nwith molecular data", x = "Taxonomic order") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_blank(),
        legend.position = "none"); p 
  
  # Add text displaying total number of species per family
  #geom_text(data = SppRichness,
  #          aes(x = Order, y = 1.03, label = nTot),
  #          size = 2.5, angle = 0, color = "black", hjust = 0.5, vjust = 0.5); p 

SppCountriesTaxonomic <- mydata %>%
  mutate(
    Taxonomic_cat = case_when(
      TaxonomicReview == 0 ~ "no",
      TaxonomicReview == 1 & N.Countries == 1 ~ "One country",
      TaxonomicReview == 1 & N.Countries == 2 ~ "Two countries",
      TaxonomicReview == 1 & N.Countries >= 3 ~ "Three or more countries"
    )) %>%
  mutate(Taxonomic_cat = factor(Taxonomic_cat,
                                levels = c(
                                  "no",
                                  "Three or more countries",
                                  "Two countries",
                                  "One country")))

m <- SppCountriesTaxonomic %>%
  drop_na(TaxonomicReview) %>%
  # convert variable to factor, ordered (descending) by the proportion of rows where order == "no"
  left_join(SppRichness, by = "Order") %>%
  mutate(Order = paste0(Order, "\n(n=", nTot, ")"),
         # convert variable to factor, ordered (descending) by the proportion of rows where order == "no"
         Order = fct_reorder(Order, TaxonomicReview == 1, .fun = mean,
                             .na_rm = TRUE, .desc = FALSE)) %>%
  ggplot(aes(x = Order, fill = Taxonomic_cat)) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = .5, linetype = "dashed", color = "grey50") +
  coord_flip()+
  # set bar colours per order
  scale_fill_manual(values = c(
    "One country" = "#c7c7c7",
    "Two countries" = "#919190",
    "Three or more countries" = "#4d4d4d",
    "no" = "white"
  )) +
  scale_y_continuous(breaks = seq(0, 1, .25), expand = expansion(mult = c(0, .1))) +
  # define axis titles
  labs(y = "Proportion of species described\nwith taxonomic review", x = "") +
  # apply themes
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10, face = "bold"),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 8, colour = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.position = "none"); m

# Arrange plots in a grid
fig <- ggpubr::ggarrange(p, m,
                         ncol = 2, nrow = 1, 
                         labels = "auto",
                         font.label = list(size = 12, color = "black"),
                         align = "hv"); fig

# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure4.PropMolecularByTaxa.pdf"),
       plot=fig, width=11, height=6, units="in", dpi = "print", cairo_pdf)

#ggsave(paste0(getwd(), "/figures/Figure4.PropMolecularByTaxa.jpg"),
#       plot=fig, width=11, height=6, units="in", dpi = "print")

#ggsave(paste0(getwd(), "/figures/Figure4.PropMolecularByTaxa.tiff"), 
#       plot=fig, width=11, height=6, units="in", dpi = "print")

rm(list = ls()); gc()

# 10) Relationship international description vs. molecular & taxonomic review.----
load("Dataset.Rdata")

mydata <- data
summary(mydata$Molecular) # 30 NAs
table(mydata$Molecular)

mydata <- mydata %>%
  filter(!is.na(Molecular)) %>%
  mutate(MolMethod = na_if(MolMethod, ""),
         MolMethod = case_when(
           MolMethod == "Allozyme" ~ "Allozymes",  # padroniza para "Allozymes"
           TRUE ~ MolMethod  
         )) %>% 
  mutate(TaxonomicReview = replace_na(TaxonomicReview, 0))

mydata <- mydata %>%
  mutate(TypeOfStudy = paste(TaxonomicReview, Molecular, sep = "_")) %>%
  mutate(TypeOfStudy = case_when(
    TypeOfStudy == "0_0"  ~ "Other \nevidences",
    TypeOfStudy == "0_1" ~ "Molecular",
    TypeOfStudy == "1_1" ~ "Taxonomic \nReview \n+ Molecular",
    TypeOfStudy == "1_0" ~ "Taxonomic \nReview",
    TRUE ~ TypeOfStudy  # mantém o valor original caso não se enquadre em nenhum caso
  )) 

# Definir os pares para comparação (cada grupo comparado entre si)
comparisons <- list(
  c("Taxonomic \nReview \n+ Molecular", "Molecular"),
  c("Taxonomic \nReview \n+ Molecular", "Taxonomic \nReview"),
  c("Taxonomic \nReview \n+ Molecular", "Other \nevidences"),
  c("Molecular", "Taxonomic \nReview"),
  c("Molecular", "Other \nevidences"),
  c("Taxonomic \nReview", "Other \nevidences")
)

table(mydata$TypeOfStudy)

# Pearson correlation between number of countries and number of authors
mydata_cor <- mydata %>% 
  filter(!is.na(N_authors) & !is.na(N.Countries))
cor.test(mydata_cor$N_authors, mydata_cor$N.Countries, method = "pearson")

# ANCOVA and post hoc test
# All mammals
res.aov <- mydata %>%
  anova_test(N.Countries ~ N_authors + TypeOfStudy)
get_anova_table(res.aov)

pwc <- mydata %>% 
  rstatix::emmeans_test(
    N.Countries ~ TypeOfStudy, covariate = N_authors,
    p.adjust.method = "bonferroni"
  )
pwc %>% View()

plot_ancova_mammals <- mydata %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color=TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("")

# Without bats & rodents
res.aov.without <- mydata %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  anova_test(N.Countries ~ N_authors + TypeOfStudy)
get_anova_table(res.aov.without)

pwc_without <- mydata %>% 
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  rstatix::emmeans_test(
    N.Countries ~ TypeOfStudy, covariate = N_authors,
    p.adjust.method = "bonferroni"
  )
pwc_without %>% View()

plot_ancova_without <- mydata %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color=TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("") + xlab("")

# Bats
res.aov.bats <- mydata %>%
  filter(Order == "Chiroptera") %>%
  anova_test(N.Countries ~ N_authors + TypeOfStudy)
get_anova_table(res.aov.bats)

pwc_bats <- mydata %>% 
  filter(Order == "Chiroptera") %>%
  rstatix::emmeans_test(
    N.Countries ~ TypeOfStudy, covariate = N_authors,
    p.adjust.method = "bonferroni"
  )
pwc_bats %>% View()

plot_ancova_bats <- mydata %>%
  filter(Order == "Chiroptera") %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color=TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "none")

# Rodents
res.aov.rodents <- mydata %>%
  filter(Order == "Rodentia") %>%
  anova_test(N.Countries ~ N_authors + TypeOfStudy)
get_anova_table(res.aov.rodents)

pwc_rodentia <- mydata %>% 
  filter(Order == "Rodentia") %>%
  rstatix::emmeans_test(
    N.Countries ~ TypeOfStudy, covariate = N_authors,
    p.adjust.method = "bonferroni"
  )
pwc_rodentia %>% View()

plot_ancova_rodents <- mydata %>%
  filter(Order == "Rodentia") %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color=TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "none")  +
  ylab("")

# Plot without legend
plot_grid <- plot_grid(plot_ancova_mammals, plot_ancova_without,
                       plot_ancova_bats, plot_ancova_rodents,
                       ncol = 2, nrow = 2, labels = "auto")

# extract legend
legend_plot <- mydata %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color = TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(legend.position = "bottom")

legenda <- get_legend(legend_plot)

# Plot with legend
plot_final <- plot_grid(
  plot_grid, legenda, ncol = 1, 
  rel_heights = c(1, 0.08)); plot_final

# export figure
ggsave(paste0(getwd(), "/figures/FigureS2.Scatterplot_nauthors_ncountries.pdf"),
       plot=plot_final, width=8, height=6, units="in", dpi = "print", cairo_pdf)

# Violinplot of each group
plot <- mydata %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Taxonomic \nReview \n+ Molecular",
                                         "Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = N.Countries)) +
  geom_violin(width = 0.8, fill = "black", alpha = 0.5, adjust = 1.5) +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  #stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.signif",
  #                   tip.length = 0.02, label.y = c(13.1, 14.4, 15.6, 16.9, 18.1, 18.4), size = 3) +  # Ajuste o size conforme necessário
  
  # Teste global (Kruskal-Wallis) um pouco acima
  #stat_compare_means(method = "kruskal.test", label.x = .75, label.y = 19, size = 3)+  # Ajuste o size conforme necessário
  # Adiciona as letras de significância com geom_text
  
  geom_text(aes(x = 1, y = 10, label = "ab"), size = 4) +  # "AB" em Taxonomic Review + Molecular
  geom_text(aes(x = 2, y = 14, label = "b"), size = 4) +   # "B" em Molecular
  geom_text(aes(x = 3, y = 8.0, label = "a"), size = 4) +   # "A" em Taxonomic Review
  geom_text(aes(x = 4, y = 7.0, label = "a", fontface = "plain"), size = 4) +   # "A" em Other evidences
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0,15), breaks = c(5, 10, 15), expand = expansion(add = c(0, .5))) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 18, face = "bold"), 
    legend.position = "none",
    #plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black")
  ) ; plot

# 
plot_mammals_without <- mydata %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Taxonomic \nReview \n+ Molecular",
                                         "Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = N.Countries)) +
  geom_violin(width = 0.8, fill = "#ff3352", alpha = 0.5, adjust = 1.5) +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  #stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.signif",
  #                   tip.length = 0.02, label.y = c(13.1, 14.4, 15.6, 16.9, 18.1, 18.4), size = 3) +  # Ajuste o size conforme necessário
  
  # Teste global (Kruskal-Wallis) um pouco acima
  #stat_compare_means(method = "kruskal.test", label.x = .75, label.y = 19, size = 3)+  # Ajuste o size conforme necessário
  geom_text(aes(x = 1, y = 6, label = "a"), size = 4) +  # "A" em Taxonomic Review + Molecular
  geom_text(aes(x = 2, y = 14, label = "b"), size = 4) +   # "B" em Molecular
  geom_text(aes(x = 3, y = 6, label = "b"), size = 4) +   # "B" em Taxonomic Review
  geom_text(aes(x = 4, y = 7, label = "b"), size = 4) +   # "B" em Other evidences
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 15), breaks = c(5, 10, 15), expand = expansion(add = c(0, .5))) +
  theme_minimal() +
  theme(
    plot.margin=unit(c(t = 0, r = 0, b = 0, l = 0), "cm"),
    #plot.margin = margin(5, 5, 5, 5, unit = "pt"),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black")
  ) ; plot_mammals_without

plot_bat <- mydata %>%
  filter(Order == "Chiroptera") %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Taxonomic \nReview \n+ Molecular",
                                         "Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = N.Countries)) +
  geom_violin(width = 0.8, fill = "#7fc97f", alpha = 0.5, adjust = 1.5) +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  #stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.signif",
  #                   tip.length = 0.02, label.y = c(13.1, 14.4, 15.6, 16.9, 18.1, 18.4), size = 3) +  # Ajuste o size conforme necessário
  
  # Teste global (Kruskal-Wallis) um pouco acima
  #stat_compare_means(method = "kruskal.test", label.x = .75, label.y = 19, size = 3)+  # Ajuste o size conforme necessário
  geom_text(aes(x = 1, y = 9.5, label = "a"), size = 4) +  # "A" em Taxonomic Review + Molecular
  geom_text(aes(x = 2, y = 8.5, label = "a"), size = 4) +   # "A" em Molecular
  geom_text(aes(x = 3, y = 5.5, label = "a"), size = 4) +   # "A" em Taxonomic Review
  geom_text(aes(x = 4, y = 5.5, label = "a"), size = 4) +   # "A" em Other evidences
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = c(1, 5, 10), expand = expansion(add = c(0, .5))) +
  theme_minimal() +
  theme(
    plot.margin=unit(c(t = 0, r = 0, b = 0, l = 0), "cm"),
    #plot.margin = margin(5, 5, 5, 5, unit = "pt"),
    legend.position = "none",
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black")
  ) ; plot_bat

plot_rodentia <- mydata %>%
  filter(Order == "Rodentia") %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Taxonomic \nReview \n+ Molecular",
                                         "Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = N.Countries)) +
  geom_violin(width = 0.8, fill = "#386cb0", alpha = 0.5, adjust = 1.5) +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  #stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.signif",
  #                   tip.length = 0.02, label.y = c(13.1, 14.4, 15.6, 16.9, 18.1, 18.4), size = 3) +  # Ajuste o size conforme necessário
  
  # Teste global (Kruskal-Wallis) um pouco acima
  #stat_compare_means(method = "kruskal.test", label.x = .75, label.y = 19, size = 3)+  # Ajuste o size conforme necessário
  geom_text(aes(x = 1, y = 5.5, label = "a"), size = 4, family = "Arial", fontface = "plain") +  # "A" em Taxonomic Review + Molecular
  geom_text(aes(x = 2, y = 5.5, label = "a"), size = 4, family = "Arial", fontface = "plain") +   # "B" em Molecular
  geom_text(aes(x = 3, y = 7.5, label = "ab"), size = 4, family = "Arial", fontface = "plain") +   # "B" em Taxonomic Review
  geom_text(aes(x = 4, y = 4.5, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "B" em Other evidences
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = c(1, 5, 10), expand = expansion(add = c(0, .5))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin=unit(c(t = 0, r = 0, b = 0, l = 0), "cm"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "black")
  ) ; plot_rodentia

#fig <- ggpubr::ggarrange(plot, plot_mammals_without, plot_bat, plot_rodentia, 
#                         ncol = 1, nrow = 4, 
#                         labels = "auto",
#                         font.label = list(size = 16, color = "black"),
#                         align = "hv");fig  # Proporções de largura entre colunas); fig

fig <- cowplot::plot_grid(plot, plot_mammals_without, plot_bat, plot_rodentia, 
                   ncol = 1, nrow = 4, align = "v", labels = "auto"); fig

# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure5.EvidencesCompare.pdf"),
       plot=fig, width=5, height=12, units="in", dpi = "print", cairo_pdf)
#ggsave(paste0(getwd(), "/figures/Figure5.EvidencesCompare.jpg"),
#       plot=fig, width=20, height=6, units="in", dpi = "print")
#ggsave(paste0(getwd(), "/figures/Figure5.EvidencesCompare.tiff"), 
#       plot=fig, width=20, height=6, units="in", dpi = "print")