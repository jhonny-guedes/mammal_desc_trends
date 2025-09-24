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
                     'rstatix',
                     'terra',
                     'grid',
                     'hrbrthemes',
                     'RColorBrewer'
)

new.packages<-needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(needed_packages, require, character.only = TRUE)

for (i in seq_along(needed_packages)) {
  print(packageVersion(needed_packages[i]))
}

# Clean global environment
rm(list=ls()); gc()

# 1) Load and understand the dataset ----
load("Dataset.Rdata")
names(data_all)
dim(data_all)

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
# N_evidences: The number of evidence types used in descriptions. All variables treated as binary (0 or 1), thus having equal weight.
# Latitude & Longitude: Type locality coordinates

# Check the number of species per Order
data_all %>% 
  dplyr::group_by(Order) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n))

# Order                n
# Rodentia           457
# Chiroptera         293
# Eulipotyphla       146
# Primates           106
# Didelphimorphia     24
# Artiodactyla        19
# Diprotodontia       14
# Dasyuromorphia      13
# Afrosoricida        10
# Lagomorpha           8
# Peramelemorphia      5
# Pilosa               4
# Carnivora            3
# Macroscelidea        3
# Paucituberculata     2
# Cingulata            1
# Hyracoidea           1
# Microbiotheria       1
# Monotremata          1

table(data_all$Order)
prop.table(table(data_all$Order))

# Check amounts of missing data among response variables and get other basic stats
names(data_all)

# Pearson correlation between number of specimens and number of taxa compared
mydata_cor <- data_all %>% 
  filter(!is.na(N.Specimens) & !is.na(TaxaComparedExamined))
cor.test(data_all$TaxaCompared, data_all$TaxaComparedExamined, method = "pearson")

# All mammals
summary(data_all[ , c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
                  "N.Pages", "N_evidences")])

# Mammals without rodents and bats
summary(
  data_all[data_all$Order != "Chiroptera" & data_all$Order != "Rodentia",
       c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
         "N.Pages", "N_evidences")]
)

# Only Chiroptera
summary(
  data_all[data_all$Order == "Chiroptera" ,
           c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
             "N.Pages", "N_evidences")]
)

# Only rodentia
summary(
  data_all[data_all$Order == "Rodentia" ,
       c("N.Specimens", "TaxaComparedExamined", "TaxaCompared", 
         "N.Pages", "N_evidences")]
)



rm(list=setdiff(ls(),c("data_all"))); gc() # clean workspace

# Make a backup
mydata <- data_all

# Check VIF multicollinearity 
vif_data <- mydata %>%
  select("N.Specimens", "TaxaCompared", "N.Pages", "N_evidences") %>%
  remove_missing()
mammals <- usdm::vif(vif_data)

vif_data <- mydata %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  select("N.Specimens", "TaxaCompared", "N.Pages", "N_evidences") %>%
  remove_missing()
nonbats_nonrodents <- usdm::vif(vif_data)[2]

vif_data <- mydata %>%
  filter(Order == "Rodentia") %>%
  select("N.Specimens", "TaxaCompared", "N.Pages", "N_evidences") %>%
  remove_missing()
rodentia <- usdm::vif(vif_data)[2]

vif_data <- mydata %>%
  filter(Order == "Chiroptera") %>%
  select("N.Specimens", "TaxaCompared", "N.Pages", "N_evidences") %>%
  remove_missing()
bats <- usdm::vif(vif_data)[2]

data.frame(mammals,
           nonbats_nonrodents,
           bats,
           rodentia
           )

# Check mean and variance across response variables
names(mydata)
mean(mydata$N_evidences, na.rm = T); var(mydata$N_evidences, na.rm = T) # 4.9; 1.2
mean(mydata$N.Pages, na.rm = T); var(mydata$N.Pages, na.rm = T) # 9.8; 162
mean(mydata$N.Specimens, na.rm = T); var(mydata$N.Specimens, na.rm = T) # 19; 1450
mean(mydata$TaxaCompared, na.rm = T); var(mydata$TaxaCompared, na.rm = T) # 5; 29
# The variance is lower than the mean only for the number of evidences; much higher for the others.

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
vars <- c("N_evidences", "N.Pages", "N.Specimens", "TaxaCompared", 
          "Year", "Log10BodyMass_g", "N_authors", "N.Countries", "SppRichPerGenus", "TaxonomicReview")

# change taxonomic review to categorical
mydata$TaxonomicReview <- ifelse(mydata$TaxonomicReview==1, yes = 'Yes', no = 'No')

# Define the custom x-axis labels
custom_labels <- c("N. of evidence", "N. of pages", "N. of specimens",
                   "N. of taxa compared", "Year of description", "Body mass (log10)", 
                   "N. of authors (log10)", "N. of countries (log 10)",
                   "N. of species/genus (log10)", "Taxonomic review")

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
# it's similar to setwd() function
local_directory <- file.path("D:/",
                             "repos",
                             "mammal_desc_trends",
                             "shapefiles") 

# Load shapefile of biogeographical realms
wwf_realms <- sf::read_sf(file.path(local_directory, "wwf_simplified", "wwf_simplified.shp"))  
wwf_realms <- wwf_realms %>% st_transform(crs = "+proj=eqearth") # change CRS to equal area
plot(wwf_realms$geometry)

# Load a shapefile depicting 'world limits'
world_limit <- sf::st_read(file.path(local_directory, "world_limit", "world_limit.shp"))
world_limit <- world_limit %>% st_transform(crs = "+proj=eqearth") # change CRS to equal area

# Load the dataset, then convert the geographical coordinates to an sf object
# load("Dataset.Rdata") # 1032 species
points_sf <- sf::st_as_sf(data_all[ !is.na(data_all$Latitude) & ! is.na(data_all$Longitude) , c('SpeciesName', 'Latitude', 'Longitude')],
                          coords = c("Longitude", "Latitude"), crs = st_crs("+proj=longlat +datum=WGS84"))

# Transform data to the same projection as the realms map
points_sf <- st_transform(points_sf, st_crs(wwf_realms))
st_crs(wwf_realms) == st_crs(points_sf) # true

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
data <- left_join(data_all, st_drop_geometry(points_sf), by = 'SpeciesName')
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
  "Eulipotyphla" = "#fa3238",
  "Other taxa" = "#ffa8ab",
  "Primates" = "#ff7579",
  "Rodentia" = "#386cb0"
)
names(MyColors)<-c("Chiroptera", "Eulipotyphla", "Other taxa", "Primates", "Rodentia")

levels(as.factor(PropPerRealm$wwf_realm))
PropPerRealm$wwf_realm <- factor(PropPerRealm$wwf_realm,
                                 labels = c("Afrotropic", "Australasia", "Global", "IndoMalay",
                                            "Nearctic", "Neotropic", "Palearctic"))

# Remake MyMap, colouring species points based on the colours of their respective Orders
points_sf <- points_sf %>% left_join(data_all[ , c('SpeciesName', 'Order')]) # extract Order from 'data' 
points_sf <- points_sf %>% left_join(classification, by = "Order")

MyMap2 <- ggplot2::ggplot() +
  
  # Add polygon boundaries for the wwf realms:
  geom_sf(data = wwf_realms, aes(fill=wwf_realm), colour="black", size=0.1) +
  geom_sf(data=world_limit, fill=NA, colour="black", linewidth=0.3)+
  
  # Add type-localities of species described:
  geom_sf(data = points_sf, color = "white", size = 2, shape = 20) +  # Larger white point for outline
  geom_sf(data = points_sf, aes(fill = NewOrder), color = "black", size = 1.3, shape = 21, alpha = 0.7) +  # Inner color based on NewOrder
  
  # Inform the filling colors:
  #scale_fill_manual(values = c(MyBiogeoColors, MyColors)) +
  scale_fill_manual(values = MyColors, name = NULL) +
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
        # legend
        legend.position = c(0.5, 0.15), 
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(face = "bold", size = 11, hjust = 0.5),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.7, "cm"),
        legend.background = element_blank(),
  ) +
  guides(fill = guide_legend(
    nrow = 2,           
    ncol = 3,           
    byrow = TRUE,       
    keywidth = unit(0.8, "cm"),
    keyheight = unit(0.9, "cm"),
    override.aes = list(shape = 22, size = 4, alpha = 0.8)  # Quadradinhos
  )); MyMap2

# Create one donut plot per biogeographical realm:
MyPlot <- list()

for(i in 1:nlevels(as.factor(PropPerRealm$wwf_realm))) {
  
  # Filter the dataset to include one biogeographical realm:
  FilteredData <- PropPerRealm[PropPerRealm$wwf_realm == levels(as.factor(PropPerRealm$wwf_realm))[i],]
  #FilteredData <- FilteredData %>% mutate(Order = fct_reorder(Order, Prop, .desc = TRUE))
  FilteredData <- FilteredData %>% mutate(NewOrder = fct_reorder(NewOrder, Prop, .desc = TRUE))
  
  # Get the color for the current realm
  current_realm <- levels(as.factor(PropPerRealm$wwf_realm))[i]
  
  # Store each plot as a list element:
  MyPlot[[i]] <- ggplot(FilteredData, aes(x = 2, y = Prop, fill = NewOrder)) +
    
    # Use geom_bar for donut segments
    geom_bar(stat = "identity", width = 1, color = "black") +
    
    #scale_fill_brewer(type = 'qual', palette = 'Set3') +
    scale_fill_manual(values = MyColors) +
    
    # Text labels based on condition
    #{
    #  if (i == 3) { # Global level; add Order names
    #    geom_text(aes(x = 2, label = paste0(scales::percent(Prop, accuracy = 1), "\n", NewOrder)), 
    #              position = position_stack(vjust = 0.5), size = 2.8, fontface = "bold")
    #  } else { # Other realms; only display percentage
    geom_text(aes(x = 2, label = scales::percent(Prop, accuracy = 1)), 
                  position = position_stack(vjust = 0.5),
              size = 2.8, fontface = "bold") +
    #  }
    #} +
    
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
ggsave(filename="figures/Figure1_Map.tiff", plot=FinalPlot, width=12, height=8, units="in", bg="white", limitsize=F)

# 3) Temporal trends in robustness of publications - based on annual means ----
# Four metrics will be analyzed:
# i)   number of evidence types (there are 2 versions of this variable: equal- and unequal-weight; see main text)
# ii)  number of pages per publication (includes only methods and results).
# iii) number of specimens examined,
# iv)  number of taxa the new species was compared to.

# Create a backup
mydata <- data_all
names(mydata)

#------------------------------------------------------------#
# Check correlation between response variables
#------------------------------------------------------------#

# Select predictor variables to check for correlation
cor(mydata[ , c("N_evidences", "N.Pages", "N.Specimens", "TaxaCompared")], 
    method = "spearman", use = "complete.obs")

#              N_evidences N.Pages   N.Specimens  TaxaCompared
#N_evidences   1.00000000 0.37784381  0.11561685   0.07198091
#N.Pages       0.37784381 1.00000000  0.04700053   0.04258466
#N.Specimens   0.11561685 0.04700053  1.00000000   0.08062655
#TaxaCompared  0.07198091 0.04258466  0.08062655   1.00000000
# low correlation among response variables (all below 0.40)

# Define custom labels
custom_labels <- c("N_evidences" = "N. of evidence",
                   "N.Pages" = "N. of pages",
                   "N.Specimens" = "N. of specimens",
                   "TaxaCompared" = "N. taxa compared")

# Create the ggpairs plot with custom labels
p <- ggpairs(
  mydata, 
  columns = c(27, 26, 23, 25), 
  upper = list(continuous = "cor"),  # sem wrap()
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag"),
  labeller = as_labeller(custom_labels)
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

# Get summary values for plotting
create_data <- function(data) {
  yearly_means <- data %>% 
    group_by(Year) %>% 
    summarise(N_evidences_avg = mean(N_evidences, na.rm = T),
              N_evidences_sd = sd(N_evidences, na.rm = T),
              N_evidences_nspp = sum( ! is.na(N_evidences)),
              
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
              N_countries_nspp = sum( ! is.na(N.Countries)),
              
              N_authors_avg = mean(N_authors, na.rm = T),
              N_authors_sd = sd(N_authors, na.rm = T),
              N_authors_nspp = sum( ! is.na(N_authors)),
              
              Morphometrics_avg = mean(Morphometrics, na.rm = T),
              Morphometrics_sd = sd(Morphometrics, na.rm = T),
              Morphometrics_nspp = sum( ! is.na(Morphometrics)),
              
              Osteology_avg = mean(Osteology, na.rm = T),
              Osteology_sd = sd(Osteology, na.rm = T),
              Osteology_nspp = sum( ! is.na(Osteology)),
              
              N_Genes_avg = mean(N.Genes, na.rm = T),
              N_Genes_sd = sd(N.Genes, na.rm = T),
              N_Genes_nspp = sum( ! is.na(N.Genes)),
              
              R_inter_avg = mean((N.Countries/N_authors)[!is.na(N.Countries) & !is.na(N_authors)], na.rm = T),
              R_inter_sd = sd((N.Countries/N_authors)[!is.na(N.Countries) & !is.na(N_authors)], na.rm = T),
              R_inter_nspp = sum(!is.na(N.Countries) & !is.na(N_authors))) %>%
    
    mutate(N_evidences_se = N_evidences_sd / sqrt(N_evidences_nspp),
           N_Pages_se = N_Pages_sd / sqrt(N_Pages_nspp),
           N_specimens_se = N_specimens_sd / sqrt(N_specimens_nspp),
           N_taxacomp_se = N_taxacomp_sd / sqrt(N_taxacomp_nspp),
           N_countries_se = N_countries_sd / sqrt (N_countries_nspp),
           N_authors_se = N_authors_sd / sqrt (N_authors_nspp),
           Morphometrics_se = Morphometrics_sd / sqrt (Morphometrics_nspp),
           Osteology_se = Osteology_sd / sqrt (Osteology_nspp),
           N_Genes_se = N_Genes_sd / sqrt (N_Genes_nspp),
           R_inter_se = R_inter_sd/ sqrt(R_inter_nspp))
  return(yearly_means)
}

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

##  Check the % increase/decrease in robustness metrics between the 
# first 5 years of the series (1990-94) and last 5-years (2018-22);
# this may avoid the impact of outliers if using a single year.
new_dat <- mydata[ , c("SpeciesName", "Order", "Year","N_evidences",
                       "N.Pages", "N.Specimens", "TaxaCompared", "N.Countries", "N_authors",
                       "Morphometrics", "Osteology", "N.Genes")]

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

# Number of evidence (non-equal weighted version as there are more variation in the data)
taxa <- all_yearly_means #para nao precisar repetir o codigo abaixo,

# so mudar o obj passado para 'taxa'. 
df90to94 <- apply(taxa[taxa$Year %in% 1990:1994, 'N_evidences_avg'], 2, mean)
df18to22 <- apply(taxa[taxa$Year %in% 2018:2022, 'N_evidences_avg'], 2, mean)
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

## Main comprehensiveness taxonomy proxies ----
figB <- create_plot(yearly_means, "N. of evidence",
                    mean = N_evidences_avg, 
                    se = N_evidences_se, 5, nrow = 1,
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

fig <- cowplot::plot_grid(figB, figC, figD, figE,
                          ncol = 1, nrow = 4, labels = "auto"); fig
# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure2.TemporalTrends.pdf"),
       plot=fig, width=12, height=10, units="in", dpi = "print", cairo_pdf)

## Average number of countries per author across the time ----
figF <- create_plot(yearly_means, "",
                    mean = R_inter_avg, 
                    se = R_inter_se, 5, nrow = 4,
                    show_titles = FALSE, show_x_labels = TRUE); figF

figF <- figF + xlab("Year of description"); figF

ggsave(paste0(getwd(), "/figures/FigureAux.AvgCountriesperAuthor.pdf"),
 plot=figF, width=4, height=10, units="in", dpi = "print", cairo_pdf)

## Alternative continuous proxies of taxonomy comprehensiveness ----
figH <- create_plot(yearly_means, "Morphometrics",
                    mean = Morphometrics_avg, 
                    se = Morphometrics_se, 5, nrow = 1,
                    show_titles = FALSE, show_x_labels = TRUE); figH

figI <- create_plot(yearly_means, "Osteology",
                    mean = Osteology_avg, 
                    se = Osteology_se, 5, nrow = 1,
                    show_titles = FALSE, show_x_labels = TRUE); figI

figJ <- create_plot(yearly_means, "N. of genes",
                    mean = N_Genes_avg, 
                    se = N_Genes_se, 5, nrow = 1,
                    show_titles = FALSE, show_x_labels = TRUE); figJ

figJ <- figJ + xlab("Year of description"); figJ

fig <- cowplot::plot_grid(figH, figI, figJ,
                          ncol = 1, nrow = 3, labels = "auto"); fig

ggsave(paste0(getwd(), "/figures/FigureAuxAlternativeProxies.pdf"),
       plot=fig, width=12, height=9, units="in", dpi = "print", cairo_pdf)

# Number of authors
figG <- create_plot(yearly_means, "N. of authors",
                    mean = N_authors_avg, 
                    se = N_authors_se, 5, nrow = 4,
                    show_titles = FALSE, show_x_labels = TRUE); figG

# Number of countries
figK <- create_plot(yearly_means, "N. of countries",
                    mean = N_countries_avg, 
                    se = N_countries_se, 5, nrow = 4,
                    show_titles = FALSE, show_x_labels = TRUE); figK

fig <- plot_grid(figG, figK, align = "v", labels = "auto") 
?plot_grid

ggsave(paste0(getwd(), "/figures/FigureAux.Nofauthors.pdf"),
       plot=fig, width=8, height=10, units="in", dpi = "print", cairo_pdf)

#ggsave(paste0(getwd(), "/figures/FigureAux.Nofcountries.pdf"),
#       plot=figF, width=4, height=10, units="in", dpi = "print", cairo_pdf)

# 4) Publication Robustness by Mammal Order ----
# Get summary values for plotting
# Select response, explanatory (year), and grouping variables
names(mydata)
new_dat <- mydata[ , c("SpeciesName","Order","N_evidences", 
                       "N.Pages", "N.Specimens", "TaxaCompared", "N.Countries")]

# Supondo que seu dataframe se chame df
df_long <- new_dat %>%
  pivot_longer(cols = c("N_evidences", "N.Pages", 
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

figA <- plot_boxplot(df_long, "N_evidences", "N. of evidence"); figA
figB <- plot_boxplot(df_long, "N.Pages", "N. of pages"); figB
figC <- plot_boxplot(df_long, "N.Specimens", "N. of specimens"); figC
figD <- plot_boxplot(df_long, "TaxaCompared", "N. of taxa compared"); figD
figE <- plot_boxplot(df_long, "N.Countries", "N. of countries involved"); figE

# Arrange plots in a grid
fig <- ggpubr::ggarrange(figA, figB, figC, figD, figE,
                         ncol = 2, nrow = 3, labels = "auto", 
                         font.label = list(size = 12,color = "black"),
                         align = "hv"); fig

# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure2.OrderRobustness.pdf"),
       plot=fig, width=14, height=18, units="in", dpi = "print", cairo_pdf)

# 5) Temporal trends in robustness of publications - based on GLMs.----
load("Dataset.Rdata")
rm(list=setdiff(ls(),c("data_all"))); gc() # clean workspace

# Make a backup
mydata <- data_all %>%
  mutate(countries_per_author = N.Countries/N_authors,
         lat_abs = abs(Latitude))

# Standardize continuous predictors (mean = 0, sd =1) in order to make them comparable
mydata$year.z <- scale(mydata$Year) 
mydata$logBodyMass.z <- scale(mydata$Log10BodyMass_g)
mydata$logN_authors.z <- scale(mydata$N_authors) 
mydata$logN_countries.z <- scale(mydata$countries_per_author) # N.Countries
mydata$logGenusRichness.z <- scale(mydata$SppRichPerGenus) 
mydata$Latitude.z <- scale(mydata$lat_abs) # New predictor

# Remove species with missing values on predictor variables
mydata <- mydata[ complete.cases(year.z, logBodyMass.z, logN_authors.z, logN_countries.z,
                                 logGenusRichness.z, TaxonomicReview, Latitude.z) , ] 

# n = 861 species with complete data on predictor variables

# Change taxonomic review to categorical
mydata$TaxonomicReview <- ifelse(mydata$TaxonomicReview==1, yes = 'Yes', no = 'No')
mydata$TaxonomicReview <- relevel(factor(mydata$TaxonomicReview), ref = "No")
levels(mydata$TaxonomicReview) # "No" taxonomic review as reference

# Check multicolinearity among continuous response variables
usdm::vif(mydata[ , year.z:Latitude.z])
#           Variables   VIF
#             year.z 1.242135
#      logBodyMass.z 1.102541
#     logN_authors.z 1.432883
#   logN_countries.z 1.342597
# logGenusRichness.z 1.083570
#         Latitude.z 1.057926
# Conclusion: keep all variables into the model as VIFs are low (< 2)

# Sample size per response variable
colSums( ! is.na(mydata[ , c("N_evidences", "N.Pages",
                             "N.Specimens", "TaxaCompared")]))
# N_evidences = 860 species  
# N.Pages = 848 species
# N.Specimens = 846 species
# TaxaCompared = 853 species

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
data_all %>% group_by(Order) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Make a backup
levels(as.factor(data_all$Order))

## All mammals ----

#------------------------------------------------------------#
# Model the number of evidence
#------------------------------------------------------------#
# Set model formula
form <- as.formula(N_evidences ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a GLM 
mod.evi2.nb <- glm.nb(formula = form, data = mydata[ ! is.na(mydata$N_evidences) , ] ) # remove rows with NAs on the response variable
mod.evi2.gau <- glm(formula = form, data = mydata[ ! is.na(mydata$N_evidences) , ] ) # remove rows with NAs on the response variable

# Compare models using AIC
AIC(mod.evi2.nb, mod.evi2.gau)
#             df      AIC
#mod.evi2.nb   9 3196.314
#mod.evi2.gau  9 2560.182
# The gaussian model is much better

# Check model output
summary(mod.evi2.gau)  
# Coefficients:
# year.z              0.306814   0.040997   7.484 1.80e-13 ***
# logN_authors.z     -0.095450   0.042542  -2.244  0.02511 *  
# logN_countries.z   -0.004043   0.043790  -0.092  0.92646    
# logBodyMass.z      -0.194836   0.038610  -5.046 5.51e-07 ***
# logGenusRichness.z  0.004075   0.039092   0.104  0.91699    
# TaxonomicReviewYes -0.124243   0.090964  -1.366  0.17235    
# Latitude.z          0.110732   0.038688   2.862  0.00431 **

# Compute R2
evidences_r2 <-performance::r2(mod.evi2.gau) # R2: 0.128

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.evi2.gau, "N. evidence II"))

# Save model output for latter checking phylogenetic correlation in model residuals
save(mod.evi2.gau, file = 'model_outputs/mod.evi.II.Rdata')

#------------------------------------------------------------#
# Number of pages
#------------------------------------------------------------#

# Set a full model formula
mydata$LogN.Pages <- log10(mydata$N.Pages) # transform it 'out' of the model, otherwise there will be an error when calculating R2
form <- as.formula(LogN.Pages ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages <- glm(formula = form, family = 'gaussian', data = mydata[ !is.na(mydata$N.Pages) , ])

# Check results
summary(mod.pages) 

# Coefficients:
# year.z              0.04022    0.01271   3.164  0.00161 ** 
# logN_authors.z     -0.01128    0.01311  -0.861  0.38975    
# logN_countries.z   -0.01642    0.01353  -1.214  0.22519    
# logBodyMass.z      -0.05254    0.01197  -4.389 1.28e-05 ***
# logGenusRichness.z -0.06234    0.01212  -5.145 3.34e-07 ***
# TaxonomicReviewYes -0.05901    0.02816  -2.096  0.03641 *  
# Latitude.z          0.00439    0.01203   0.365  0.71520    

# Compute R2
pages_r2 <- performance::r2(mod.pages) # R2: 0.066

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
                     logGenusRichness.z + TaxonomicReview + + Latitude.z)

# Fit the model 
mod.ts <- glm.nb(formula = form, data = mydata[ !is.na(mydata$N.Specimens) , ])

# Check results
summary(mod.ts) 

# Coefficients:
# year.z              0.197774   0.044835   4.411 1.03e-05 ***
# logN_authors.z     -0.137162   0.046910  -2.924 0.003456 ** 
# logN_countries.z   -0.082510   0.048007  -1.719 0.085665 .  
# logBodyMass.z       0.007855   0.043072   0.182 0.855294    
# logGenusRichness.z  0.144755   0.042610   3.397 0.000681 ***
# TaxonomicReviewYes  0.030727   0.099531   0.309 0.757532    
# Latitude.z          0.033516   0.042359   0.791 0.428802    

# Get R2
nspecimens_r2 <- performance::r2(mod.ts) # Nagelkerke's R2: 0.078

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
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.tcom <- glm.nb(formula = form, data = mydata[ !is.na(mydata$TaxaCompared) , ])

# Check results
summary(mod.tcom) 

# Coefficients:
# year.z              0.144762   0.031814   4.550 5.36e-06 ***
# logN_authors.z      0.010253   0.032574   0.315   0.7529    
# logN_countries.z    0.010370   0.033712   0.308   0.7584    
# logBodyMass.z       0.008963   0.030072   0.298   0.7657    
# logGenusRichness.z  0.194757   0.028733   6.778 1.22e-11 ***
# TaxonomicReviewYes  0.024025   0.070610   0.340   0.7337    
# Latitude.z         -0.052695   0.030021  -1.755   0.0792 . 

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom) # Nagelkerke's R2: 0.164

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.tcom, "N. taxa compared"))

# Save model output
save(mod.tcom, file = 'model_outputs/mod.tcom.Rdata')

# Round numbers 
results[,c(3:5)] <- round(results[,c(3:5)], digits = 3)

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
rm(list=setdiff(ls(),c("data_all","mydata","extract_model_results"))); gc()

## Non-bats & non-rodents ----
mammals_without <- mydata %>%
  filter(Order != "Rodentia" & Order != "Chiroptera")

# Check multicolinearity among continuous response variables
usdm::vif(mammals_without[ , year.z:Latitude.z])

results <- data.frame()
levels(mydata$TaxonomicReview) # "No" taxonomic review as reference

#------------------------------------------------------------#
# Model the number of evidence
#------------------------------------------------------------#
nrow(mammals_without) # without NA's 268 spp

# Set model formula
form <- as.formula(N_evidences ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a GLM 
mod.evi2.nb.without <- glm.nb(formula = form,
                              data = mammals_without[ ! is.na(mammals_without$N_evidences) , ] ) # remove rows with NAs on the response variable
mod.evi2.gau.without <- glm(formula = form, 
                            data = mammals_without[ ! is.na(mammals_without$N_evidences) , ] ) # remove rows with NAs on the response variable

# Compare models using AIC
AIC(mod.evi2.nb.without, mod.evi2.gau.without)
#                     df       AIC
#mod.evi2.nb.without   9 1033.9402
#mod.evi2.gau.without  9  829.9525
# The gaussian model is much better

# Check model output
summary(mod.evi2.gau.without)  
# Results
#                     Estimate Std. Error t value Pr(>|t|)    
# year.z              0.34377    0.07104   4.839 2.17e-06 ***
# logN_authors.z     -0.08288    0.05313  -1.560  0.11990    
# logN_countries.z   -0.05398    0.08040  -0.671  0.50255    
# logBodyMass.z      -0.17127    0.05659  -3.026  0.00271 ** 
# logGenusRichness.z  0.13642    0.04815   2.833  0.00495 ** 
# TaxonomicReviewYes  0.02602    0.15354   0.169  0.86557    
# Latitude.z          0.19475    0.06871   2.834  0.00493 ** 

# Compute R2
evidences_r2 <- performance::r2(mod.evi2.gau.without) # R2: 0.24

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
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages.without <- glm(formula = form, family = 'gaussian', data = mammals_without[ !is.na(mammals_without$N.Pages) , ])

# Check results
summary(mod.pages.without) 

# Results:
#                     Estimate Std. Error t value Pr(>|t|)    
# year.z              0.06577    0.02060   3.193 0.001572 ** 
# logN_authors.z     -0.02733    0.01529  -1.788 0.074908 .  
# logN_countries.z   -0.05860    0.02329  -2.517 0.012430 *  
# logBodyMass.z      -0.06173    0.01644  -3.755 0.000213 ***
# logGenusRichness.z -0.05647    0.01390  -4.064 6.34e-05 ***
# TaxonomicReviewYes -0.13803    0.04481  -3.081 0.002279 ** 
# Latitude.z          0.01284    0.01994   0.644 0.520167   

# Compute R2
pages_r2 <- performance::r2(mod.pages.without) # R2: 0.19

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
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.ts.without <- glm.nb(formula = form, data = mammals_without[ !is.na(mammals_without$N.Specimens) , ])

# Check results
summary(mod.ts.without) 

# Results
# year.z              0.28542    0.07610   3.751 0.000176 ***
# logN_authors.z     -0.19823    0.05757  -3.443 0.000574 ***
# logN_countries.z   -0.27881    0.08733  -3.193 0.001409 ** 
# logBodyMass.z       0.02392    0.06273   0.381 0.703004    
# logGenusRichness.z  0.26157    0.05076   5.153 2.56e-07 ***
# TaxonomicReviewYes  0.04216    0.16493   0.256 0.798229    
# Latitude.z          0.28315    0.07356   3.849 0.000119 ***

# Get R2
nspecimens_r2 <- performance::r2(mod.ts.without) # Nagelkerke's R2: 0.32

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
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.tcom.without <- glm.nb(formula = form, data = mammals_without[ !is.na(mammals_without$TaxaCompared) , ])

# Check results
summary(mod.tcom.without) 

# Results
#                     Estimate Std. Error z value Pr(>|z|)    
# year.z              0.167398   0.053191   3.147  0.00165 ** 
# logN_authors.z      0.007078   0.039298   0.180  0.85706    
# logN_countries.z    0.078090   0.058347   1.338  0.18078    
# logBodyMass.z       0.065251   0.042526   1.534  0.12494    
# logGenusRichness.z  0.176901   0.034038   5.197 2.02e-07 ***
# TaxonomicReviewYes -0.123074   0.117394  -1.048  0.29446    
# Latitude.z          0.017347   0.051115   0.339  0.73433   

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom.without) # Nagelkerke's R2: 0.286

# Extract and store model results
results <- bind_rows(results, 
                     extract_model_results(mod.tcom.without, "N. taxa compared"))

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
  value = c(evidences_r2$R2, pages_r2$R2,
            nspecimens_r2$R2_Nagelkerke, taxacompared_r2$R2_Nagelkerke),
  metric = c("R2", "R2", "R2_Nagelkerke", "R2_Nagelkerke"),
  group = "Non-bats & non-rodents"
)
fwrite(results_r2, file = 'model_outputs/r2_without.csv')

# Clean workspace
rm(list=setdiff(ls(),c("data_all","mydata", "extract_model_results"))); gc()

## Rodents ----
# Subset data
rodents <- data_all %>%
  filter(Order == "Rodentia") %>% # n = 421
  mutate(countries_per_author = N.Countries/N_authors,
         lat_abs = abs(Latitude)) 
  
# Check for skewed distributions and kurtosis among predictors (transform data if necessary).
names(rodents)
e1071::skewness(rodents$N_authors); e1071::kurtosis(rodents$N_authors) # 0.97 and 0.52
e1071::skewness(rodents$countries_per_author, na.rm = T); e1071::kurtosis(rodents$countries_per_author, na.rm = T) # 1.53 and 3.78
e1071::skewness(rodents$Year); e1071::kurtosis(rodents$Year) # -0.26 and -1.07
e1071::skewness(rodents$SppRichPerGenus, na.rm = T); e1071::kurtosis(rodents$SppRichPerGenus, na.rm = T) # 1.76 and 2.68
# Conclusion: log10 transform no. of countries_per_author and species richness per genus [body mass is already transformed]
rodents$countries_per_author <- log10(rodents$countries_per_author)
rodents$SppRichPerGenus <- log10(rodents$SppRichPerGenus + 1)

# Change taxonomic review to categorical
rodents$TaxonomicReview <- ifelse(rodents$TaxonomicReview==1, yes = 'Yes', no = 'No')
rodents$TaxonomicReview <- relevel(factor(rodents$TaxonomicReview), ref = "No")
levels(rodents$TaxonomicReview) # "No" taxonomic review as reference

# Standardize continuous predictors (mean = 0, sd =1) in order to make them comparable
rodents$year.z <- scale(rodents$Year) 
rodents$logBodyMass.z <- scale(rodents$Log10BodyMass_g)
rodents$logN_authors.z <- scale(rodents$N_authors) 
rodents$logN_countries.z <- scale(rodents$countries_per_author) 
rodents$logGenusRichness.z <- scale(rodents$SppRichPerGenus) 
rodents$Latitude.z <- scale(rodents$lat_abs) 

# Remove species with missing values on predictor variables
rodents <- rodents[ complete.cases(year.z, logBodyMass.z, logN_authors.z, logN_countries.z,
                                   logGenusRichness.z, TaxonomicReview, lat_abs) , ] 
# n = 351 species with complete data on predictor variables

# Check multicolinearity among predictor variables
usdm::vif(rodents[ , year.z:Latitude.z])
#           Variables      VIF
#             year.z 1.330228
#      logBodyMass.z 1.016157
#     logN_authors.z 2.001723
#   logN_countries.z 1.769303
# logGenusRichness.z 1.057308
#         Latitude.z 1.036161

# Sample size per response variable
colSums( ! is.na(rodents[ , c("N_evidences", "N.Pages",
                              "N.Specimens", "TaxaCompared")]))
# N_evidences = 350 species  
# N.Pages = 344 species
# N.Specimens = 347 species
# TaxaCompared = 349 species

# Create an empty data frame to store model results
results <- data.frame()

#------------------------------------------------------------#
# Model the number of evidence
#------------------------------------------------------------#
# Set model formula
form <- as.formula(N_evidences ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a GLM 
mod.evi2.nb <- glm.nb(formula = form, data = rodents[ ! is.na(rodents$N_evidences) , ] ) 
mod.evi2.gau <- glm(formula = form, data = rodents[ ! is.na(rodents$N_evidences) , ] ) 

# Compare models using AIC
AIC(mod.evi2.nb, mod.evi2.gau)
#            df      AIC
#mod.evi2.nb   9 1324.531
#mod.evi2.gau  9 1029.075

# Check model output
summary(mod.evi2.gau)  
# Results
#                     Estimate Std. Error t value Pr(>|t|)    
# year.z              0.22854    0.06327   3.612 0.000349 ***
# logN_authors.z      0.04768    0.08676   0.550 0.582963    
# logN_countries.z    0.05731    0.07827   0.732 0.464538    
# logBodyMass.z      -0.03071    0.05496  -0.559 0.576703    
# logGenusRichness.z  0.04331    0.05619   0.771 0.441344    
# TaxonomicReviewYes -0.24438    0.14236  -1.717 0.086949 .  
# Latitude.z          0.07120    0.06111   1.165 0.244810 

# Compute R2
evidences_r2 <- performance::r2(mod.evi2.gau) # R2: 0.073

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
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages <- glm(formula = form, family = 'gaussian', data = rodents[ !is.na(rodents$N.Pages) , ])

# Check results
summary(mod.pages) 

# Results:
#                 Estimate Std. Error t value Pr(>|t|)    
# year.z              0.0206415  0.0200187   1.031 0.303232    
# logN_authors.z      0.0005953  0.0272508   0.022 0.982586    
# logN_countries.z   -0.0065848  0.0246318  -0.267 0.789379    
# logBodyMass.z      -0.0100196  0.0173787  -0.577 0.564632    
# logGenusRichness.z -0.0665933  0.0176902  -3.764 0.000197 ***
# TaxonomicReviewYes -0.0216636  0.0446239  -0.485 0.627658    
# Latitude.z         -0.0316376  0.0194041  -1.630 0.103941

# Compute R2
pages_r2 <- performance::r2(mod.pages) # R2: 0.052

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.pages, "N. pages"))

# save model output
save(mod.pages, file = 'model_outputs/mod.pages.rodents.Rdata')

#------------------------------------------------------------#
# Number of specimens 
#------------------------------------------------------------#
# Set a full model formula
form <- as.formula(N.Specimens ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.ts <- glm.nb(formula = form, data = rodents[ !is.na(rodents$N.Specimens) , ])

# Check results
summary(mod.ts) 

# Results
#                     Estimate Std. Error z value Pr(>|z|)    
# year.z              0.17361    0.06855   2.533   0.0113 *  
# logN_authors.z     -0.11666    0.09480  -1.231   0.2185    
# logN_countries.z   -0.05375    0.08525  -0.631   0.5284    
# logBodyMass.z      -0.04017    0.05993  -0.670   0.5027    
# logGenusRichness.z  0.24993    0.06160   4.057 4.97e-05 ***
# TaxonomicReviewYes -0.26002    0.15573  -1.670   0.0950 .  
# Latitude.z         -0.06774    0.06656  -1.018   0.3088  

# Get R2
nspecimens_r2 <- performance::r2(mod.ts) # 0.109

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.ts, "N. specimens"))

# Save model output
save(mod.ts, file = 'model_outputs/mod.ts.rodents.Rdata')

#------------------------------------------------------------#
# Number of taxa compared
#------------------------------------------------------------#
# Set a full model formula
form <- as.formula(TaxaCompared ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.tcom <- glm.nb(formula = form, data = rodents[ !is.na(rodents$TaxaCompared) , ])

# Check results
summary(mod.tcom) 

# Results
#                     Estimate Std. Error z value Pr(>|z|)    
# year.z              0.06689    0.05450   1.227   0.2197    
# logN_authors.z      0.05408    0.07414   0.729   0.4657    
# logN_countries.z    0.00516    0.06746   0.076   0.9390    
# logBodyMass.z       0.01325    0.04763   0.278   0.7808    
# logGenusRichness.z  0.25623    0.04981   5.144 2.69e-07 ***
# TaxonomicReviewYes  0.16386    0.12159   1.348   0.1778    
# Latitude.z         -0.12232    0.05317  -2.301   0.0214 *  

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom) # R2: 0.204

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
rm(list=setdiff(ls(),c("data_all", "extract_model_results"))); gc()

## Bats ----
# Subset data
bats <- data_all %>%
  filter(Order == "Chiroptera") %>% # 280 
  mutate(countries_per_author = N.Countries/N_authors,
         lat_abs = abs(Latitude))  

# Check for skewed distributions and kurtosis among predictors (transform data if necessary).
names(bats)
e1071::skewness(bats$N_authors); e1071::kurtosis(bats$N_authors) # 1.60 and 3.04
e1071::skewness(bats$countries_per_author, na.rm = T); e1071::kurtosis(bats$countries_per_author, na.rm = T) # 1.83 and 6.98
e1071::skewness(bats$Year); e1071::kurtosis(bats$Year) # -0.55 and -0.42
e1071::skewness(bats$SppRichPerGenus, na.rm = T); e1071::kurtosis(bats$SppRichPerGenus, na.rm = T) # 1.36 and 0.42
# Conclusion: log10 transform no. of authors and no. of countries [body mass is already transformed]
bats$N_authors <- log10(bats$N_authors)
bats$N.Countries <- log10(bats$countries_per_author)

# Change taxonomic review to categorical
bats$TaxonomicReview <- ifelse(bats$TaxonomicReview==1, yes = 'Yes', no = 'No')
bats$TaxonomicReview <- relevel(factor(bats$TaxonomicReview), ref = "No")
levels(bats$TaxonomicReview) # "No" taxonomic review as reference

# Standardize continuous predictors (mean = 0, sd =1) in order to make them comparable
bats$year.z <- scale(bats$Year) 
bats$logBodyMass.z <- scale(bats$Log10BodyMass_g)
bats$logN_authors.z <- scale(bats$N_authors) 
bats$logN_countries.z <- scale(bats$countries_per_author) 
bats$logGenusRichness.z <- scale(bats$SppRichPerGenus) 
bats$Latitude.z <- scale(bats$lat_abs) 

# Remove species with missing values on predictor variables
bats <- bats[ complete.cases(year.z, logBodyMass.z, logN_authors.z, logN_countries.z,
                             logGenusRichness.z, TaxonomicReview, Latitude.z) , ] 
# n = 221 species with complete data on predictor variables

# Check multicolinearity among predictor variables
usdm::vif(bats[ , year.z:Latitude.z])
#           Variables   VIF
#             year.z 1.471766
#      logBodyMass.z 1.061872
#     logN_authors.z 1.921782
#   logN_countries.z 1.355935
# logGenusRichness.z 1.052768
#         Latitude.z 1.047410
# Low multicolinearity (VIFs < 2)

# Sample size per response variable
colSums( ! is.na(bats[ , c("N_evidences", "N.Pages", "N.Specimens", "TaxaCompared")]))
# N_evidences = 225 species  
# N.Pages = 225 species
# N.Specimens = 223 species
# TaxaCompared = 225 species

# Create an empty data frame to store model results
results <- data.frame()

#------------------------------------------------------------#
# Model the number of evidence
#------------------------------------------------------------#

# Set model formula
form <- as.formula(N_evidences ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a GLM 
mod.evi2.nb <- glm.nb(formula = form, data = bats[ ! is.na(bats$N_evidences) , ] ) 
mod.evi2.gau <- glm(formula = form, data = bats[ ! is.na(bats$N_evidences) , ] ) 

# Compare models using AIC
AIC(mod.evi2.nb, mod.evi2.gau)
#             df      AIC
#mod.evi2.nb   9 833.8470
#mod.evi2.gau  9 526.2016

# Check model output
summary(mod.evi2.gau)  
# Results
#                     Estimate Std. Error t value Pr(>|t|)    
# year.z              0.126515   0.062692   2.018  0.04482 *  
# logN_authors.z      0.214211   0.067641   3.167  0.00176 ** 
# logN_countries.z    0.064706   0.061904   1.045  0.29707    
# logBodyMass.z      -0.030954   0.051944  -0.596  0.55186    
# logGenusRichness.z -0.014911   0.059166  -0.252  0.80127    
# TaxonomicReviewYes -0.007859   0.126648  -0.062  0.95058    
# Latitude.z          0.053196   0.052292   1.017  0.31015  

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
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit a Gaussian model as the response is continuous (remove rows with NAs on the response variable)
mod.pages <- glm(formula = form, family = 'gaussian', data = bats[ !is.na(bats$N.Pages) , ])

# Check results
summary(mod.pages) 

# Results:
# #                 Estimate Std. Error t value Pr(>|t|)    
# year.z             -0.01565    0.02585  -0.605   0.5455    
# logN_authors.z      0.12556    0.02789   4.501  1.1e-05 ***
# logN_countries.z    0.04794    0.02553   1.878   0.0618 .  
# logBodyMass.z       0.01848    0.02142   0.863   0.3892    
# logGenusRichness.z -0.02824    0.02440  -1.157   0.2483    
# TaxonomicReviewYes  0.01668    0.05223   0.319   0.7497    
# Latitude.z          0.03777    0.02156   1.752   0.0813 . 

# Compute R2
pages_r2 <- performance::r2(mod.pages) # R2: 0.12

# Extract and store model results
results <- bind_rows(results, extract_model_results(mod.pages, 
                                                    "N. pages"))

# save model output
save(mod.pages, file = 'model_outputs/mod.pages.bats.Rdata')

#------------------------------------------------------------#
# Number of specimens 
#------------------------------------------------------------#

# Set a full model formula
form <- as.formula(N.Specimens ~ 
                     year.z + logN_authors.z + logN_countries.z + logBodyMass.z + 
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.ts <- glm.nb(formula = form, data = bats[ !is.na(bats$N.Specimens) , ])

# Check results
summary(mod.ts) 

# Results
#                     Estimate Std. Error z value Pr(>|z|)    
# year.z              0.02417    0.09740   0.248   0.8040    
# logN_authors.z      0.07612    0.10476   0.727   0.4675    
# logN_countries.z   -0.18359    0.09671  -1.898   0.0577 .  
# logBodyMass.z       0.18540    0.07994   2.319   0.0204 *  
# logGenusRichness.z -0.20118    0.09256  -2.174   0.0297 *  
# TaxonomicReviewYes  0.58473    0.19491   3.000   0.0027 ** 
# Latitude.z         -0.04183    0.08138  -0.514   0.6072  

# Get R2
nspecimens_r2 <- performance::r2(mod.ts) # 0.128

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
                     logGenusRichness.z + TaxonomicReview + Latitude.z)

# Fit the model 
mod.tcom <- glm.nb(formula = form, data = bats[ !is.na(bats$TaxaCompared) , ])

# Check results
summary(mod.tcom) 

# Results
#                     Estimate Std. Error z value Pr(>|z|)    
# year.z              0.10841    0.05910   1.834  0.06659 .  
# logN_authors.z      0.10051    0.06306   1.594  0.11095    
# logN_countries.z   -0.02797    0.05819  -0.481  0.63071    
# logBodyMass.z       0.02023    0.04843   0.418  0.67614    
# logGenusRichness.z  0.15282    0.05327   2.869  0.00412 ** 
# TaxonomicReviewYes -0.16664    0.12020  -1.386  0.16564    
# Latitude.z         -0.01535    0.04857  -0.316  0.75201 

# Get R2
taxacompared_r2 <- performance::r2(mod.tcom) # R2: 0.188

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
#rm(list = ls()); gc()
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
#View(results)
# Reorder predictors and fix names
levels(as.factor(results$term))
results <- results %>%
  filter(term != "(Intercept)") %>%
  mutate(response = str_replace(response, "N. evidence II", "N. evidence")) 

results$term <- factor(results$term, 
                       levels = c("logBodyMass.z", "logN_authors.z", "year.z", 
                                  "Latitude.z",
                                  "TaxonomicReviewYes","logN_countries.z",
                                  "logGenusRichness.z"),
                       labels = c("Body mass" ,"Number of\nauthors", "Year of\ndescription", 
                                  "Absolute\nLatitude",
                                  "Taxonomic\nreview","Avg. Number of\ncountries/author", 
                                  "Number of\nspecies/genus"))

levels(as.factor(results$response))

MyColors <- c("#8e0152", "#bf812d", "#4d4d4d", "#d6604d")
names(MyColors) <- c("N. evidence","N. pages","N. specimens","N. taxa compared")

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
  scale_y_continuous(breaks = seq(-1, 1, by = 0.2)) +
  facet_grid(term ~ group, scales = 'fixed', switch = "y") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    axis.ticks.y.left = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
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
#ggsave(paste0(getwd(), "/figures/Figure.ModelOutputs2.jpg"), plot=p, width=10, height=6, units="in", dpi = 'print')
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
View(results_r2)
MyColors <- c("#8e0152", "#bf812d", "#4d4d4d", "#d6604d")
names(MyColors) <- c("N. evidence","N. pages","N. specimens","N. taxa compared")
# Crie um dataframe para os pontos do eixo Y

# Plot
inset_plot <- ggplot(results_r2, aes(x = name, y = value, fill = group)) +
  # barras
  geom_col(position = position_dodge(width = 0.9), width = 0.7, alpha = 0.3,
           color = "black", size = 0.2) +
  # Ajuste de escala para o eixo X após o coord_flip
  scale_x_discrete(expand = c(0, 0)) +  # Evita o espaçamento extra
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1), limits = c(0, 0.5), expand = c(0, 0)) +  # Definindo limite superior no eixo Y (horizontal)
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
    axis.text.x = element_text(angle = 0, hjust = 1, size = 15),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 15),
    axis.line.x = element_line(color = "black", size = 0.5),  # Linha do eixo X após coord_flip
    axis.ticks.x = element_line(color = "black", size = 0.5),  
    axis.line.y = element_line(color = "black", size = 0.5),  # Linha do eixo Y (horizontal após coord_flip)
    axis.ticks.y = element_line(color = "black", size = 0.5),  # Marcas no eixo Y (horizontal após coord_flip)
    legend.position = "none"
  ); inset_plot

ggsave(paste0(getwd(), "/figures/Figure.aux2.pdf"), plot=inset_plot,
       width=5, height=6, units="in", dpi = 'print', cairo_pdf)

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

data <- data_all %>%
  drop_na(Year, Log10BodyMass_g, N_authors, N.Countries, 
          SppRichPerGenus, TaxonomicReview, Latitude) # 861 linhas

# Sample size per response variable
colSums( ! is.na(data[ , c("N_evidences", "N.Pages", "N.Specimens", "TaxaCompared")]))
# N_evidences = 860 species  
# N.Pages = 848 species
# N.Specimens = 846 species
# TaxaCompared = 853 species
# OKAY; match the numbers on modelling procedures (line 982)

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
# N. evidences
evi_datII <- as.data.frame(
  cbind( 
    data[ ! is.na(data$N_evidences) , ], evi_residsII
    )
  ); rm(evi_residsII)
evi_datII_bats <- as.data.frame(
  cbind(
    data[ data$Order == 'Chiroptera' & (! is.na(data$N_evidences)) , ],
    evi_residsII_bats
    )
  ); rm(evi_residsII_bats)
evi_datII_without <- as.data.frame(
  cbind(
    data[ data$Order != 'Chiroptera' & data$Order != 'Rodentia' & (! is.na(data$N_evidences)) , ]
    , evi_residsII_without
    )
  ); rm(evi_residsII_without)
evi_datII_rodents <- as.data.frame(
  cbind(
    data[data$Order == "Rodentia" & !is.na(data$N_evidences), ],
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
name.check(phylo_tree_ts[[1]], ts_dat_without); rm(obj) # OK = all species on phylogeny matching those on the data frame

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

## Check if we have the same spp in our data as in the tree
#obj <- geiger::name.check(mammal_tree[[1]], evi_datI_bats)
#
## Drop species not present on the tree
#evi_datI_bats <- evi_datI_bats[ ! evi_datI_bats$SpeciesName %in% obj$data_not_tree , ]
#
## Remove species present on the tree but not on the dataset
#phylo_tree_evi<-list()
#
## Remove spp that are in the phylogeny but not in the dataset
#for (i in 1:100) {
#  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
#} 
#name.check(phylo_tree_evi[[1]], evi_datI_bats); rm(obj) # OK = all species on phylogeny matching those on the data frame
#
## Prepare workspace for parallel computing:
#cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK') # selecting half of all available cores
#registerDoParallel(cl)
#getDoParWorkers()
#
#{
#  PhyCorr_evi_I_bats<-foreach(i = 1:100, 
#                              .export = 'rbind',
#                              .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
#                                
#                                # Select one trimmed fully-sampled tree:
#                                my_tree<-phylo_tree_evi[[i]]
#                                
#                                # Create a phylo4 object including GLMM model residuals:
#                                phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datI_bats$evi_residsI_bats))
#                                
#                                # Compute the phylogenetic correlogram:
#                                phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
#                                                                       dist.phylo="patristic", n.points=14, ci.bs=100)
#                                
#                                correlogram_data<-as.data.frame(phy.cor[[1]])
#                                names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
#                                correlogram_data$Iter<-i
#                                correlogram_data$N_class<-1:14
#                                correlogram_data
#                                
#                              }
#  stopCluster(cl) # terminate cluster
#}
#
## Extract the average correlogram output across iterations:
#PhyCorr_evi_I_bats<-as.data.table(rbindlist(PhyCorr_evi_I_bats))
#AvgPhyCorr_evi_I_bats<-PhyCorr_evi_I_bats[, .(Distance=mean(dist.class, na.rm=T),
#                                              Lower_CI=mean(lower_ci, na.rm=T),
#                                              Upper_CI=mean(upper_ci, na.rm=T),
#                                              MoranI_coef=mean(coef, na.rm=T)),
#                                          by = .(N_class)]
#
## Export the results:
#save(PhyCorr_evi_I_bats, AvgPhyCorr_evi_I_bats, file="PhyloCorr/PhyloCorr_evi_I_bats.Rdata")
#rm(evi_datI_bats, PhyCorr_evi_I_bats, AvgPhyCorr_evi_I_bats) # clean workspace

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

## Check if we have the same spp in our data as in the tree
#obj <- geiger::name.check(mammal_tree[[1]], evi_datI_rodents)
#
## Drop species not present on the tree
#evi_datI_rodents <- evi_datI_rodents[ ! evi_datI_rodents$SpeciesName %in% obj$data_not_tree , ]
#
## Remove species present on the tree but not on the dataset
#phylo_tree_evi<-list()
#
## Remove spp that are in the phylogeny but not in the dataset
#for (i in 1:100) {
#  phylo_tree_evi[[i]] <- drop.tip(mammal_tree[[i]], obj$tree_not_data)
#} 
#name.check(phylo_tree_evi[[1]], evi_datI_rodents); rm(obj) # OK = all species on phylogeny matching those on the data frame
#
## Prepare workspace for parallel computing:
#cl <- makePSOCKcluster(detectCores()*0.5, type = 'SOCK') # selecting half of all available cores
#registerDoParallel(cl)
#getDoParWorkers()
#
#{
#  PhyCorr_evi_I_rodents<-foreach(i = 1:100, 
#                                 .export = 'rbind',
#                                 .packages = c("data.table", "phylobase", "phylosignal"))  %dopar% {
#                                   
#                                   # Select one trimmed fully-sampled tree:
#                                   my_tree<-phylo_tree_evi[[i]]
#                                   
#                                   # Create a phylo4 object including GLMM model residuals:
#                                   phylo4d_filter<-phylobase::phylo4d(x=my_tree, data.frame(GLM_resid=evi_datI_rodents$evi_residsI_rodents))
#                                   
#                                   # Compute the phylogenetic correlogram:
#                                   phy.cor<-phylosignal::phyloCorrelogram(p4d=phylo4d_filter, trait=names(tdata(phylo4d_filter)),
#                                                                          dist.phylo="patristic", n.points=14, ci.bs=100)
#                                   
#                                   correlogram_data<-as.data.frame(phy.cor[[1]])
#                                   names(correlogram_data)<-c("dist.class", "lower_ci", "upper_ci", "coef")
#                                   correlogram_data$Iter<-i
#                                   correlogram_data$N_class<-1:14
#                                   correlogram_data
#                                   
#                                 }
#  stopCluster(cl) # terminate cluster
#}
#
## Extract the average correlogram output across iterations:
#PhyCorr_evi_I_rodents<-as.data.table(rbindlist(PhyCorr_evi_I_rodents))
#AvgPhyCorr_evi_I_rodents<-PhyCorr_evi_I_rodents[, .(Distance=mean(dist.class, na.rm=T),
#                                                    Lower_CI=mean(lower_ci, na.rm=T),
#                                                    Upper_CI=mean(upper_ci, na.rm=T),
#                                                    MoranI_coef=mean(coef, na.rm=T)),
#                                                by = .(N_class)]
#
## Export the results:
#save(PhyCorr_evi_I_rodents, AvgPhyCorr_evi_I_rodents, file="PhyloCorr/PhyloCorr_evi_I_rodents.Rdata")
#rm(evi_datI_rodents, PhyCorr_evi_I_rodents, AvgPhyCorr_evi_I_rodents) # clean workspace
#
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
response <- c('N. evidence II', 'N. preserved\nspecimens',
              'N. pages', 'N. taxa\ncompared')

for (i in seq_along(Corr_list)) {
  Corr_list[[i]]$Response <- response[i]
}

Corr_list <- rbindlist(Corr_list) # convert to dataframe

# Same color of figure 5
MyColors <- c("#8e0152", "#bf812d", "#4d4d4d", "#d6604d")
names(MyColors) <- c('N. evidence II', 'N. preserved\nspecimens',
                     'N. pages', 'N. taxa\ncompared')

# Make the plot
MyCorrelogram <- ggplot(Corr_list, aes(x = Distance, y = MoranI_coef, colour = Response)) +
  geom_point(aes(shape = Response, colour = Response))+
  scale_shape_manual(values = c(0, 1, 2, 4, 5)) +
  scale_color_manual(values = MyColors) +  
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
#ggsave(paste0(getwd(), "/figures/FigureS2.PhyloCorrelogram_rodents.jpg"),
#       plot=MyCorrelogram, width=5, height=4, units="in", bg = 'white', dpi = "print")

rm(list = ls()); gc() # clean workspace and garbage collection

# 9) Explore temporal trends in the use of molecular data on Mammal description.----
# Load dataset
#mydata <- fread("Dataset.csv", na.strings = '')
load("Dataset.Rdata")
names(data_all)
dim(data_all)

mydata <- data_all
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
levels(as.factor(mydata$MolMethod)) # 21 levels

# Transforme em fator no próprio dataframe
mydata$MolMethod <- as.character(mydata$MolMethod)
mydata$MolMethod[is.na(mydata$MolMethod)] <- "NA"
mydata$MolMethod <- factor(mydata$MolMethod, levels = mol_levels)

# extend colors
extended_colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(mol_levels))
names(extended_colors) <- mol_levels

all_mammals <- mydata %>%
  group_by(Year, MolMethod) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = MolMethod)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "N. of species", fill = "Molecular Methods") +
  scale_x_discrete(breaks = seq(1990, 2025, 5), expand = expansion(mult = c(0.01, 0))) +
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
        legend.position = 'none'); all_mammals

without_molecular <- mydata %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  group_by(Year, MolMethod) %>%
  summarize(Count = n(), .groups = 'drop') %>%
  ggplot(aes(x = as.factor(Year), y = Count, fill = MolMethod)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "", fill = "Molecular Methods") +
  scale_x_discrete(breaks = seq(1990, 2025, 5), expand = expansion(mult = c(0.01, 0))) +
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
  scale_x_discrete(breaks = seq(1990, 2025, 5), expand = expansion(mult = c(0.01, 0))) +
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
  scale_x_discrete(breaks = seq(1990, 2025, 5), expand = expansion(mult = c(0.01, 0))) +
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

ggsave(paste0(getwd(), "/figures/FigureS4.MolecularMethods.pdf"), 
       plot=fig, width=9, height=6, units="in", dpi = "print", cairo_pdf())

legend_plot <- all_mammals +
  guides(fill = guide_legend(ncol = 6, byrow = TRUE)) +   # ajuste ncol conforme necessário
  theme(legend.position = "bottom",
        legend.box = "horizontal")

ggsave(paste0(getwd(), "/figures/aux.pdf"), 
       plot=legend_plot, width=9, height=6, units="in", dpi = "print", cairo_pdf())

# Obtain proportion
# Mammals
df_prop <- mydata %>%
  select(Year, Molecular) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_molecular = sum(Molecular),
    prop = n_molecular / total
  ) 

cor_spearman <- cor.test(df_prop$Year,
                         df_prop$prop,
                         method = "spearman")
mammals_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                        p = ifelse(cor_spearman$p.value < 0.001, "<0.001", 
                                   round(cor_spearman$p.value, 3)))

# Non-bats & non-rodents
df_prop_without <- mydata %>%
  filter(Order != "Rodentia" & Order != "Chiroptera") %>%
  select(Year, Molecular) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_molecular = sum(Molecular),
    prop = n_molecular / total
  )

cor_spearman <- cor.test(df_prop_without$Year,
                         df_prop_without$prop,
                         method = "spearman")
nonbatsnonrodents_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                            p = ifelse(cor_spearman$p.value < 0.001, "<0.001", 
                                       round(cor_spearman$p.value, 3)))
# bats
df_prop_bats <- mydata %>%
  filter(Order == "Chiroptera") %>%
  select(Year, Molecular) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_molecular = sum(Molecular),
    prop = n_molecular / total
  )

cor_spearman <- cor.test(df_prop_bats$Year,
                         df_prop_bats$prop,
                         method = "spearman")
bats_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                                      p = ifelse(cor_spearman$p.value < 0.001, "<0.001", 
                                                 round(cor_spearman$p.value, 3)))
# Rodents
df_prop_rodents <- mydata %>%
  filter(Order == "Rodentia") %>%
  select(Year, Molecular) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_molecular = sum(Molecular),
    prop = n_molecular / total
  )

cor_spearman <- cor.test(df_prop_rodents$Year,
                         df_prop_rodents$prop,
                         method = "spearman")
rodents_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                         p = ifelse(cor_spearman$p.value < 0.001, "<0.001",
                                    round(cor_spearman$p.value, 3)))

# all mammals 
inset <- df_prop %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") + 
  labs(x = NULL, y = "Prop. spp. described\nwith molecular") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = mammals_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); inset

# non-bats & non-rodents
inset_without <- df_prop_without %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "#ff3352") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#ff3352") + 
  labs(x = NULL, y = "") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = nonbatsnonrodents_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); inset_without

# bats
inset_bats <- df_prop_bats %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "#7fc97f") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#7fc97f") + 
  labs(x = NULL, y = "Prop. spp. described\nwith molecular") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = bats_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); inset_bats

# rodents
inset_rodents <- df_prop_rodents %>%
  ggplot(aes(x = Year, y = prop)) +
  geom_point(size = 2, alpha = 0.5, color = "#386cb0") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.0, color = "#386cb0") + 
  labs(x = NULL, y = "") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = rodents_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); inset_rodents

fig <- cowplot::plot_grid(inset, inset_without, inset_bats, inset_rodents,
                   ncol = 2, nrow = 2, align = "v", labels = "auto"); fig
# Save the plot
ggsave(paste0(getwd(), "/figures/Figure3.MolecularMethods.pdf"), plot=fig, 
       width=9, height=6, units="in", dpi = "print", cairo_pdf())

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
mydata <- data_all

# Spp countries
names(mydata)
nrow(mydata)


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
nrow(SppCountries)
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

mydata <- data_all
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

# Pearson correlation between number of countries and number of authors
mydata_cor <- mydata %>% 
  filter(!is.na(N_authors) & !is.na(N.Countries))
cor.test(mydata_cor$N_authors, mydata_cor$N.Countries, method = "spearman")

# Exloring between taxonomic pratices 
mydata <- mydata %>%
  mutate(TypeOfStudy = paste(TaxonomicReview, Molecular, sep = "_")) %>%
  mutate(TypeOfStudy = case_when(
    TypeOfStudy == "0_0"  ~ "Other \nevidences",
    TypeOfStudy == "0_1" ~ "Molecular",
    TypeOfStudy == "1_1" ~ "Taxonomic \nReview \n+ Molecular",
    TypeOfStudy == "1_0" ~ "Taxonomic \nReview",
    TRUE ~ TypeOfStudy  # mantém o valor original caso não se enquadre em nenhum caso
  )) 

# Duplicando o que é taxonomic review + molecular para cada grupo
mydata_expanded <- mydata %>%
  # Manter os grupos que não precisam ser divididos
  filter(TypeOfStudy != "Taxonomic \nReview \n+ Molecular") %>%
  # Adicionar os registros "Taxonomic Review + Molecular" como "Molecular"
  bind_rows(
    mydata %>%
      filter(TypeOfStudy == "Taxonomic \nReview \n+ Molecular") %>%
      mutate(TypeOfStudy = "Molecular")
  ) %>%
  # Adicionar os registros "Taxonomic Review + Molecular" como "Taxonomic Review"
  bind_rows(
    mydata %>%
      filter(TypeOfStudy == "Taxonomic \nReview \n+ Molecular") %>%
      mutate(TypeOfStudy = "Taxonomic \nReview")
  ) %>%
  mutate(razao_int = N.Countries/N_authors) # variavel nova


## Test difference between between taxonomy practices type ----
# Teste Kruskal-Wallis
res.aov <- mydata_expanded %>%
  rstatix::kruskal_test(razao_int ~ TypeOfStudy) %>%
  mutate(p = ifelse(p < 0.001, "< 0.001", as.character(p)))

# Post-hoc para identificar grupos diferentes
posthoc_test <- mydata_expanded %>%
  rstatix::dunn_test(razao_int ~ TypeOfStudy, p.adjust.method = "bonferroni",
                     detailed = TRUE)
posthoc_test[,c("group1", "group2","estimate", "p.adj", "p.adj.signif")]

# Without bats & rodents
res.aov.without <- mydata_expanded %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  rstatix::kruskal_test(razao_int ~ TypeOfStudy) %>%
  mutate(p = ifelse(p < 0.001, "< 0.001", as.character(p)))
res.aov.without

posthoc_without <- mydata_expanded %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  rstatix::dunn_test(razao_int ~ TypeOfStudy, p.adjust.method = "bonferroni",
                     detailed = TRUE)
posthoc_without[,c("group1", "group2","estimate", "p.adj", "p.adj.signif")]

# Bats
res.aov.bats <- mydata_expanded %>%
  filter(Order == "Chiroptera") %>%
  rstatix::kruskal_test(razao_int ~ TypeOfStudy) #%>%
  #mutate(p = ifelse(p < 0.001, "< 0.001", as.character(p)))

res.aov.bats

pwc_bats <- mydata_expanded %>% 
  filter(Order == "Chiroptera") %>%
  rstatix::dunn_test(razao_int ~ TypeOfStudy, 
                     p.adjust.method = "bonferroni",
                     detailed = TRUE)
pwc_bats[,c("group1", "group2","estimate", "p.adj", "p.adj.signif")]

# Rodents
res.aov.rodents <- mydata_expanded %>%
  filter(Order == "Rodentia") %>%
  rstatix::kruskal_test(razao_int ~ TypeOfStudy) %>%
  mutate(p = ifelse(p < 0.001, "< 0.001", as.character(p)))

res.aov.rodents

pwc_rodentia <- mydata_expanded %>% 
  filter(Order == "Rodentia") %>%
  rstatix::dunn_test(razao_int ~ TypeOfStudy, 
                     p.adjust.method = "bonferroni",
                     detailed = TRUE) 
pwc_rodentia[,c("group1", "group2","estimate", "p.adj", "p.adj.signif")]

## Violinplot plots ----
plot <- mydata_expanded %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = razao_int)) +
  geom_violin(width = 0.8, fill = "black", alpha = 0.5, adjust = 1.5) +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0,4), breaks = c(0,1,2,3,4), expand = expansion(add = c(0, .5))) +
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
  ) +
  geom_text(aes(x = 1, y = 3.2, label = "a"), size = 4, family = "Arial", fontface = "plain") +  # "A" Molecular
  geom_text(aes(x = 2, y = 3.2, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "B" Taxonomic Review 
  geom_text(aes(x = 3, y = 3.2, label = "c"), size = 4, family = "Arial", fontface = "plain") +   # "C" Other evidences
  geom_text(
    data = res.aov,
    aes(
      x = 0.5, 
      y = 4, 
      label = paste(
        "χ2 =",  round(statistic,2),
        "; df =", df, 
        "; Bonferroni p =", p)
    ),
    hjust = 0, vjust = 1.5, size = 4, color = "black", inherit.aes = FALSE
  ); plot

# 
plot_mammals_without <- mydata_expanded %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = razao_int)) +
  geom_violin(width = 0.8, fill = "#ff3352", alpha = 0.5, adjust = 1.5,scale = "width") +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 1, 2, 3, 4), expand = expansion(add = c(0, .5))) +
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
  ) +
  geom_text(aes(x = 1, y = 2.2, label = "a"), size = 4, family = "Arial", fontface = "plain") +  # "A" Molecular
  geom_text(aes(x = 2, y = 2.2, label = "a"), size = 4, family = "Arial", fontface = "plain") +   # "B" Taxonomic Review 
  geom_text(aes(x = 3, y = 3.2, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "C" Other evidences
  geom_text(
    data = res.aov.without,
    aes(
      x = 0.5, 
      y = 4, 
      label = paste(
        "χ2 =",  round(statistic,2),
        "; df =", df, 
        "; Bonferroni p =", p)
    ),
    hjust = 0, vjust = 1.5, size = 4, color = "black", inherit.aes = FALSE
  ) ; plot_mammals_without

plot_bat <- mydata_expanded %>%
  filter(Order == "Chiroptera") %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = razao_int)) +
  geom_violin(width = 0.8, fill = "#7fc97f", alpha = 0.5, adjust = 1.5, scale = "width") +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +
  
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 4), breaks = c(0,1,2,3,4), expand = expansion(add = c(0, .5))) +
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
  ) +
  geom_text(aes(x = 1, y = 3.2, label = "a"), size = 4, family = "Arial", fontface = "plain") +  # "A" Molecular
  geom_text(aes(x = 2, y = 3.2, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "B" Taxonomic Review 
  geom_text(aes(x = 3, y = 2.2, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "C" Other evidences
  geom_text(
    data = res.aov.bats,
    aes(
      x = 0.5, 
      y = 4, 
      label = paste(
        "χ2 =",  round(statistic,2),
        "; df =", df, 
        "; Bonferroni p =", round(p, 3))
    ),
    hjust = 0, vjust = 1.5, size = 4, color = "black", inherit.aes = FALSE
  ) ; plot_bat

plot_rodentia <- mydata_expanded %>%
  filter(Order == "Rodentia") %>%
  mutate(TypeOfStudy = factor(TypeOfStudy,
                              levels = c("Molecular",
                                         "Taxonomic \nReview",
                                         "Other \nevidences"))) %>%
  ggplot(aes(x = TypeOfStudy, y = razao_int)) +
  geom_violin(width = 0.8, fill = "#386cb0", alpha = 0.5, adjust = 1.5) +  # "adjust" suaviza o violino
  geom_boxplot(width = 0.1, color = "black", fill = "white", alpha = 0.1) +

  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_y_continuous(limits = c(0, 4), breaks = c(0, 1, 2, 3, 4), expand = expansion(add = c(0, .5))) +
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
  ) +
  geom_text(aes(x = 1, y = 2.7, label = "a"), size = 4, family = "Arial", fontface = "plain") +  # "A" Molecular
  geom_text(aes(x = 2, y = 2.7, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "B" Taxonomic Review 
  geom_text(aes(x = 3, y = 2.2, label = "b"), size = 4, family = "Arial", fontface = "plain") +   # "C" Other evidences
  geom_text(
    data = res.aov.rodents,
    aes(
      x = 0.5, 
      y = 4, 
      label = paste(
        "χ2 =",  round(statistic,2),
        "; df =", df, 
        "; Bonferroni p =", p)
    ),
    hjust = 0, vjust = 1.5, size = 4, color = "black", inherit.aes = FALSE
  ) ; plot_rodentia

fig <- cowplot::plot_grid(plot, plot_mammals_without, plot_bat, plot_rodentia, 
                   ncol = 1, nrow = 4, align = "v", labels = "auto"); fig

# Export the figure:
ggsave(paste0(getwd(), "/figures/Figure5.EvidencesCompare.pdf"),
       plot=fig, width=5, height=12, units="in", dpi = "print", cairo_pdf)

plot_ancova_mammals <- mydata_expanded %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color = TypeOfStudy)) + #, color = TypeOfStudy
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  ) +
  labs(x = NULL, y = expression(Log[10]("N. Countries"))); plot_ancova_mammals

plot_ancova_without <- mydata_expanded %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color = TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = ""
  ) +
  labs(x = NULL, y = NULL); plot_ancova_without 

plot_ancova_bats <- mydata_expanded %>%
  filter(Order == "Chiroptera") %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color = TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  ) +
  labs(x = expression(Log[10]("N. Authors")),
       y = expression(Log[10]("N. Countries")))

plot_ancova_rodents <- mydata_expanded %>%
  filter(Order == "Rodentia") %>%
  ggplot(aes(x = log10(N_authors), y = log10(N.Countries), color = TypeOfStudy)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  ) +
  labs(x = expression(Log[10]("N authors")), y = NULL)

# Plot without legend
plot_grid <- plot_grid(plot_ancova_mammals, plot_ancova_without,
                       plot_ancova_bats, plot_ancova_rodents,
                       ncol = 2, nrow = 2, labels = "auto"); plot_grid

# extract legend
legend_plot <- mydata_expanded %>%
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

## export figure
ggsave(paste0(getwd(), "/figures/FigureS2.Scatterplot_nauthors_ncountries.pdf"),
       plot=plot_grid, width=9, height=6, units="in", dpi = "print", cairo_pdf)

## Collector and authorship ----
# A proporcao de coletor participando de artigos, aumenta ao longo do tempo?
load("Dataset.RData")

# All mammals
all_mammals_data <- data_all %>%
  select(Year, CollectionIsAuthor) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_autor_coletor = sum(CollectionIsAuthor),
    prop_autor_coletor = n_autor_coletor / total
  ) 

cor_spearman <- cor.test(all_mammals_data$Year,
                         all_mammals_data$prop_autor_coletor,
                         method = "spearman")

rho_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                        p = round(signif(cor_spearman$p.value), 3))

all_mammals <- all_mammals_data %>%
  ggplot(aes(x = Year, y = prop_autor_coletor)) +
  geom_point(size = 2, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "black") + 
  labs(x = NULL, y = "Prop. spp. described\nwith collector is also author") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = rho_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); all_mammals

# Non-bats & non-rodents
nonbats_nonrodents_data <- data_all %>%
  filter(Order != "Chiroptera" & Order != "Rodentia") %>%
  select(Year, CollectionIsAuthor) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_autor_coletor = sum(CollectionIsAuthor),
    prop_autor_coletor = n_autor_coletor / total
  )

cor_spearman <- cor.test(nonbats_nonrodents_data$Year,
                         nonbats_nonrodents_data$prop_autor_coletor,
                         method = "spearman")

rho_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                        p = round(signif(cor_spearman$p.value), 3))

nonbats_nonrodents <- nonbats_nonrodents_data %>%
  ggplot(aes(x = Year, y = prop_autor_coletor)) +
  geom_point(size = 2, alpha = 0.5, color = "#ff3352") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#ff3352") + 
  labs(x = NULL, y = NULL) +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = rho_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); nonbats_nonrodents

# Bats
bats_data <- data_all %>%
  filter(Order == "Chiroptera") %>%
  select(Year, CollectionIsAuthor) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_autor_coletor = sum(CollectionIsAuthor),
    prop_autor_coletor = n_autor_coletor / total
  )

bats <- bats_data %>%
  ggplot(aes(x = Year, y = prop_autor_coletor)) +
  geom_point(size = 2, alpha = 0.5, color = "#7fc97f") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#7fc97f") + 
  labs(x = "Year of description", y = "Prop. spp. described\nwith collector is also author") +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = rho_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); bats

# Rodents
rodents_data <- data_all %>%
  filter(Order == "Rodentia") %>%
  select(Year, CollectionIsAuthor) %>%
  remove_missing() %>%
  group_by(Year) %>%
  summarise(
    total = n(),
    n_autor_coletor = sum(CollectionIsAuthor),
    prop_autor_coletor = n_autor_coletor / total
  )

cor_spearman <- cor.test(rodents_data$Year,
                         rodents_data$prop_autor_coletor,
                         method = "pearson")

rho_label <- data.frame(cor = round(cor_spearman$estimate, 2), 
                        p = round(signif(cor_spearman$p.value), 3))

rodents <- rodents_data %>%
  ggplot(aes(x = Year, y = prop_autor_coletor)) +
  geom_point(size = 2, alpha = 0.5, color = "#386cb0") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, color = "#386cb0") + 
  labs(x = "Year of description", y = NULL) +
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(breaks = seq(1990, 2025, by = 5))+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold', size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0)) +
  geom_text(
    data = rho_label,
    aes(
      x = min(data_all$Year, na.rm = TRUE), 
      y = Inf, 
      label = paste("rs =", round(cor, 3),
                    "\nBonferroni p =", p )
    ),
    hjust = 0, vjust = 1.5, size = 3, color = "black", inherit.aes = FALSE
  ); rodents

fig <- cowplot::plot_grid(all_mammals,
                          nonbats_nonrodents,
                          bats,
                          rodents,
                          ncol = 2, nrow = 2,
                          align = "v", labels = "auto"); fig

# Save the plot
ggsave(paste0(getwd(), "/figures/FigureS8.Author&Collector.pdf"), plot=fig, 
       width=9, height=6, units="in", dpi = "print", cairo_pdf()) 
