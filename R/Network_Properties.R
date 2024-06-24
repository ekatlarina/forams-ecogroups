### Network Properties ####

# This script calculates network properties at the node level using the 'bipartite' R library.
# In the final analysis, we only adapt the Ecogroup Species Specificity Index.
# This script also produces the raw plots for Figures 2C, 2E, and 2G of the manuscript.
# The final figure was created in Illustrator.


# install relevant packages for data morphing and calculating network metrics 

install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("bipartite")
install.packages("gplots")

# load libraries

library(dplyr) # used for data manipulation and transformation
library(tidyr) # used for data tidying and reshaping
library(reshape2) # used for data reshaping and aggregation
library(bipartite) # used for analysis of bipartite networks
library(gplots) # used for plotting data

####  1. set paths for source files and configrue the environment. ####

# set the working directory 
# setwd(" YOUR WOKRING DIRECTORY ")

# load dataframes containing planktonic foraminiferal data from Triton database
# for the Atlantic, Pacific, and Indian Oceans

load("Triton_df_Oceans.RData")

# define a sequence of time bins
roundagePleiPl_150K = seq(from = 1.8, to = 3.75, by = 0.15)


####  2. execute function to analyze ecological network properties #####

Eco_Network_Function = function (tb, ls, forHLeco) {
  # Loop through each time bin to analyze network properties
  for (i in 1:length(tb)){
    
    # Generate a frequency table of ecological groups 
    pt = as.data.frame(table(ls[[i]]$round.lat.5D, ls[[i]]$ecogroup))
    pt$Freq = as.numeric(pt$Freq)  # Convert frequency to numeric for manipulation
    # Transform frequency table to a wide format where columns represent ecogroups
    pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))

    # Calculate indices for network properties at the species level
    nodelvl = specieslevel(pt2) 
    ecot = nodelvl[[1]]  # Extract higher level network data (ecogroups)
    ecot$timeslab = tb[i]  # Append time bin information
    ecot$ecot = rownames(ecot)  # Assign row names as ecogroup identifiers
    
    latitude = nodelvl[[2]]  # Extract lower level network data (latitude)
    latitude$timeslab = tb[i]
    latitude$pal.lat = rownames(latitude)  # Assign row names as latitude identifiers
    forHLeco = rbind(forHLeco, latitude)  # Aggregate higher level data across all time bins
    print(tb[i])  # Print the current time bin being processed
  }

  # Return the complete dataframe 
  return(forHLeco)
}


####  3. run bipartite network analysis for the Atlantic Ocean  ####

# Define the latitudinal range for the Atlantic Ocean analysis
lat.range_AO = seq(-40, 60, 5)

# Initialize a list to store data frames filtered by 150k year time bins from 1.8 to 3.9 million years ago
timebinsPleiPli_AO = list()

# Populate the list with subsets of Atlantic Ocean data corresponding to each time bin
for (i in 1:length(roundagePleiPl_150K)) {
  timebinsPleiPli_AO[[i]] = Triton_Atlantic[which(Triton_Atlantic$round.150K == roundagePleiPl_150K[i]),]
}

# Prepare a null data frame for storing latitudinal properties of each time bin's network analysis
forHL_eco_PliPL_150k_AO = NULL

# Perform the network function to analyze ecological and latitudinal interactions
forHLeco_AO = Eco_Network_Function(tb = roundagePleiPl_150K, ls = timebinsPleiPli_AO, 
                                   forHLeco = forHL_eco_PliPL_150k_AO)

####  4. run bipartite network analysis for the Pacific Ocean ####

# Define the latitudinal range for the Pacific Ocean
lat.range_PO = seq(-45, 40, 5)

# Initialize list for storing Pacific Ocean data frames filtered by 150K year time bins
timebinsPleiPli_PO = list()

# Populate list with subsets of Pacific Ocean data for each time bin
for (i in 1:length(roundagePleiPl_150K)) {
  timebinsPleiPli_PO[[i]] = Triton_Pacific[which(Triton_Pacific$round.150K == roundagePleiPl_150K[i]),]
}

# Prepare a null data frame for latitudinal properties analysis
forHL_eco_PliPL_150k_PO = NULL

# Calculate ecological networks for each time bin in the Pacific Ocean
forHLeco_PO = Eco_Network_Function(tb = roundagePleiPl_150K, ls = timebinsPleiPli_PO, 
                                   forHLeco = forHL_eco_PliPL_150k_PO)

####  5. run bipartite network analysis for the Indian Ocean ####

# Set the latitudinal range for the Indian Ocean
lat.range_IO = seq(-45, 15, 5)

# Initialize list for Indian Ocean data frames filtered by 100k year time bins from 1.8 to 4 Ma
timebinsPleiPli_IO = list()

# Populate list with data subsets for each time bin
for (i in 1:length(roundagePleiPl_150K)) {
  timebinsPleiPli_IO[[i]] = Triton_Indian[which(Triton_Indian$round.150K == roundagePleiPl_150K[i]),]
}

# Prepare a null data frame for analyzing latitudinal properties in each time bin
forHL_eco_PliPL_150k_IO = NULL

# Compute network properties for the Indian Ocean ecological networks
forHLeco_IO = Eco_Network_Function(tb = roundagePleiPl_150K, ls = timebinsPleiPli_IO, 
                                   forHLeco = forHL_eco_PliPL_150k_IO)

##### 6. plotting the heatmap for the Atlantic Ocean ####

# Define color gradient function for heatmap
colfunc = colorRampPalette(c("royalblue4", "grey95", "red3"))

# Convert time bins to factor and prepare data frame for heatmap creation
forHLeco_AO$timeslab = as.factor(forHLeco_AO$timeslab)
x = paste0(lat.range_AO)
y = paste0(unique(forHLeco_AO$timeslab))
data = expand.grid(pal.lat = x, timeslab = y)  # Create a grid 

# Join data with ecological properties and reshape for heatmap
data_wide_AO <- data %>%
  left_join(forHLeco_AO, by = c("pal.lat", "timeslab")) %>%
  mutate(pal.lat = factor(pal.lat, levels = x)) %>%
  acast(pal.lat ~ timeslab, value.var = "species.specificity.index")

# Generate the heatmap for the Atlantic Ocean
heatmap.2(data_wide_AO, col=colfunc(80), trace="none", key.xlab="ESI", key=TRUE, dendrogram="none",
          Rowv=NULL, Colv=NULL, density.info="none", revC=TRUE, main="ESI Atlantic")

##### 7. plotting heatmap for the Pacific Ocean ####

# Convert time bins to factor and prepare data frame for heatmap creation
forHLeco_PO$timeslab = as.factor(forHLeco_PO$timeslab) 
x = paste0(lat.range_PO)
y = paste0(unique(forHLeco_PO$timeslab))
data = expand.grid(pal.lat = x, timeslab = y) # create a grid 

# Join data with ecological properties and reshape for heatmap
data_wide_PO <- data %>%
  left_join(forHLeco_PO, by = c("pal.lat", "timeslab")) %>%
  mutate(pal.lat = factor(pal.lat, levels = x)) %>%
  acast(pal.lat ~ timeslab, value.var = "species.specificity.index")

# Generate the heatmap for the Pacific Ocean
heatmap.2(data_wide_PO,col=colfunc(80), trace="none", key.xlab="ESI",key=TRUE, dendrogram = "none", 
          Rowv = NULL, Colv = NULL, density.info="none", revC = TRUE, main = "ESI Pacific")


##### 8. plotting heatmap for the Indian Ocean ####

# Convert time bins to factor and prepare data frame for heatmap creation
forHLeco_IO$timeslab = as.factor(forHLeco_IO$timeslab) 
x = paste0(lat.range_IO)
y = paste0(unique(forHLeco_IO$timeslab))
data = expand.grid(pal.lat = x, timeslab = y) # create a grid from 

# Join data with ecological properties and reshape for heatmap
data_wide_IO <- data %>%
  left_join(forHLeco_IO, by = c("pal.lat", "timeslab")) %>%
  mutate(pal.lat = factor(pal.lat, levels = x)) %>%
  acast(pal.lat ~ timeslab, value.var = "species.specificity.index")

# Generate the heatmap for the Indian Ocean
heatmap.2(data_wide_IO,col=colfunc(80), trace="none", key.xlab="ESI",key=TRUE, dendrogram = "none", 
          Rowv = NULL, Colv = NULL, density.info="none", revC = TRUE, main = "ESI Indian")




