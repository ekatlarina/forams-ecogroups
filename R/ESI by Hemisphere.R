### ESI by Hemisphere ####

# This script analyzes network properties at the node level using the 'bipartite' R package. 
# In the final analysis, we focus on the Ecogroup Specialization Index (ESI), 
# referred to as the Ecogroup Species Specificity Index in the dataset. The script 
# generates raw plots for Figures 2A, 2C, and 2E of the manuscript, which are further 
# refined and finalized in Illustrator.


# install relevant packages for data morphing and calculating network metrics 

install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("bipartite")
install.packages("ggplot2")

# load libraries

library(dplyr) # used for data manipulation and transformation
library(tidyr) # used for data tidying and reshaping
library(reshape2) # used for data reshaping and aggregation
library(bipartite) # used for the analysis of bipartite networks
library(ggplot2) # used for plotting data

#### 1. Set paths for source files and configure the environment. ####

# set the working directory 
# setwd(" YOUR WORKING DIRECTORY ")
setwd("/Users/ekaterina.larina/Dropbox/Nature Geo 2024/RData files")

# load data frames containing planktic foraminiferal data from the Triton database
# for the Atlantic, Pacific, and Indian Oceans

load("Triton_df_Oceans.RData")

# define a sequence of time bins
roundage = seq(from = 1.8, to = 3.75, by = 0.15)

#### 2. Create lists for each basin ####

timebinsPleiPli_AO = list()

for (i in 1:length(roundage)) {
  
 timebinsPleiPli_AO[[i]] = Triton_Atlantic[which(Triton_Atlantic$round.150K==roundage[i]),]
}

timebinsPleiPli_PO = list()

for (i in 1:length(roundage)) {
  
 timebinsPleiPli_PO[[i]] = Triton_Pacific[which(Triton_Pacific$round.150K==roundage[i]),]
}

timebinsPleiPli_IO = list()

for (i in 1:length(roundage)) {
  
 timebinsPleiPli_IO[[i]] = Triton_Indian[which(Triton_Indian$round.150K==roundage[i]),]
}

#### 3. Run the network for Atlantic, Pacific and Indian Oceans ####

#### Atlantic Ocean ####

forLL_eco_PliPL_150k_AO = NULL #eco - ecotype properties for each time bin network
forHL_eco_PliPL_150k_AO = NULL #eco - latitudinal properties for each time bin network

for (i in 1:length(roundage)){
  
  # creating properties for ecogroups
  pt = as.data.frame(table(timebinsPleiPli_AO[[i]]$Hemisphere, timebinsPleiPli_AO[[i]]$ecogroup)) # table with counts of ecogroups 
  pt$Freq = as.numeric(pt$Freq) # converting column Freq(how many times each ecogroup occurs) numeric
  pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq")) # creating a wide table where columns are ecogroups
  
  # calculate various indices for network properties at the species level specieslevel()
  nodelvl = specieslevel(pt2) 
  ecot = nodelvl[[1]] #higher level - ecogroup
  ecot$timeslab = roundage[i] # assigning time bins
  ecot$ecot = rownames(ecot) 
  forLL_eco_PliPl_150k_AO = rbind(forLL_eco_PliPL_150k_AO,ecot)
  
  latitude = nodelvl[[2]] # lower level latitude
  latitude$timeslab = roundage[i]
  latitude$pal.lat = rownames(latitude)
  forHL_eco_PliPL_150k_AO = rbind(forHL_eco_PliPL_150k_AO,latitude) # creating a table with all time bins
  print(roundage[i])

}

#### Pacific Ocean ####

forLL_eco_PliPL_150k_PO = NULL #eco - ecotype properties for each timebin network
forHL_eco_PliPL_150k_PO = NULL #eco - latitudinal properties for each timebin network


for (i in 1:length(roundage)){
  
  # creating properties for ecogroups
  pt = as.data.frame(table(timebinsPleiPli_PO[[i]]$Hemisphere, timebinsPleiPli_PO[[i]]$ecogroup)) # table with counts of ecogroups 
  pt$Freq = as.numeric(pt$Freq) # converting column Freq(how many times each eacogroup occurs) numeric
  pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq")) # creating a wide table where columns are ecogroups
  
  # calculate various indices for network properties at the species level specieslevel()
  nodelvl = specieslevel(pt2) 
  ecot = nodelvl[[1]] #higher level - ecogroup
  ecot$timeslab = roundage[i] # assigning time bins
  ecot$ecot = rownames(ecot) 
  forLL_eco_PliPl_150k_PO = rbind(forLL_eco_PliPL_150k_PO,ecot)
  
  latitude = nodelvl[[2]] # lower level latitude
  latitude$timeslab = roundage[i]
  latitude$pal.lat = rownames(latitude)
  forHL_eco_PliPL_150k_PO = rbind(forHL_eco_PliPL_150k_PO,latitude) # creating a table with all time bins
  print(roundage[i])
}

#### Indian Ocean ####

forLL_eco_PliPL_150k_IO = NULL #eco - ecotype properties for each time bin network
forHL_eco_PliPL_150k_IO = NULL #eco - latitudinal properties for each time bin network


for (i in 1:length(roundage)){
  
  # creating properties for ecogroups
  pt = as.data.frame(table(timebinsPleiPli_IO[[i]]$Hemisphere, timebinsPleiPli_IO[[i]]$ecogroup)) # table with counts of ecogroups 
  pt$Freq = as.numeric(pt$Freq) # converting column Freq(how many times each ecogroup occurs) numeric
  pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq")) # creating a wide table where columns are ecogroups
  
  # calculate various indices for network properties at the species level specieslevel()
  nodelvl = specieslevel(pt2) 
  ecot = nodelvl[[1]] #higher level - ecogroup
  ecot$timeslab = roundage[i] # assigning time bins
  ecot$ecot = rownames(ecot) 
  forLL_eco_PliPl_150k_IO = rbind(forLL_eco_PliPL_150k_IO,ecot)
  
  latitude = nodelvl[[2]] # lower level latitude
  latitude$timeslab = roundage[i]
  latitude$pal.lat = rownames(latitude)
  forHL_eco_PliPL_150k_IO = rbind(forHL_eco_PliPL_150k_IO,latitude) # creating a table with all timeb ins
  print(roundage[i])
}


#### 4. Plot the ESI ####

# set midpoints for plotting
repetitive_midpoints <- c(1.88, 1.88, 2.02, 2.02, 2.17, 2.17, 2.33, 2.33, 2.47, 2.47, 2.62, 2.62, 
                          2.78, 2.78, 2.92, 2.92, 3.08, 3.08, 3.22, 3.22, 3.38, 3.38, 3.53, 3.53, 
                          3.67, 3.67, 3.83, 3.83)

forHL_eco_PliPL_150k_AO$midpoints = repetitive_midpoints
forHL_eco_PliPL_150k_PO$midpoints = repetitive_midpoints
forHL_eco_PliPL_150k_IO$midpoints = repetitive_midpoints


forHL_eco_PliPL_150k_AO$pal.lat <- factor(forHL_eco_PliPL_150k_AO$pal.lat)
forHL_eco_PliPL_150k_PO$pal.lat <- factor(forHL_eco_PliPL_150k_PO$pal.lat)
forHL_eco_PliPL_150k_IO$pal.lat <- factor(forHL_eco_PliPL_150k_IO$pal.lat)

# create plot for the Atlantic Ocean
ESI_plot_AO = ggplot(forHL_eco_PliPL_150k_AO, aes(x = midpoints, 
                                                  y = species.specificity.index, 
                                                  group = pal.lat)) +
  
  # Map color to hemisphere by checking if pal.lat is positive or negative
  geom_point(aes(color = pal.lat), size = 2) +
  geom_line(aes(color = pal.lat), size = 1) +
  
  # Set color for Northern and Southern hemispheres
  scale_color_manual(values = c("Northern" = "#242f33", "Southern" = "#2b7dac")) +
  
  # Customize theme
  theme(legend.position = "top",
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.title = element_blank()) +  # Remove legend title
  
  # Add labels and titles
  labs(x = "Age [Ma]", y = "ESI") +
  
  # Customize x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2)) +
  
  # Set the plot title
  ggtitle("Atlantic ESI")

# Display the plot
ESI_plot_AO

#create plot for the Atlantic Ocean
ESI_plot_PO = ggplot(forHL_eco_PliPL_150k_PO, aes(x = midpoints, y = species.specificity.index,
                                                  group = pal.lat)) +
  # Map color to hemisphere by checking if pal.lat is positive or negative
  geom_point(aes(color = pal.lat), size = 2) +
  geom_line(aes(color = pal.lat), size = 1) +
  
  # Set color for Northern and Southern hemispheres
  scale_color_manual(values = c("Northern" = "#242f33", "Southern" = "#2b7dac")) +
  
  # Customize theme
  theme(legend.position = "top",
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.title = element_blank()) +  # Remove legend title
  
  # Add labels and titles
  labs(x = "Age [Ma]", y = "ESI") +
  
  # Customize x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2)) +
  
  # Set the plot title
  ggtitle("Pacific ESI")

# Display the plot
ESI_plot_PO

# Plot ESI for the Indian Ocean
ESI_plot_IO = ggplot(forHL_eco_PliPL_150k_IO, aes(x = midpoints, y = species.specificity.index,
                                                  group = pal.lat)) +
  # Map color to hemisphere by checking if pal.lat is positive or negative
  geom_point(aes(color = pal.lat), size = 2) +
  geom_line(aes(color = pal.lat), size = 1) +
  
  # Set color for Northern and Southern hemispheres
  scale_color_manual(values = c("Northern" = "#242f33", "Southern" = "#2b7dac")) +
  
  # Customize theme
  theme(legend.position = "top",
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12),
        legend.title = element_blank()) +  # Remove legend title
  
  # Add labels and titles
  labs(x = "Age [Ma]", y = "ESI") +
  
  # Customize x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2)) +
  
  # Set the plot title
  ggtitle("Indian ESI")

# Display the plot
ESI_plot_IO


