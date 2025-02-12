# Sample Compleness Estimate by Hemisphere

# This script assesses dataset completeness by examining the 
# sampling completeness of species and ecogroups within each hemisphere 
# across the Atlantic, Pacific, and Indian Oceans, using the iNEXT package. 
# It generates Figures 13 and 14 of the supplementary material. 
# Final figures were created in Illustrator. 

#### Install Required Packages ####

# Install iNEXT and other relevant packages if not already installed
install.packages("iNEXT")
install.packages("devtools")
install.packages("cowplot")
install.packages("vegan")
install.packages("reshape2")
install.packages("dbplyr")
install.packages("tidyverse")

# Install iNEXT from GitHub
install_github("AnneChao/iNEXT")

# Load required libraries
library(devtools)
library(iNEXT)
library(cowplot)
library(vegan)
library(reshape2)
library(dbplyr)
library(tidyverse)

#### Load and Prepare Data ####

# Set working directory and load dataset
setwd("YOUR_WORKING_DIRECTORY")
load("Triton_df_Oceans.RData")

# Filter dataset by hemisphere for each ocean
my_data_AO_NH <- Triton_Atlantic %>% filter(Hemisphere == "Northern")
my_data_AO_SH <- Triton_Atlantic %>% filter(Hemisphere == "Southern")
my_data_PO_NH <- Triton_Pacific %>% filter(Hemisphere == "Northern")
my_data_PO_SH <- Triton_Pacific %>% filter(Hemisphere == "Southern")
my_data_IO_NH <- Triton_Indian %>% filter(Hemisphere == "Northern")
my_data_IO_SH <- Triton_Indian %>% filter(Hemisphere == "Southern")

#### 1. Create Datasets to Test Sample Coverage for Species and Ecogroups ####

# Extract unique time bins
tb <- unique(Triton_Atlantic$round.150K)

# Create lists to store time bin subsets for each ocean
timebins_AO <- list()
timebins_PO <- list()
timebins_IO <- list()

# Populate lists with subsets of data for each ocean
for (i in 1:length(tb)) {
  timebins_AO[[i]] <- Triton_Atlantic[Triton_Atlantic$round.150K == tb[i],]
  timebins_PO[[i]] <- Triton_Pacific[Triton_Pacific$round.150K == tb[i],]
  timebins_IO[[i]] <- Triton_Indian[Triton_Indian$round.150K == tb[i],]
}

#### 2. Run Sample Coverage Test for the Atlantic Ocean (Species Level) ####

# Initialize a list to store iNEXT results for species-level analysis
Sp_150k_iNext_AO <- list()

# Loop through each time bin and run the sample coverage test
for (i in 1:length(tb)) {
  # Create species abundance table by hemisphere
  pt <- as.data.frame(table(timebins_AO[[i]]$Hemisphere, timebins_AO[[i]]$species))
  pt$Freq <- as.numeric(pt$Freq) 
  
  # Convert to wide format for analysis
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq"))
  
  # Prepare data for iNEXT analysis
  t_df <- as.data.frame(t(tblb))
  
  # Run iNEXT analysis
  list_iNext <- iNEXT(t_df, q = 0, datatype = "abundance")
  
  # Store results
  Sp_150k_iNext_AO[[i]] <- list_iNext
}

#### Extract Observed Sample Coverage ####

# Initialize a data frame to store observed sample coverage for each time bin
sp_observed_coverage_per_bin_AO <- data.frame(
  time_bins = tb,
  Northern = NA,
  Southern = NA
)

# Loop through each bin and extract observed sample coverage
for (i in 1:length(tb)) {
  # Extract coverage-based data for this time bin
  coverage_summary <- Sp_150k_iNext_AO[[i]]$iNextEst$coverage_based
  
  # Extract observed sample coverage for Northern and Southern Hemispheres
  observed_row_NH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Northern", ]
  
  observed_row_SH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Southern", ]
  
  # Store sample coverage values
  sp_observed_coverage_per_bin_AO[i, 2] <- observed_row_NH$SC
  sp_observed_coverage_per_bin_AO[i, 3] <- observed_row_SH$SC
}

# Print observed sample coverage for each time bin
print(sp_observed_coverage_per_bin_AO)

# Print range of sample coverage values for each hemisphere
print(range(sp_observed_coverage_per_bin_AO[,2]))  # Northern Hemisphere
print(range(sp_observed_coverage_per_bin_AO[,3]))  # Southern Hemisphere


##### 3. Run Sample Coverage Test for the Pacific Ocean ####
#### Species-Level Analysis ####

# Initialize a list to store iNEXT results for species-level analysis in the Pacific Ocean
Sp_150k_iNext_PO <- list()

# Loop through each time bin and run the sample coverage test
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_PO[[i]]$Hemisphere, timebins_PO[[i]]$species))
  pt$Freq <- as.numeric(pt$Freq) 
  
  # Convert to wide format for analysis
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq"))
  
  # Prepare data for iNEXT analysis
  t_df <- as.data.frame(t(tblb))
  
  # Run iNEXT analysis
  list_iNext <- iNEXT(t_df, q = 0, datatype = "abundance")
  
  # Store results
  Sp_150k_iNext_PO[[i]] <- list_iNext
}

#### Extract Observed Sample Coverage ####

# Initialize a data frame to store observed sample coverage for each time bin
sp_observed_coverage_per_bin_PO <- data.frame(
  time_bins = tb,
  Northern = NA,  # Sample coverage for the Northern Hemisphere
  Southern = NA   # Sample coverage for the Southern Hemisphere
)

# Loop through each bin and extract observed sample coverage
for (i in 1:length(tb)) {
  # Extract coverage-based data for this time bin
  coverage_summary <- Sp_150k_iNext_PO[[i]]$iNextEst$coverage_based
  
  # Extract observed sample coverage for Northern and Southern Hemispheres
  observed_row_NH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Northern", ]
  
  observed_row_SH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Southern", ]
  
  # Store sample coverage values
  sp_observed_coverage_per_bin_PO[i, 2] <- observed_row_NH$SC
  sp_observed_coverage_per_bin_PO[i, 3] <- observed_row_SH$SC
}

# Print observed sample coverage for each time bin
print(sp_observed_coverage_per_bin_PO)

#### 4. Run Sample Coverage Test for the Indian Ocean ####
#### Species-Level Analysis ####

# Initialize a list to store iNEXT results for species-level analysis in the Indian Ocean
Sp_150k_iNext_IO <- list()

# Loop through each time bin and run the sample coverage test
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_IO[[i]]$Hemisphere, timebins_IO[[i]]$species))
  pt$Freq <- as.numeric(pt$Freq) 
  
  # Convert to wide format for analysis
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq"))
  
  # Prepare data for iNEXT analysis
  t_df <- as.data.frame(t(tblb))
  
  # Run iNEXT analysis
  list_iNext <- iNEXT(t_df, q = 0, datatype = "abundance")
  
  # Store results
  Sp_150k_iNext_IO[[i]] <- list_iNext
}

#### Extract Observed Sample Coverage ####

# Initialize a data frame to store observed sample coverage for each time bin
sp_observed_coverage_per_bin_IO <- data.frame(
  time_bins = tb,
  Northern = NA,  # Sample coverage for the Northern Hemisphere
  Southern = NA   # Sample coverage for the Southern Hemisphere
)

# Loop through each bin and extract observed sample coverage
for (i in 1:length(tb)) {
  # Extract coverage-based data for this time bin
  coverage_summary <- Sp_150k_iNext_IO[[i]]$iNextEst$coverage_based
  
  # Extract observed sample coverage for Northern and Southern Hemispheres
  observed_row_NH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Northern", ]
  
  observed_row_SH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Southern", ]
  
  # Store sample coverage values
  sp_observed_coverage_per_bin_IO[i, 2] <- observed_row_NH$SC
  sp_observed_coverage_per_bin_IO[i, 3] <- observed_row_SH$SC
}

# Print observed sample coverage for each time bin
print(sp_observed_coverage_per_bin_IO)

#### 5.Plotting Sample Coverage (Species Level) ####

#Atlantic Ocean
plot_sp_SC_AO <- ggplot(data = sp_observed_coverage_per_bin_AO, aes(x = time_bins)) +
   geom_line(aes(y = Northern, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Northern, color = "Northern Hemisphere")) +
  geom_line(aes(y = Southern, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Southern, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Sample Coverage", color = "Region") +
  ggtitle("Sample Coverage: Atlantic Ocean - Species Diversity")

plot_sp_SC_AO

#Pacific Ocean
plot_sp_SC_PO <- ggplot(data = sp_observed_coverage_per_bin_PO, 
                        aes(x = time_bins)) +
   geom_line(aes(y = Northern, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Northern, color = "Northern Hemisphere")) +
  geom_line(aes(y = Southern, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Southern, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Sample Coverage", color = "Region") +
  ggtitle("Sample Coverage: Pacific Ocean - Species Diversity")

plot_sp_SC_PO

#Indian Ocean
plot_sp_SC_IO <- ggplot(data = sp_observed_coverage_per_bin_IO, 
                        aes(x = time_bins)) +
   geom_line(aes(y = Northern, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Northern, color = "Northern Hemisphere")) +
  geom_line(aes(y = Southern, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Southern, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Sample Coverage", color = "Region") +
  ggtitle("Sample Coverage: Indian Ocean - Species Diversity")

plot_sp_SC_IO

plots_sp <- plot_grid(plot_sp_SC_AO, plot_sp_SC_PO, plot_sp_SC_IO, ncol = 1, 
                      align = "v")
plots_sp



#### 6. Run Sample Coverage Test for the Atlantic Ocean ####
#### Ecogroup-Level Analysis ####

# Initialize a list to store iNEXT results for ecogroup-level analysis in the Atlantic Ocean
Eco_150k_iNext_AO <- list()

# Loop through each time bin and run the sample coverage test
for (i in 1:length(tb)) {
  # Create an ecogroup abundance table per hemisphere
  pt <- as.data.frame(table(timebins_AO[[i]]$Hemisphere, timebins_AO[[i]]$ecogroup))
  pt$Freq <- as.numeric(pt$Freq) 
  
  # Convert to wide format for analysis
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq"))
  
  # Prepare data for iNEXT analysis
  t_df <- as.data.frame(t(tblb))
  
  # Run iNEXT analysis
  list_iNext <- iNEXT(t_df, q = 0, datatype = "abundance")
  
  # Store results
  Eco_150k_iNext_AO[[i]] <- list_iNext
}

#### Extract Observed Sample Coverage ####

# Initialize a data frame to store observed sample coverage for each time bin
eco_observed_coverage_per_bin_AO <- data.frame(
  time_bins = tb,
  Northern = NA,  # Sample coverage for the Northern Hemisphere
  Southern = NA   # Sample coverage for the Southern Hemisphere
)

# Loop through each bin and extract observed sample coverage
for (i in 1:length(tb)) {
  # Extract coverage-based data for this time bin
  coverage_summary <- Eco_150k_iNext_AO[[i]]$iNextEst$coverage_based
  
  # Extract observed sample coverage for Northern and Southern Hemispheres
  observed_row_NH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Northern", ]
  
  observed_row_SH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Southern", ]
  
  # Store sample coverage values
  eco_observed_coverage_per_bin_AO[i, 2] <- observed_row_NH$SC
  eco_observed_coverage_per_bin_AO[i, 3] <- observed_row_SH$SC
}

# Print observed sample coverage for each time bin
print(eco_observed_coverage_per_bin_AO)

#### 7. Run Sample Coverage Test for the Pacific Ocean ####
#### Ecogroup-Level Analysis ####

# Initialize a list to store iNEXT results for ecogroup-level analysis in the Pacific Ocean
Eco_150k_iNext_PO <- list()

# Loop through each time bin and run the sample coverage test
for (i in 1:length(tb)) {
  # Create an ecogroup abundance table per hemisphere
  pt <- as.data.frame(table(timebins_PO[[i]]$Hemisphere, timebins_PO[[i]]$ecogroup))
  pt$Freq <- as.numeric(pt$Freq) 
  
  # Convert to wide format for analysis
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq"))
  
  # Prepare data for iNEXT analysis
  t_df <- as.data.frame(t(tblb))
  
  # Run iNEXT analysis
  list_iNext <- iNEXT(t_df, q = 0, datatype = "abundance")
  
  # Store results
  Eco_150k_iNext_PO[[i]] <- list_iNext
}

#### Extract Observed Sample Coverage ####

# Initialize a data frame to store observed sample coverage for each time bin
eco_observed_coverage_per_bin_PO <- data.frame(
  time_bins = tb,
  Northern = NA,  # Sample coverage for the Northern Hemisphere
  Southern = NA   # Sample coverage for the Southern Hemisphere
)

# Loop through each bin and extract observed sample coverage
for (i in 1:length(tb)) {
  # Extract coverage-based data for this time bin
  coverage_summary <- Eco_150k_iNext_PO[[i]]$iNextEst$coverage_based
  
  # Extract observed sample coverage for Northern and Southern Hemispheres
  observed_row_NH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Northern", ]
  
  observed_row_SH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Southern", ]
  
  # Store sample coverage values
  eco_observed_coverage_per_bin_PO[i, 2] <- observed_row_NH$SC
  eco_observed_coverage_per_bin_PO[i, 3] <- observed_row_SH$SC
}

# Print observed sample coverage for each time bin
print(eco_observed_coverage_per_bin_PO)

#### 8. Run Sample Coverage Test for the Indian Ocean ####
#### Ecogroup-Level Analysis ####

# Initialize a list to store iNEXT results for ecogroup-level analysis in the Indian Ocean
Eco_150k_iNext_IO <- list()

# Loop through each time bin and run the sample coverage test
for (i in 1:length(tb)) {
  # Create an ecogroup abundance table per hemisphere
  pt <- as.data.frame(table(timebins_IO[[i]]$Hemisphere, timebins_IO[[i]]$ecogroup))
  pt$Freq <- as.numeric(pt$Freq) 
  
  # Convert to wide format for analysis
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq"))
  
  # Prepare data for iNEXT analysis
  t_df <- as.data.frame(t(tblb))
  
  # Run iNEXT analysis
  list_iNext <- iNEXT(t_df, q = 0, datatype = "abundance")
  
  # Store results
  Eco_150k_iNext_IO[[i]] <- list_iNext
}

#### Extract Observed Sample Coverage ####

# Initialize a data frame to store observed sample coverage for each time bin
eco_observed_coverage_per_bin_IO <- data.frame(
  time_bins = tb,
  Northern = NA,  # Sample coverage for the Northern Hemisphere
  Southern = NA   # Sample coverage for the Southern Hemisphere
)

# Loop through each bin and extract observed sample coverage
for (i in 1:length(tb)) {
  # Extract coverage-based data for this time bin
  coverage_summary <- Eco_150k_iNext_IO[[i]]$iNextEst$coverage_based
  
  # Extract observed sample coverage for Northern and Southern Hemispheres
  observed_row_NH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Northern", ]
  
  observed_row_SH <- coverage_summary[coverage_summary$Method == "Observed" &
                                        coverage_summary$Assemblage == "Southern", ]
  
  # Store sample coverage values
  eco_observed_coverage_per_bin_IO[i, 2] <- observed_row_NH$SC
  eco_observed_coverage_per_bin_IO[i, 3] <- observed_row_SH$SC
}

# Print observed sample coverage for each time bin
print(eco_observed_coverage_per_bin_IO)


#### Plotting Sample Coverage (Ecogroup Level) ####
plot_eco_SC_AO <- ggplot(data = eco_observed_coverage_per_bin_AO, aes(x = time_bins)) +
   geom_line(aes(y = Northern, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Northern, color = "Northern Hemisphere")) +
  geom_line(aes(y = Southern, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Southern, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Sample Coverage", color = "Region") +
  ggtitle("Sample Coverage: Atlantic Ocean - Ecogroup Diversity")

plot_eco_SC_AO

plot_eco_SC_PO <- ggplot(data = eco_observed_coverage_per_bin_PO, 
                        aes(x = time_bins)) +
   geom_line(aes(y = Northern, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Northern, color = "Northern Hemisphere")) +
  geom_line(aes(y = Southern, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Southern, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Sample Coverage", color = "Region") +
  ggtitle("Sample Coverage: Pacific Ocean - Ecogroup Diversity")

plot_eco_SC_PO

plot_eco_SC_IO <- ggplot(data = eco_observed_coverage_per_bin_IO, 
                        aes(x = time_bins)) +
   geom_line(aes(y = Northern, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Northern, color = "Northern Hemisphere")) +
  geom_line(aes(y = Southern, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Southern, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Sample Coverage", color = "Region") +
  ggtitle("Sample Coverage: Indian Ocean - Ecogroup Diversity")

plot_eco_SC_IO

# Combine the plots using plot_grid

plots_eco <- plot_grid(plot_eco_SC_AO, plot_eco_SC_PO, plot_eco_SC_IO, ncol = 1, align = "v")

plots_eco

