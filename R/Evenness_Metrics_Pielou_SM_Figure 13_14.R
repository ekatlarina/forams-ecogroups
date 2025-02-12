# Evenness Metrics - Pielou

# This script calculates the Pielou's evenness metric for species and ecogroups.
# It also generates raw plots for Figures 15 and 16 in the supplementary material.
# Final figures were created in Illustrator.

# install required packages

install.packages("vegan")
install.packages("dplyr")
install.packages("reshape2")
install.packages("cowplot")
install.packages("ggplot2")

library(vegan)
library(reshape2)
library(dplyr)
library(cowplot)
library(ggplot2)

# set up working directory
setwd("YOUR_WORKING_DIRECTORY")

# load the data
load("Triton_df_Oceans.RData")

#### 1. Create datasets to test sample coverage for species and ecogroups ####
# Define the number of time bins

# Filter dataframes by hemisphere for each ocean
my_data_AO_NH = Triton_Atlantic %>% filter(Hemisphere == "Northern")
my_data_AO_SH = Triton_Atlantic %>% filter(Hemisphere == "Southern")
my_data_PO_NH = Triton_Pacific %>% filter(Hemisphere == "Northern")
my_data_PO_SH = Triton_Pacific %>% filter(Hemisphere == "Southern")
my_data_IO_NH = Triton_Indian %>% filter(Hemisphere == "Northern")
my_data_IO_SH = Triton_Indian %>% filter(Hemisphere == "Southern")

# Extract unique time bins
tb = unique(Triton_Atlantic$round.150K)

# Create lists to store subsets of data for each ocean
timebins_AO = list()
timebins_PO = list()
timebins_IO = list()

# Populate lists with subsets of data based on time bins
for (i in 1:length(tb)) {
  timebins_AO[[i]] = Triton_Atlantic[Triton_Atlantic$round.150K == tb[i],]
  timebins_PO[[i]] = Triton_Pacific[Triton_Pacific$round.150K == tb[i],]
  timebins_IO[[i]] = Triton_Indian[Triton_Indian$round.150K == tb[i],]
}



#### 2. Species Diversity: Pielou's Evenness Metric for the Atlantic Ocean ####

# Initialize a data frame to store Pielou's Evenness results for the Atlantic Ocean
pielou_results_AO_sp <- data.frame(
  TimeBin = tb,
  Pielou_NH = numeric(length(tb)),  # Evenness for the Northern Hemisphere
  Pielou_SH = numeric(length(tb))   # Evenness for the Southern Hemisphere
)

# Loop through each time bin to calculate evenness metrics
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_AO[[i]]$Hemisphere, timebins_AO[[i]]$species))
  pt$Freq <- as.numeric(pt$Freq)
  
  # Convert table to wide format
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq", fill = 0))
  
  # Calculate Pielou's Evenness for the Northern Hemisphere
  species_abundance_NH <- tblb[1, ]  # Extract species abundance for NH
  shannon_index_NH <- diversity(species_abundance_NH, index = "shannon")  # Shannon Index
  species_count_NH <- length(species_abundance_NH[species_abundance_NH > 0])  # Number of species
  pielou_j_NH <- ifelse(species_count_NH > 0, shannon_index_NH / log(species_count_NH), NA)  # Pielou’s J
  
  # Store the result for the Northern Hemisphere
  pielou_results_AO_sp$Pielou_NH[i] <- pielou_j_NH
  
  # Calculate Pielou's Evenness for the Southern Hemisphere
  species_abundance_SH <- tblb[2, ]  # Extract species abundance for SH
  shannon_index_SH <- diversity(species_abundance_SH, index = "shannon")
  species_count_SH <- length(species_abundance_SH[species_abundance_SH > 0])
  pielou_j_SH <- ifelse(species_count_SH > 0, shannon_index_SH / log(species_count_SH), NA)
  
  # Store the result for the Southern Hemisphere
  pielou_results_AO_sp$Pielou_SH[i] <- pielou_j_SH
}

# Print the final results for the Atlantic Ocean
print(pielou_results_AO_sp)

#### 3. Species Diversity: Pielou's Evenness Metric for the Pacific Ocean ####

# Initialize a data frame to store Pielou's Evenness results for the Pacific Ocean
pielou_results_PO_sp <- data.frame(
  TimeBin = tb,
  Pielou_NH = numeric(length(tb)),  # Evenness for the Northern Hemisphere
  Pielou_SH = numeric(length(tb))   # Evenness for the Southern Hemisphere
)

# Loop through each time bin to calculate evenness metrics
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_PO[[i]]$Hemisphere, timebins_PO[[i]]$species))
  pt$Freq <- as.numeric(pt$Freq)
  
  # Convert table to wide format
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq", fill = 0))
  
  # Calculate Pielou's Evenness for the Northern Hemisphere
  species_abundance_NH <- tblb[1, ]  # Extract species abundance for NH
  shannon_index_NH <- diversity(species_abundance_NH, index = "shannon")
  species_count_NH <- length(species_abundance_NH[species_abundance_NH > 0])
  pielou_j_NH <- ifelse(species_count_NH > 0, shannon_index_NH / log(species_count_NH), NA)
  
  # Store the result for the Northern Hemisphere
  pielou_results_PO_sp$Pielou_NH[i] <- pielou_j_NH
  
  # Calculate Pielou's Evenness for the Southern Hemisphere
  species_abundance_SH <- tblb[2, ]  # Extract species abundance for SH
  shannon_index_SH <- diversity(species_abundance_SH, index = "shannon")
  species_count_SH <- length(species_abundance_SH[species_abundance_SH > 0])
  pielou_j_SH <- ifelse(species_count_SH > 0, shannon_index_SH / log(species_count_SH), NA)
  
  # Store the result for the Southern Hemisphere
  pielou_results_PO_sp$Pielou_SH[i] <- pielou_j_SH
}

# Print the final results for the Pacific Ocean
print(pielou_results_PO_sp)


#### 4. Species Diversity: Pielou's Evenness Metric for the Indian Ocean ####

# Initialize a data frame to store Pielou's Evenness results for the Indian Ocean
pielou_results_IO_sp <- data.frame(
  TimeBin = tb,
  Pielou_NH = numeric(length(tb)),  # Evenness for the Northern Hemisphere
  Pielou_SH = numeric(length(tb))   # Evenness for the Southern Hemisphere
)

# Loop through each time bin to calculate evenness metrics
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_IO[[i]]$Hemisphere, timebins_IO[[i]]$species))
  pt$Freq <- as.numeric(pt$Freq)
  
  # Convert table to wide format
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq", fill = 0))
  
  # Calculate Pielou's Evenness for the Northern Hemisphere
  species_abundance_NH <- tblb[1, ]  # Extract species abundance for NH
  shannon_index_NH <- diversity(species_abundance_NH, index = "shannon")
  species_count_NH <- length(species_abundance_NH[species_abundance_NH > 0])
  pielou_j_NH <- ifelse(species_count_NH > 0, shannon_index_NH / log(species_count_NH), NA)
  
  # Store the result for the Northern Hemisphere
  pielou_results_IO_sp$Pielou_NH[i] <- pielou_j_NH
  
  # Calculate Pielou's Evenness for the Southern Hemisphere
  species_abundance_SH <- tblb[2, ]  # Extract species abundance for SH
  shannon_index_SH <- diversity(species_abundance_SH, index = "shannon")
  species_count_SH <- length(species_abundance_SH[species_abundance_SH > 0])
  pielou_j_SH <- ifelse(species_count_SH > 0, shannon_index_SH / log(species_count_SH), NA)
  
  # Store the result for the Southern Hemisphere
  pielou_results_IO_sp$Pielou_SH[i] <- pielou_j_SH
}

# Print the final results for the Indian Ocean
print(pielou_results_IO_sp)

#### 5. Plotting Pielou's Evenness for Each Ocean ####

# Create plots for each ocean

# Atlantic Ocean
plot_sp_AO <- ggplot(data = pielou_results_AO_sp, aes(x = TimeBin)) +
   geom_line(aes(y = Pielou_NH, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Pielou_NH, color = "Northern Hemisphere")) +
  geom_line(aes(y = Pielou_SH, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Pielou_SH, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Pielou's Evenness (J)", color = "Region") +
  ggtitle("Pielou's Evenness: Atlantic Ocean - Species Diversity")

plot_sp_AO

#Pacific Ocean
plot_sp_PO <- ggplot(data = pielou_results_PO_sp, aes(x = TimeBin)) +
   geom_line(aes(y = Pielou_NH, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Pielou_NH, color = "Northern Hemisphere")) +
  geom_line(aes(y = Pielou_SH, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Pielou_SH, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Pielou's Evenness (J)", color = "Region") +
  ggtitle("Pielou's Evenness: Pacific Ocean - Species Diversity")

plot_sp_PO

plot_sp_IO <- ggplot(data = pielou_results_IO_sp, aes(x = TimeBin)) +
   geom_line(aes(y = Pielou_NH, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Pielou_NH, color = "Northern Hemisphere")) +
  geom_line(aes(y = Pielou_SH, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Pielou_SH, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Pielou's Evenness (J)", color = "Region") +
  ggtitle("Pielou's Evenness: Indian Ocean - Species Diversity")

plot_sp_IO


plots_sp <- plot_grid(plot_sp_AO, plot_sp_PO, plot_sp_IO, ncol = 1, 
                      align = "v")
plots_sp


#### 6. Ecogroup Diversity: Pielou's Evenness Metric for the Atlantic Ocean ####

# Initialize a data frame to store Pielou's Evenness results for ecogroups in the Atlantic Ocean
pielou_results_AO_eco <- data.frame(
  TimeBin = tb,
  Pielou_NH = numeric(length(tb)),  # Evenness for the Northern Hemisphere
  Pielou_SH = numeric(length(tb))   # Evenness for the Southern Hemisphere
)

# Loop through each time bin to calculate evenness metrics
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_AO[[i]]$Hemisphere, timebins_AO[[i]]$ecogroup))
  pt$Freq <- as.numeric(pt$Freq)
  
  # Convert table to wide format
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq", fill = 0))
  
  # Calculate Pielou's Evenness for the Northern Hemisphere
  species_abundance_NH <- tblb[1, ]  # Extract species abundance for NH
  shannon_index_NH <- diversity(species_abundance_NH, index = "shannon")
  species_count_NH <- length(species_abundance_NH[species_abundance_NH > 0])
  pielou_j_NH <- ifelse(species_count_NH > 0, shannon_index_NH / log(species_count_NH), NA)
  
  # Store the result for the Northern Hemisphere
  pielou_results_AO_eco$Pielou_NH[i] <- pielou_j_NH
  
  # Calculate Pielou's Evenness for the Southern Hemisphere
  species_abundance_SH <- tblb[2, ]  # Extract species abundance for SH
  shannon_index_SH <- diversity(species_abundance_SH, index = "shannon")
  species_count_SH <- length(species_abundance_SH[species_abundance_SH > 0])
  pielou_j_SH <- ifelse(species_count_SH > 0, shannon_index_SH / log(species_count_SH), NA)
  
  # Store the result for the Southern Hemisphere
  pielou_results_AO_eco$Pielou_SH[i] <- pielou_j_SH
}

# Print the final results for the Atlantic Ocean
print(pielou_results_AO_eco)

#### 7. Ecogroup Diversity: Pielou's Evenness Metric for the Pacific Ocean ####

# Initialize a data frame to store Pielou's Evenness results for ecogroups in the Pacific Ocean
pielou_results_PO_eco <- data.frame(
  TimeBin = tb,
  Pielou_NH = numeric(length(tb)),  # Evenness for the Northern Hemisphere
  Pielou_SH = numeric(length(tb))   # Evenness for the Southern Hemisphere
)

# Loop through each time bin to calculate evenness metrics
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_PO[[i]]$Hemisphere, timebins_PO[[i]]$ecogroup))
  pt$Freq <- as.numeric(pt$Freq)
  
  # Convert table to wide format
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq", fill = 0))
  
  # Calculate Pielou's Evenness for the Northern Hemisphere
  species_abundance_NH <- tblb[1, ]  # Extract species abundance for NH
  shannon_index_NH <- diversity(species_abundance_NH, index = "shannon")
  species_count_NH <- length(species_abundance_NH[species_abundance_NH > 0])
  pielou_j_NH <- ifelse(species_count_NH > 0, shannon_index_NH / log(species_count_NH), NA)
  
  # Store the result for the Northern Hemisphere
  pielou_results_PO_eco$Pielou_NH[i] <- pielou_j_NH
  
  # Calculate Pielou's Evenness for the Southern Hemisphere
  species_abundance_SH <- tblb[2, ]  # Extract species abundance for SH
  shannon_index_SH <- diversity(species_abundance_SH, index = "shannon")
  species_count_SH <- length(species_abundance_SH[species_abundance_SH > 0])
  pielou_j_SH <- ifelse(species_count_SH > 0, shannon_index_SH / log(species_count_SH), NA)
  
  # Store the result for the Southern Hemisphere
  pielou_results_PO_eco$Pielou_SH[i] <- pielou_j_SH
}

# Print the final results for the Pacific Ocean
print(pielou_results_PO_eco)

#### 8. Ecogroup Diversity: Pielou's Evenness Metric for the Indian Ocean ####

# Initialize a data frame to store Pielou's Evenness results for ecogroups in the Indian Ocean
pielou_results_IO_eco <- data.frame(
  TimeBin = tb,
  Pielou_NH = numeric(length(tb)),  # Evenness for the Northern Hemisphere
  Pielou_SH = numeric(length(tb))   # Evenness for the Southern Hemisphere
)

# Loop through each time bin to calculate evenness metrics
for (i in 1:length(tb)) {
  # Create a species abundance table per hemisphere
  pt <- as.data.frame(table(timebins_IO[[i]]$Hemisphere, timebins_IO[[i]]$ecogroup))
  pt$Freq <- as.numeric(pt$Freq)
  
  # Convert table to wide format
  tblb <- as.data.frame(acast(pt, Var1 ~ Var2, value.var = "Freq", fill = 0))
  
  # Calculate Pielou's Evenness for the Northern Hemisphere
  species_abundance_NH <- tblb[1, ]  # Extract species abundance for NH
  shannon_index_NH <- diversity(species_abundance_NH, index = "shannon")
  species_count_NH <- length(species_abundance_NH[species_abundance_NH > 0])
  pielou_j_NH <- ifelse(species_count_NH > 0, shannon_index_NH / log(species_count_NH), NA)
  
  # Store the result for the Northern Hemisphere
  pielou_results_IO_eco$Pielou_NH[i] <- pielou_j_NH
  
  # Calculate Pielou's Evenness for the Southern Hemisphere
  species_abundance_SH <- tblb[2, ]  # Extract species abundance for SH
  shannon_index_SH <- diversity(species_abundance_SH, index = "shannon")
  species_count_SH <- length(species_abundance_SH[species_abundance_SH > 0])
  pielou_j_SH <- ifelse(species_count_SH > 0, shannon_index_SH / log(species_count_SH), NA)
  
  # Store the result for the Southern Hemisphere
  pielou_results_IO_eco$Pielou_SH[i] <- pielou_j_SH
}

# Print the final results for the Indian Ocean
print(pielou_results_IO_eco)

#### 9. Plotting Pielou's Evenness for Each Ocean ####

plot_eco_AO <- ggplot(data = pielou_results_AO_eco, aes(x = TimeBin)) +
   geom_line(aes(y = Pielou_NH, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Pielou_NH, color = "Northern Hemisphere")) +
  geom_line(aes(y = Pielou_SH, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Pielou_SH, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Pielou's Evenness (J)", color = "Region") +
  ggtitle("Pielou's Evenness: Atlantic Ocean - Ecogroup Diversity")

plot_eco_AO

plot_eco_PO <- ggplot(data = pielou_results_PO_eco, aes(x = TimeBin)) +
   geom_line(aes(y = Pielou_NH, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Pielou_NH, color = "Northern Hemisphere")) +
  geom_line(aes(y = Pielou_SH, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Pielou_SH, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Pielou's Evenness (J)", color = "Region") +
  ggtitle("Pielou's Evenness: Pacific Ocean - Ecogroup Diversity")

plot_eco_PO

plot_eco_IO <- ggplot(data = pielou_results_IO_eco, aes(x = TimeBin)) +
   geom_line(aes(y = Pielou_NH, color = "Northern Hemisphere"), 
             linetype = "solid") +
  geom_point(aes(y = Pielou_NH, color = "Northern Hemisphere")) +
  geom_line(aes(y = Pielou_SH, color = "Southern Hemisphere"), 
            linetype = "solid") +
  geom_point(aes(y = Pielou_SH, color = "Southern Hemisphere")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", 
                                "Southern Hemisphere" = "#2b7dac")) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Pielou's Evenness (J)", color = "Region") +
  ggtitle("Pielou's Evenness: Indian Ocean - Ecogroup Diversity")

plot_eco_IO

plots_eco <- plot_grid(plot_eco_AO, plot_eco_PO, plot_eco_IO, ncol = 1, 
                      align = "v")
plots_eco




