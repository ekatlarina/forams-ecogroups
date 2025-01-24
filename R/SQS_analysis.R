# SQS_analysis

# This script calculates the corrected sample-in-bin metric, utilizing the shareholder quorum 
# subsampling (SQS) method. This script also produces the raw plots for Figures 4 of 
# the manuscript and Figures 18 and 19 of the supplementary material. 
# Final figures were created in Illustrator.

install.packages("divDyn")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("cowplot")

# Load necessary libraries
library(divDyn)    # For paleobiological and biodiversity analysis
library(tidyverse) # For data manipulation and visualization
library(magrittr)  # For using the pipe operator `%>%`
library(reshape2)  # For reshaping data between long and wide forms
library(gridExtra) # For arranging multiple grid-based plots
library(cowplot)   # For advanced plot layouts

#### 1. Set paths for source files and configure the environment ####

# Set the working directory to where the data files are stored

#setwd("Your_working_directory")

# Load planktonic foraminiferal data from the Triton database for three major oceans
load("Triton_df_Oceans.RData")

# Assign data to variables by ocean
my_data_AO = Triton_Atlantic
my_data_PO = Triton_Pacific
my_data_IO = Triton_Indian

# Filter dataframes by hemisphere for each ocean
my_data_AO_NH = Triton_Atlantic %>% filter(Hemisphere == "Northern")
my_data_AO_SH = Triton_Atlantic %>% filter(Hemisphere == "Southern")
my_data_PO_NH = Triton_Pacific %>% filter(Hemisphere == "Northern")
my_data_PO_SH = Triton_Pacific %>% filter(Hemisphere == "Southern")
my_data_IO_NH = Triton_Indian %>% filter(Hemisphere == "Northern")
my_data_IO_SH = Triton_Indian %>% filter(Hemisphere == "Southern")

# Define unique time bins of 150,000 years from the Atlantic dataset
tb = unique(Triton_Atlantic$round.150K)

# Collect all dataframes into a list for further processing
data_frames = list(my_data_AO = my_data_AO, my_data_PO = my_data_PO, my_data_IO = my_data_IO,
                    my_data_AO_NH = my_data_AO_NH, my_data_AO_SH = my_data_AO_SH,
                    my_data_PO_NH = my_data_PO_NH, my_data_PO_SH = my_data_PO_SH,
                    my_data_IO_NH = my_data_IO_NH, my_data_IO_SH = my_data_IO_SH)

# Process each dataframe to assign 'bin' values based on time bins
for (i in 1:length(data_frames)) {
  data_frames[[i]]$bin = rep(NA, nrow(data_frames[[i]]))  # Initialize 'bin' column with NA
  
  for (j in 1:length(tb)) {
    store = which(data_frames[[i]]$round.150K == tb[j])
    data_frames[[i]]$bin[store] = j  # Assign bin number based on time bin
  }
}

# Export the updated data frames back to the global environment
list2env(data_frames, envir = .GlobalEnv)


#### 2. Create dataset for plotting with midpoints ####

# Initialize vector with starting numbers for time intervals
numbers = seq(from = 1.8, to = 3.9, by= 0.15)

# Calculate midpoints for each interval to use in plots
midpoints = seq(from = 1.85, to= 3.85, by = 0.15)

######### SPECIES DIVERSITY ################

############################################

#### 3. Run SQS for species diversity and plot the results for the Atlantic Ocean 
#### Run SQS species diversity Atlantic Ocean (Figure 4A and Supplementary Figure 19)  #####

# Perform subsampling and return distributions
SQS_AO = subsample(my_data_AO, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                   coll = "sampleID", output = "dist")
SQS_AO_NH = subsample(my_data_AO_NH, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")
SQS_AO_SH = subsample(my_data_AO_SH, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")

#### Create a function to calculate mean and 90% CI ####
calculate_mean_and_CI <- function(data, variable_name) {
  # Extract the data for the specified variable
  data_matrix <- data[[variable_name]]
  
  # Calculate the mean, lower CI (2.5th percentile), and upper CI (97.5th percentile) for each bin
  mean_values <- apply(data_matrix, 1, mean, na.rm = TRUE)
  lower_CI <- apply(data_matrix, 1, function(x) quantile(x, probs = 0.1, na.rm = TRUE))
  upper_CI <- apply(data_matrix, 1, function(x) quantile(x, probs = 0.9, na.rm = TRUE))
  
  # Create a data frame to store the results
  results <- data.frame(
    Bin = 1:nrow(data_matrix),  # Bin numbers (1 to 14)
    Mean = mean_values,
    Lower_CI = lower_CI,
    Upper_CI = upper_CI
  )
  
  return(results)
}

#### Apply functions ####

# Apply the function to the "divCSIB" variable for each dataset (SQS_AO, SQS_AO_NH, SQS_AO_SH)
SQS_AO_summary <- calculate_mean_and_CI(SQS_AO, "divCSIB")
SQS_AO_NH_summary <- calculate_mean_and_CI(SQS_AO_NH, "divCSIB")
SQS_AO_SH_summary <- calculate_mean_and_CI(SQS_AO_SH, "divCSIB")

SQS_AO_summary$tb = tb
SQS_AO_summary$midpoints = midpoints
SQS_AO_NH_summary$tb = tb
SQS_AO_NH_summary$midpoints = midpoints
SQS_AO_SH_summary$tb = tb
SQS_AO_SH_summary$midpoints = midpoints
                                                               
# Label the hemispheres
SQS_AO_summary$Hemisphere = "Combined"
SQS_AO_NH_summary$Hemisphere = "Northern"
SQS_AO_SH_summary$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_AO_combined = rbind(SQS_AO_summary, SQS_AO_NH_summary, SQS_AO_SH_summary)

#### 4. Plot Species Diversity Atlantic Ocean Main Figure 4A and SM Figure 18 #### 

plot_SQS_AO_CSIB_main <- ggplot(SQS_AO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                linetype = Hemisphere)) +

  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Atlantic q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_AO_CSIB_main

plot_SQS_AO_CSIB_sm <- ggplot(SQS_AO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, linetype = Hemisphere)) +
  
  # Add confidence intervals as shaded ribbon
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Atlantic q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_AO_CSIB_sm

#### 5. Run SQS for species diversity Indian Ocean (Figure 4A and Supplementary Figures)  #####

# Perform subsampling and return distributions
SQS_IO = subsample(my_data_IO, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                   coll = "sampleID", output = "dist")
SQS_IO_NH = subsample(my_data_IO_NH, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")
SQS_IO_SH = subsample(my_data_IO_SH, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")

# Apply the function to the "divCSIB" variable for each dataset (SQS_AO, SQS_AO_NH, SQS_AO_SH)
SQS_IO_summary <- calculate_mean_and_CI(SQS_IO, "divCSIB")
SQS_IO_NH_summary <- calculate_mean_and_CI(SQS_IO_NH, "divCSIB")
SQS_IO_SH_summary <- calculate_mean_and_CI(SQS_IO_SH, "divCSIB")

SQS_IO_summary$tb = tb
SQS_IO_summary$midpoints = midpoints
SQS_IO_NH_summary$tb = tb
SQS_IO_NH_summary$midpoints = midpoints
SQS_IO_SH_summary$tb = tb
SQS_IO_SH_summary$midpoints = midpoints
                                                               
# Label the hemispheres
SQS_IO_summary$Hemisphere = "Combined"
SQS_IO_NH_summary$Hemisphere = "Northern"
SQS_IO_SH_summary$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_IO_combined = rbind(SQS_IO_summary, SQS_IO_NH_summary, SQS_IO_SH_summary)

### 6. Plot Species Diversity Indian Ocean Main Figure 4A and SM #### 

plot_SQS_IO_CSIB_main <- ggplot(SQS_IO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                linetype = Hemisphere)) +

  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Indian q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_IO_CSIB_main

plot_SQS_IO_CSIB_sm <- ggplot(SQS_IO_combined, aes(x = midpoints, y = Mean, color = Hemisphere,
                                                   linetype = Hemisphere)) +
  
  # Remove borders for geom_rect panels
  #geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05, color = NA) +
  #geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05, color = NA) +
  #geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5, color = NA) +
  
  # Add confidence intervals as shaded ribbon
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Indian q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_IO_CSIB_sm

#### 7. Run SQS for species diversity Pacific Ocean ####
# Perform subsampling and return distributions
SQS_PO = subsample(my_data_PO, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                   coll = "sampleID", output = "dist")
SQS_PO_NH = subsample(my_data_PO_NH, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")
SQS_PO_SH = subsample(my_data_PO_SH, iter = 1000, q = 0.8, tax = "species", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")

# Apply the function to the "divCSIB" variable for each dataset (SQS_AO, SQS_AO_NH, SQS_AO_SH)
SQS_PO_summary <- calculate_mean_and_CI(SQS_PO, "divCSIB")
SQS_PO_NH_summary <- calculate_mean_and_CI(SQS_PO_NH, "divCSIB")
SQS_PO_SH_summary <- calculate_mean_and_CI(SQS_PO_SH, "divCSIB")

SQS_PO_summary$tb = tb
SQS_PO_summary$midpoints = midpoints
SQS_PO_NH_summary$tb = tb
SQS_PO_NH_summary$midpoints = midpoints
SQS_PO_SH_summary$tb = tb
SQS_PO_SH_summary$midpoints = midpoints
                                                               
# Label the hemispheres
SQS_PO_summary$Hemisphere = "Combined"
SQS_PO_NH_summary$Hemisphere = "Northern"
SQS_PO_SH_summary$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_PO_combined = rbind(SQS_PO_summary, SQS_PO_NH_summary, SQS_PO_SH_summary)

### 8. Plot Species Diversity Pacific Ocean Main Figure 4A and SM #### 

plot_SQS_PO_CSIB_main <- ggplot(SQS_PO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                linetype = Hemisphere)) +
  # Add confidence intervals as shaded ribbon
 # geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Pacific q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_PO_CSIB_main

plot_SQS_PO_CSIB_sm <- ggplot(SQS_PO_combined, aes(x = midpoints, y = Mean, color = Hemisphere,
                                                   linetype = Hemisphere)) +
  
  # Remove borders for geom_rect panels
  #geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05, color = NA) +
  #geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05, color = NA) +
  #geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5, color = NA) +
  
  # Add confidence intervals as shaded ribbon
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Pacific q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_PO_CSIB_sm


######### ECOGROUP DIVERSITY #############

##########################################

#### 9. Run SQS for ecogroup diversity and plot the results for the Atlantic Ocean 
# (Figure 4B and Supplementary Figure 19)  #####

# Perform subsampling and return distributions
SQS_AO = subsample(my_data_AO, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                   coll = "sampleID", output = "dist")
SQS_AO_NH = subsample(my_data_AO_NH, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")
SQS_AO_SH = subsample(my_data_AO_SH, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")

# Apply the function to the "divCSIB" variable for each dataset (SQS_AO, SQS_AO_NH, SQS_AO_SH)
SQS_AO_summary <- calculate_mean_and_CI(SQS_AO, "divCSIB")
SQS_AO_NH_summary <- calculate_mean_and_CI(SQS_AO_NH, "divCSIB")
SQS_AO_SH_summary <- calculate_mean_and_CI(SQS_AO_SH, "divCSIB")

SQS_AO_summary$tb = tb
SQS_AO_summary$midpoints = midpoints
SQS_AO_NH_summary$tb = tb
SQS_AO_NH_summary$midpoints = midpoints
SQS_AO_SH_summary$tb = tb
SQS_AO_SH_summary$midpoints = midpoints
                                                               
# Label the hemispheres
SQS_AO_summary$Hemisphere = "Combined"
SQS_AO_NH_summary$Hemisphere = "Northern"
SQS_AO_SH_summary$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_AO_combined = rbind(SQS_AO_summary, SQS_AO_NH_summary, SQS_AO_SH_summary)

#### 10. Plot Species Diversity Atlantic Ocean Main Figure 4B and SM Figure 19 #### 

plot_SQS_AO_CSIB_main <- ggplot(SQS_AO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                linetype = Hemisphere)) +
 
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Ecogroup Diversity", title = "SQS CSIB: Atlantic q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_AO_CSIB_main

plot_SQS_AO_CSIB_sm <- ggplot(SQS_AO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                   linetype = Hemisphere)) +
  
  # Add confidence intervals as shaded ribbon
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Ecogroup Diversity", title = "SQS CSIB: Atlantic q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac",  
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_AO_CSIB_sm

#### 11. Run SQS for ecogroup diversity the Indian Ocean (Figure 4B and Supplementary Figures)  #####

# Perform subsampling and return distributions
SQS_IO = subsample(my_data_IO, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                   coll = "sampleID", output = "dist")
SQS_IO_NH = subsample(my_data_IO_NH, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")
SQS_IO_SH = subsample(my_data_IO_SH, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")

# Apply the function to the "divCSIB" variable for each dataset (SQS_AO, SQS_AO_NH, SQS_AO_SH)
SQS_IO_summary <- calculate_mean_and_CI(SQS_IO, "divCSIB")
SQS_IO_NH_summary <- calculate_mean_and_CI(SQS_IO_NH, "divCSIB")
SQS_IO_SH_summary <- calculate_mean_and_CI(SQS_IO_SH, "divCSIB")

SQS_IO_summary$tb = tb
SQS_IO_summary$midpoints = midpoints
SQS_IO_NH_summary$tb = tb
SQS_IO_NH_summary$midpoints = midpoints
SQS_IO_SH_summary$tb = tb
SQS_IO_SH_summary$midpoints = midpoints
                                                               
# Label the hemispheres
SQS_IO_summary$Hemisphere = "Combined"
SQS_IO_NH_summary$Hemisphere = "Northern"
SQS_IO_SH_summary$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_IO_combined = rbind(SQS_IO_summary, SQS_IO_NH_summary, SQS_IO_SH_summary)

### 12. Plot Ecogroup Diversity Indian Ocean Main Figure 4B and SM #### 

plot_SQS_IO_CSIB_main <- ggplot(SQS_IO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                linetype = Hemisphere)) +

  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Ecogroup Diversity", title = "SQS CSIB: Indian q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_IO_CSIB_main

plot_SQS_IO_CSIB_sm <- ggplot(SQS_IO_combined, aes(x = midpoints, y = Mean, color = Hemisphere,
                                                   linetype = Hemisphere)) +
  
  # Add confidence intervals as shaded ribbon
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Ecogroup Diversity", title = "SQS CSIB: Indian q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_IO_CSIB_sm

#### 13. Run SQS for ecogroup diversity the Pacific Ocean (Figure 4B and Supplementary Figures)  #####

# Perform subsampling and return distributions
SQS_PO = subsample(my_data_PO, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                   coll = "sampleID", output = "dist")
SQS_PO_NH = subsample(my_data_PO_NH, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")
SQS_PO_SH = subsample(my_data_PO_SH, iter = 1000, q = 0.8, tax = "ecogroup", bin = "bin", type = "sqs",
                      coll = "sampleID", output = "dist")

# Apply the function to the "divCSIB" variable for each dataset (SQS_AO, SQS_AO_NH, SQS_AO_SH)
SQS_PO_summary <- calculate_mean_and_CI(SQS_PO, "divCSIB")
SQS_PO_NH_summary <- calculate_mean_and_CI(SQS_PO_NH, "divCSIB")
SQS_PO_SH_summary <- calculate_mean_and_CI(SQS_PO_SH, "divCSIB")

SQS_PO_summary$tb = tb
SQS_PO_summary$midpoints = midpoints
SQS_PO_NH_summary$tb = tb
SQS_PO_NH_summary$midpoints = midpoints
SQS_PO_SH_summary$tb = tb
SQS_PO_SH_summary$midpoints = midpoints
                                                               
# Label the hemispheres
SQS_PO_summary$Hemisphere = "Combined"
SQS_PO_NH_summary$Hemisphere = "Northern"
SQS_PO_SH_summary$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_PO_combined = rbind(SQS_PO_summary, SQS_PO_NH_summary, SQS_PO_SH_summary)

#### 14. Plot Species Diversity Indian Ocean Main Figure 4A and SM #### 

plot_SQS_PO_CSIB_main <- ggplot(SQS_PO_combined, aes(x = midpoints, y = Mean, color = Hemisphere, 
                                                linetype = Hemisphere)) +
 
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Ecogroup Diversity", title = "SQS CSIB: Pacific q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_PO_CSIB_main

plot_SQS_PO_CSIB_sm <- ggplot(SQS_PO_combined, aes(x = midpoints, y = Mean, color = Hemisphere,
                                                   linetype = Hemisphere)) +
  
  # Add confidence intervals as shaded ribbon
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, fill = Hemisphere), alpha = 0.2, color = NA) +
  
  # Add the line and points for species diversity
  geom_line() +
  geom_point() +
  
  # Customize labels and theme
  labs(x = "Age (Ma)", y = "Ecogroup Diversity", title = "SQS CSIB: Pacific q=0.8") +
  
  # Customize color and fill scales
  scale_color_manual(values = c("Northern" = "#242f33", 
                                "Southern" = "#2b7dac", 
                                "Combined" = "grey70")) +
  
  scale_fill_manual(values = c("Northern" = "#242f33", 
                               "Southern" = "#2b7dac", 
                               "Combined" = "grey70")) +
  
  # Set linetypes: solid for Northern and Southern, dashed for Combined
  scale_linetype_manual(values = c("Northern" = 1, 
                                   "Southern" = 1, 
                                   "Combined" = 2)) +
  
  # Customize the x-axis
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.8, 3.8), expand = c(0.005, 0.005)) +
  
  # Customize the plot theme
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

# Display the plot
plot_SQS_PO_CSIB_sm

