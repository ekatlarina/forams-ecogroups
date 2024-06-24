# SQS_analysis

# This script calculates the corrected sample-in-bin metric, utilizing the shareholder quorum 
# subsampling (SQS) method. This script also produces the raw plots for Figures 4 of the manuscript.
# The final figure was created in Illustrator.

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

####  1. Set paths for source files and configure the environment ####

# Set the working directory to where the data files are stored
setwd("Your_working_directory")

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

# Define unique time bins of 100,000 years from the Atlantic dataset
tb = unique(Triton_Atlantic$round.age_100_ka)

# Collect all dataframes into a list for further processing
data_frames = list(my_data_AO = my_data_AO, my_data_PO = my_data_PO, my_data_IO = my_data_IO,
                    my_data_AO_NH = my_data_AO_NH, my_data_AO_SH = my_data_AO_SH,
                    my_data_PO_NH = my_data_PO_NH, my_data_PO_SH = my_data_PO_SH,
                    my_data_IO_NH = my_data_IO_NH, my_data_IO_SH = my_data_IO_SH)

# Process each dataframe to assign 'bin' values based on time bins
for (i in 1:length(data_frames)) {
  data_frames[[i]]$bin = rep(NA, nrow(data_frames[[i]]))  # Initialize 'bin' column with NA
  
  for (j in 1:length(tb)) {
    store = which(data_frames[[i]]$round.age_100_ka == tb[j])
    data_frames[[i]]$bin[store] = j  # Assign bin number based on time bin
  }
}

# Export the updated data frames back to the global environment
list2env(data_frames, envir = .GlobalEnv)

#### 2. Create dataset for plotting with midpoints ####

# Initialize vector with starting numbers for time intervals
numbers = seq(from = 1.8, to = 3.9, by=0.1)

# Calculate midpoints for each interval to use in plots
midpoints = seq(from = 1.85, to= 3.85, by = 0.1)


#### 3. Run SQS for species diversity and plot the results for the Atlantic Ocean (Figure 4A) ####

# Subsample Atlantic Ocean data to calculate species diversity
SQS_AO = subsample(my_data_AO, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")
SQS_AO_NH = subsample(my_data_AO_NH, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")
SQS_AO_SH = subsample(my_data_AO_SH, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")

# Associate time bins and midpoints for plotting
SQS_AO$tb = tb
SQS_AO$midpoints = midpoints
SQS_AO_NH$tb = tb
SQS_AO_NH$midpoints = midpoints
SQS_AO_SH$tb = tb
SQS_AO_SH$midpoints = midpoints

# Label the hemispheres
SQS_AO$Hemisphere = "Combined"
SQS_AO_NH$Hemisphere = "Northern"
SQS_AO_SH$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_AO_combined = rbind(SQS_AO, SQS_AO_NH, SQS_AO_SH)

# Plot species diversity index by hemisphere over time for the Indian Ocean #
plot_SQS_AO_CSIB = ggplot(SQS_AO_combined, aes(x = midpoints, y = divCSIB)) +
  geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05) +
  geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5) +
  geom_line(aes(linetype = Hemisphere, color = Hemisphere)) +
  geom_point(aes(color = Hemisphere)) +
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Atlantic") +
  scale_x_continuous(breaks = numbers, labels = numbers, limits = c(1.75, 3.95), expand = c(0.005, 0.005)) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))
  
plot_SQS_AO_CSIB

#### 4. Run SQS for species diversity and plot the results for the Pacific Ocean (Figure 4A) ####

# Subsample Pacific Ocean data to calculate species diversity
SQS_PO = subsample(my_data_PO, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")
SQS_PO_NH = subsample(my_data_PO_NH, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")
SQS_PO_SH = subsample(my_data_PO_SH, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")

# Associate time bins and midpoints for plotting
SQS_PO$tb = tb
SQS_PO$midpoints = midpoints
SQS_PO_NH$tb = tb
SQS_PO_NH$midpoints = midpoints
SQS_PO_SH$tb = tb
SQS_PO_SH$midpoints = midpoints

# Label the hemispheres
SQS_PO$Hemisphere = "Combined"
SQS_PO_NH$Hemisphere = "Northern"
SQS_PO_SH$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_PO_combined = rbind(SQS_PO, SQS_PO_NH, SQS_PO_SH)
       
# Plot species diversity index by hemisphere over time for the Pacific Ocean                                         
plot_SQS_PO_CSIB = ggplot(SQS_PO_combined, aes(x= midpoints, y = divCSIB))+
  geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05) +
  geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5) +
  geom_line(aes(linetype = Hemisphere, color = Hemisphere)) +
  geom_point(aes(color = Hemisphere)) +
 labs(x="Age (Ma)", y= "Species Diversity",
      title = "SQS CSIB: Pacific") +
 scale_x_continuous(breaks = numbers, labels = numbers, limits = c(1.75, 3.95), 
                    expand = c(0.005, 0.005)) +
 theme(legend.position = "top",
       legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))
   
plot_SQS_PO_CSIB

#### 5. Run SQS for species diversity and plot the results for the Indian Ocean (Figure 4A) ####

# Subsample Indian Ocean data to calculate species diversity
SQS_IO = subsample(my_data_IO, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")
SQS_IO_NH = subsample(my_data_IO_NH, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")
SQS_IO_SH = subsample(my_data_IO_SH, iter = 1000, q = 0.7, tax = "species", bin = "bin", type = "sqs", coll = "sampleID")

# Associate time bins and midpoints for plotting
SQS_IO$tb = tb
SQS_IO$midpoints = midpoints
SQS_IO_NH$tb = tb
SQS_IO_NH$midpoints = midpoints
SQS_IO_SH$tb = tb
SQS_IO_SH$midpoints = midpoints

# Label the hemispheres
SQS_IO$Hemisphere = "Combined"
SQS_IO_NH$Hemisphere = "Northern"
SQS_IO_SH$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_IO_combined = rbind(SQS_IO, SQS_IO_NH, SQS_IO_SH)

# Plot species diversity index by hemisphere over time for the Indian Ocean #
plot_SQS_IO_CSIB = ggplot(SQS_IO_combined, aes(x = midpoints, y = divCSIB)) +
  geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05) +
  geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5) +
  geom_line(aes(linetype = Hemisphere, color = Hemisphere), na.rm = TRUE) +
  geom_point(aes(color = Hemisphere)) +
  labs(x = "Age (Ma)", y = "Species Diversity", title = "SQS CSIB: Indian") +
  scale_x_continuous(breaks = numbers, labels = numbers, limits = c(1.75, 3.95), expand = c(0.005, 0.005)) +
  theme(legend.position = "top", 
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"), axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))

plot_SQS_IO_CSIB

# Combine the plots using plot_grid

SQS_CSIB_all <- plot_grid(plot_SQS_AO_CSIB, plot_SQS_PO_CSIB, plot_SQS_IO_CSIB, ncol = 1, align = "v")

# Display the combined plot 
# The code creates a raw figure 4A of the manuscript

print(SQS_CSIB_all)


setwd("/Users/ekaterina.larina/Dropbox/Nature Geo 2024/figures/subsampling plots")

ggsave(plot = SQS_CSIB_all, filename = "SQS_CSIB_by_Oceans_SpDiv.png", width = 8, height = 10, dpi = 300)

#### 6. Run SQS for ecogroup diversity and plot the results for the Atlantic Ocean (Figure 4B) ####

# Subsample Atlantic Ocean data to calculate ecogroup diversity

 SQS_AO_eco = subsample(my_data_AO, iter = 1000, q = 0.7, tax="ecogroup", bin = "bin", 
                   type = "sqs", coll = "sampleID")

 SQS_AO_NH_eco = subsample(my_data_AO_NH, iter = 1000, q = 0.7, tax = "ecogroup", bin = "bin", 
                   type = "sqs", coll = "sampleID")
 
 SQS_AO_SH_eco = subsample(my_data_AO_SH, iter = 1000, q = 0.7, tax = "ecogroup", bin = "bin", 
                   type = "sqs", coll = "sampleID")

# Associate time bins and midpoints for plotting
 
SQS_AO_eco$tb = tb
SQS_AO_eco$midpoints = midpoints

SQS_AO_NH_eco$tb = tb
SQS_AO_NH_eco$midpoints = midpoints

SQS_AO_SH_eco$tb = tb
SQS_AO_SH_eco$midpoints = midpoints

# Label the hemispheres
SQS_AO_eco$Hemisphere = "Combined"
SQS_AO_NH_eco$Hemisphere = "Northern"
SQS_AO_SH_eco$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_AO_eco_combined = rbind(SQS_AO_eco, SQS_AO_NH_eco, SQS_AO_SH_eco)

# Plot ecogroup diversity index by hemisphere over time for the Atlantic Ocean #
plot_SQS_AO_CSIB = ggplot(SQS_AO_eco_combined, aes(x= midpoints, y = divCSIB))+
  geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05) +
  geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5) +
  geom_line(aes(linetype = Hemisphere, color = Hemisphere)) +
  geom_point(aes(color = Hemisphere)) +
 labs(x="Age (Ma)", y= "Ecogroup Diversity",
      title = "SQS CSIB: Atlantic") +
 scale_x_continuous(breaks = numbers, labels = numbers, limits = c(1.75, 3.95), 
                    expand = c(0.005, 0.005)) +
 theme(legend.position = "top",
        legend.key=element_rect(fill="transparent", colour=NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))
   
plot_SQS_AO_CSIB

#### 7. Run SQS for ecogroup diversity and plot the results for the Pacific Ocean (Figure 4B) ####

# Subsample Pacific Ocean data to calculate ecogroup diversity
SQS_PO_eco = subsample(my_data_PO, iter=1000, q=0.7, tax="ecogroup", bin="bin", 
                   type="sqs", coll="sampleID")
SQS_PO_NH_eco = subsample(my_data_PO_NH, iter=1000, q=0.7, tax="ecogroup", bin="bin", 
                   type="sqs", coll="sampleID")
SQS_PO_SH_eco = subsample(my_data_PO_SH, iter=1000, q=0.7, tax="ecogroup", bin="bin", 
                   type="sqs", coll="sampleID")

# Associate time bins and midpoints for plotting
SQS_PO_eco$tb = tb
SQS_PO_eco$midpoints = midpoints

SQS_PO_NH_eco$tb = tb
SQS_PO_NH_eco$midpoints = midpoints

SQS_PO_SH_eco$tb = tb
SQS_PO_SH_eco$midpoints = midpoints

# Label the hemispheres
SQS_PO_eco$Hemisphere = "Combined"
SQS_PO_NH_eco$Hemisphere = "Northern"
SQS_PO_SH_eco$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_PO_eco_combined = rbind(SQS_PO_eco, SQS_PO_NH_eco, SQS_PO_SH_eco)

# Plot ecogroup diversity index by hemisphere over time for the Pacific Ocean #
plot_SQS_PO_CSIB = ggplot(SQS_PO_eco_combined, aes(x= midpoints, y = divCSIB))+
  geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05) +
  geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5) +
  geom_line(aes(linetype = Hemisphere, color = Hemisphere)) +
  geom_point(aes(color = Hemisphere)) +
 labs(x="Age (Ma)", y= "Ecogroup Diversity",
      title = "SQS CSIB: Pacific") +
 scale_x_continuous(breaks = numbers, labels = numbers, limits = c(1.75, 3.95), 
                    expand = c(0.005, 0.005)) +
 theme(legend.position = "top",
        legend.key=element_rect(fill="transparent", colour=NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12))
   
plot_SQS_PO_CSIB

#### 8. Run SQS for ecogroup diversity and plot the results for the Indian Ocean (Figure 4B) ####

# Subsample Indian Ocean data to calculate ecogroup diversity
SQS_IO_eco = subsample(my_data_IO, iter=1000, q=0.7, tax="ecogroup", bin="bin", 
                   type="sqs", coll="sampleID")
SQS_IO_NH_eco = subsample(my_data_IO_NH, iter=1000, q=0.7, tax="ecogroup", bin="bin", 
                   type="sqs", coll="sampleID")
SQS_IO_SH_eco = subsample(my_data_IO_SH, iter=1000, q=0.7, tax="ecogroup", bin="bin", 
                   type="sqs", coll="sampleID")

# Associate time bins and midpoints for plotting
SQS_IO_eco$tb = tb
SQS_IO_eco$midpoints = midpoints

SQS_IO_NH_eco$tb = tb
SQS_IO_NH_eco$midpoints = midpoints

SQS_IO_SH_eco$tb = tb
SQS_IO_SH_eco$midpoints = midpoints

# Label the hemispheres
SQS_IO_eco$Hemisphere = "Combined"
SQS_IO_NH_eco$Hemisphere = "Northern"
SQS_IO_SH_eco$Hemisphere = "Southern"

# Combine the subsampled datasets for plotting
SQS_IO_eco_combined = rbind(SQS_IO_eco, SQS_IO_NH_eco, SQS_IO_SH_eco)

# Plot ecogroup diversity index by hemisphere over time for the Indian Ocean
plot_SQS_IO_CSIB = ggplot(SQS_IO_eco_combined, aes(x=midpoints, y=divCSIB)) +
  geom_rect(xmin=3, xmax=3.3, ymin=-Inf, ymax=Inf, fill="mistyrose1", alpha=0.05) +
  geom_rect(xmin=2.5, xmax=2.7, ymin=-Inf, ymax=Inf, fill="lightcyan", alpha=0.05) +
  geom_rect(xmin=3.6, xmax=3.9, ymin=-Inf, ymax=Inf, fill="aliceblue", alpha=0.5) +
  geom_line(aes(linetype=Hemisphere, color=Hemisphere), na.rm=TRUE) +
  geom_point(aes(color=Hemisphere)) +
  labs(x="Age (Ma)", y="Ecogroup Diversity", title="SQS CSIB: Indian") +
  scale_x_continuous(breaks=numbers, labels=numbers, limits=c(1.75, 3.95), expand=c(0.005, 0.005)) +
  theme(legend.position="top",
        legend.key=element_rect(fill="transparent", colour=NA),
        panel.background=element_rect(fill="transparent"),
        panel.border=element_rect(fill=NA, color="black"),
        axis.text=element_text(colour="black", size=12),
        axis.title=element_text(colour="black", size=12))

print(plot_SQS_IO_CSIB)


# Combine the plots using plot_grid

SQS_CSIB_eco <- plot_grid(plot_SQS_AO_CSIB, plot_SQS_PO_CSIB, plot_SQS_IO_CSIB, ncol = 1, align = "v")

# Display the combined plot

print(SQS_CSIB_eco)

#setwd("/Users/ekaterina.larina/Dropbox/Nature Geo 2024/figures/subsampling plots")

#ggsave(plot = SQS_CSIB_eco, filename = "SQS_CSIB_by_Oceans_EcoDiv.png", width = 8, height = 10, dpi = 300)






