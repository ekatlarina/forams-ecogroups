# Sample Completeness Evaluation using iNEXT package

# This script assesses our dataset completeness. Here, we examined 
# the sampling coverage and frequency of each unique ecogroup within 
# a paleolatitudinal band during each time interval, utilizing the iNEXT package.

# install relevant packages for assessing sample completeness
## install iNEXT package from CRAN
install.packages("iNEXT")
install.packages('devtools')
library(devtools)
install_github('AnneChao/iNEXT')
library(iNEXT)

install.packages("vegan")
install.packages("reshape2")
install.packages("dbplyr")
install.packages("tidyverse")
install.packages("gplots")

library(vegan)
library(reshape2) 
library(dbplyr)
library(tidyverse)
library(gplots)

#### 1. Set the paths for source files and configure the environment. ####

# set the working directory 
# setwd(" YOUR WOKRING DIRECTORY ")


# load the data frames containing planktic foraminiferal data from Triton database
# for the Atlantic, Pacific, and Indian Oceans
load("Triton_df_Oceans.RData")

# define a number of time bins
K = unique(Triton_Pacific$round.150K)

#### 2. Run a sample completeness test for the Atlantic Ocean ####

# create a list to store time bins
timebins_AO = list()

# populate the list with subsets of data from the Triton_Atlantic 
for (i in 1:length(K)) {
  timebins_AO[[i]] = Triton_Atlantic[which(Triton_Atlantic$round.150K == K[i]),]
}

Eco_150k_iNext_AO = NULL

for (i in 1:length(K)){
  pt = as.data.frame(table(timebins_AO[[i]]$round.lat.5D, timebins_AO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  tblb = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  t_df<- as.data.frame(t(tblb))
  list_iNext = iNEXT(t_df, q=0, datatype="abundance")
  Eco_150k_iNext_AO[[i]] = list_iNext
}

# print the Data Info part of the iNEXT result for the current index to see 
# sample completeness   results, marked as SC. The 'Assemblage' shows 
# the paleolatitudinal band.

for (i in 1:length(K)){
  print(K[i]) # print time bin
  print(Eco_150k_iNext_AO[[i]]$DataInfo)
}

#### Creating the table for the supplementary material: Atlantic Ocean ####

# Define the ordered latitudinal bins and time bins
lat_bins <- seq(-40, 60, by = 5)  # defining latitudinal range for the Atlantic Ocean
time_bins <- K

# Initialize an empty table with predefined row and column names
data_wide_ordered_AO <- matrix(NA, nrow = length(lat_bins), ncol = length(time_bins),
                            dimnames = list(lat_bins, time_bins))
data_wide_ordered_AO <- as.data.frame(data_wide_ordered_AO)

# Fill in the table with SC values
for (i in 1:length(K)) {
  time_bin <- K[i]
  data_info <- Eco_150k_iNext_AO[[i]]$DataInfo
  
  # Match rows by latitude and fill in the corresponding time bin column
  for (j in 1:nrow(data_info)) {
    lat <- data_info$Assemblage[j]
    if (lat %in% lat_bins) {
      data_wide_ordered_AO[as.character(lat), as.character(time_bin)] <- data_info$SC[j]
    }
  }
}

# View the final table
print(data_wide_ordered_AO)

min(data_wide_ordered_AO, na.rm = TRUE)
max(data_wide_ordered_AO, na.rm = TRUE)

#### 3. Run a sample completeness test for the Pacific Ocean ####

timebins_PO = list()

for (i in 1:length(K)) {
  timebins_PO[[i]]=Triton_Pacific[which(Triton_Pacific$round.150K == K[i]),]
}

Eco_150k_iNext_PO = NULL

for (i in 1:length(K)){
  pt = as.data.frame(table(timebins_PO[[i]]$round.lat.5D, timebins_PO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  tblb = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  t_df<- as.data.frame(t(tblb))
  list_iNext = iNEXT(t_df, q = 0, datatype = "abundance")
  Eco_150k_iNext_PO[[i]] = list_iNext
}

for (i in 1:length(K)){
  print(K[i])
  print(Eco_150k_iNext_PO[[i]]$DataInfo)
}

### Creating the table for the supplementary material: Pacific Ocean. ####

# Define the ordered latitudinal bins and time bins
lat_bins <- seq(-45, 40, by = 5)  # defining latitudinal range for the Atlantic Ocean

# Initialize an empty table with predefined row and column names
data_wide_ordered_PO <- matrix(NA, nrow = length(lat_bins), ncol = length(time_bins),
                            dimnames = list(lat_bins, time_bins))
data_wide_ordered_PO <- as.data.frame(data_wide_ordered_PO)

# Fill in the table with SC values
for (i in 1:length(K)) {
  time_bin <- K[i]
  data_info <- Eco_150k_iNext_PO[[i]]$DataInfo
  
  # Match rows by latitude and fill in the corresponding time bin column
  for (j in 1:nrow(data_info)) {
    lat <- data_info$Assemblage[j]
    if (lat %in% lat_bins) {
      data_wide_ordered_PO[as.character(lat), as.character(time_bin)] <- data_info$SC[j]
    }
  }
}

# View the final table
print(data_wide_ordered_PO)
min(data_wide_ordered_PO, na.rm = TRUE)
max(data_wide_ordered_PO, na.rm = TRUE)

#### 4. Run a sample completeness test for the Indian Ocean ####

timebins_IO = list()

for (i in 1:length(K)) {
  timebins_IO[[i]] = Triton_Indian[which(Triton_Indian$round.150K == K[i]),]
}

Eco_150k_iNext_IO = NULL

for (i in 1:length(K)){
  pt = as.data.frame(table(timebins_IO[[i]]$round.lat.5D, timebins_IO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  tblb = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  t_df = as.data.frame(t(tblb))
  list_iNext = iNEXT(t_df, q = 0, datatype = "abundance")
  Eco_150k_iNext_IO[[i]] = list_iNext
}

for (i in 1:length(K)){
  print(K[i])
  print(Eco_150k_iNext_IO[[i]]$DataInfo)
}

### Creating the table for the supplementary material: Indian Ocean.####

# Define the ordered latitudinal bins and time bins
lat_bins <- seq(-45, 15, by = 5)  # defining latitudinal range for the Atlantic Ocean


# Initialize an empty table with predefined row and column names
data_wide_ordered_IO <- matrix(NA, nrow = length(lat_bins), ncol = length(time_bins),
                            dimnames = list(lat_bins, time_bins))
data_wide_ordered_IO <- as.data.frame(data_wide_ordered_IO)

# Fill in the table with SC values
for (i in 1:length(K)) {
  time_bin <- K[i]
  data_info <- Eco_150k_iNext_IO[[i]]$DataInfo
  
  # Match rows by latitude and fill in the corresponding time bin column
  for (j in 1:nrow(data_info)) {
    lat <- data_info$Assemblage[j]
    if (lat %in% lat_bins) {
      data_wide_ordered_IO[as.character(lat), as.character(time_bin)] <- data_info$SC[j]
    }
  }
}

# View the final table
print(data_wide_ordered_IO)
min(data_wide_ordered_IO, na.rm = TRUE)
max(data_wide_ordered_IO, na.rm = TRUE)

##### 5. Plotting heatmaps for the basins. ####

data_wide_ordered_AO = as.matrix(data_wide_ordered_AO)
data_wide_ordered_PO = as.matrix(data_wide_ordered_PO)
data_wide_ordered_IO = as.matrix(data_wide_ordered_IO)


# Define color gradient function for heatmap
colfunc = colorRampPalette(c("#440154FF", "#1F968BFF", "#FDE725FF"))


# Generate the heatmap for the Atlantic Ocean
heatmap.2(data_wide_ordered_AO, col = colfunc(80), trace="none", key.xlab="SC", 
          key=TRUE, dendrogram="none", Rowv=NULL, Colv=NULL, 
          density.info="none", revC=TRUE, main="Sample Coverage: Atlantic")


# Generate the heatmap for the Pacific Ocean
heatmap.2(data_wide_ordered_PO, col = colfunc(80), trace="none", key.xlab="SC", 
          key=TRUE, dendrogram="none", Rowv=NULL, Colv=NULL, 
          density.info="none", revC=TRUE, main="Sample Coverage: Pacific")



# Generate the heatmap for the Indian Ocean
heatmap.2(data_wide_ordered_IO, col = colfunc(80), trace="none", key.xlab="SC", 
          key=TRUE, dendrogram="none", Rowv=NULL, Colv=NULL, 
          density.info="none", revC=TRUE, main="Sample Coverage: Indian")



