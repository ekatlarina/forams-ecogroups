# Ecogroup Distributions from Raw and Subsampled Data Across Hemispheres

# This script performs resampling of ecogroup distribution data without 
# replacement and produces the raw plots for Figure 9 of the supplementary 
# material

# install relevant packages for calculating ecogroup distributions
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")  

library(ggplot2) # used for plotting data
library(dplyr) # used for data manipulation and transformation
library(gridExtra) # used to arrange plots on one page

#### 1. set paths for source files and configure the environment. ####

# set the working directory 
# setwd(" YOUR WOKRING DIRECTORY ")
setwd("/Users/ekaterina.larina/Dropbox/Nature Geo 2024/RData files")

# load the dataframes containing planktonic foraminiferal data from Triton database
# for the Atlantic, Pacific, and Indian Oceans

load("Triton_df_Oceans.RData")

# filter the dataframes by hemisphere for each ocean
my_data_AO_NH = Triton_Atlantic %>% filter(Hemisphere == "Northern")
my_data_AO_SH = Triton_Atlantic %>% filter(Hemisphere == "Southern")
my_data_PO_NH = Triton_Pacific %>% filter(Hemisphere == "Northern")
my_data_PO_SH = Triton_Pacific %>% filter(Hemisphere == "Southern")
my_data_IO_NH = Triton_Indian %>% filter(Hemisphere == "Northern")
my_data_IO_SH = Triton_Indian %>% filter(Hemisphere == "Southern")

# define a sequence of time bins
roundage150K = seq(from = 1.8, to = 3.75, by = 0.15)
K = length(roundage150K) # number of time bins
 
# define a number of iterations
 it = 1000
 
 # define the titles for each plot
title_AO_NH = "Atlantic Northern Hemisphere: Ecogroups"
title_AO_SH = "Atlantic Southern Hemisphere: Ecogroups"

title_PO_NH = "Pacific Northern Hemisphere: Ecogroups"
title_PO_SH = "Pacific Southern Hemisphere: Ecogroups"

title_IO_NH = "Indian Northern Hemisphere: Ecogroups"
title_IO_SH = "Indian Southern Hemisphere: Ecogroups"
 
#### 2. execute the functions defined in the script #### 

# the 'calculate_proportions' function performs resampling of ecogroup 
# distribution data without replacement for each 150-kyr time bin, matching 
# the smallest occurrence count of planktic foraminifera observed in any 150-kyr bin

 #calculate_proportions <- function(data_list, K, it, nval, tb) {
 
# initialize an empty dataframe to store the mean results for all time bins
#  for_eco_ResMean <- NULL
  
# loop over each time bin
#  for (i in 1:K) {
# initialize an empty dataframe for accumulating resampled results
#    for_eco_re0 <- NULL
# perform resampling of 1000 iterations for the current time bin
#    for (j in 1:it) {
# resample the data without replacement
#      rowselect <- data_list[[i]] %>% 
#        sample_n(size = nval, replace = FALSE)
# calculate the proportion of occurrences for each ecogroup at each time point
#      proportion_data <- rowselect %>%
#        group_by(ecogroup) %>%
#        summarise(count = n(), .groups = 'drop') %>%
#        mutate(proportion = count / sum(count))
# accumulate all the results in one dataframe
#      for_eco_re0 <- rbind(for_eco_re0, proportion_data)
#    }
# recording the mean of resampled results
#    netrand_eco <- for_eco_re0 %>%
#      group_by(ecogroup) %>%
#      summarise(across(where(is.numeric), mean, na.rm = TRUE))
# adding time bin to the dataframes
#    netrand_eco$timebin <- tb[i]
# accumulate the mean results for all time bins
#    for_eco_ResMean <- rbind(for_eco_ResMean, netrand_eco)
#    print(paste("Finished Timebin", i))    
#  }
#  return(for_eco_ResMean)
#}

# the 'calculate_proportions_by_core' function performs resampling of ecogroup 
# distribution data without replacement for each 150-kyr time bin, matching 
# the smallest number of cores in any 150-kyr bin

calculate_proportions_by_core <- function(data_list, K, it, n_cores_min, tb) {
  for_eco_ResMean <- NULL
  
  for (i in 1:K) {
    time_bin_data <- data_list[[i]]
    unique_cores <- unique(time_bin_data$holeID)
    
   if (length(unique_cores) < n_cores_min) next  # skip if not enough cores

    for_eco_re0 <- NULL
    
    for (j in 1:it) {
      sampled_cores <- sample(unique_cores, size = n_cores_min, replace = FALSE)
      subset_data <- time_bin_data %>% filter(holeID %in% sampled_cores)

      proportion_data <- subset_data %>%
        group_by(ecogroup) %>%
        summarise(count = n(), .groups = 'drop') %>%
        mutate(proportion = count / sum(count))

      for_eco_re0 <- rbind(for_eco_re0, proportion_data)
    }
    
    netrand_eco <- for_eco_re0 %>%
      group_by(ecogroup) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE))
    
    netrand_eco$timebin <- tb[i]
    for_eco_ResMean <- rbind(for_eco_ResMean, netrand_eco)
    
    print(paste("Finished Timebin", i))
  }
  
  return(for_eco_ResMean)
}

 
# the 'eco_proportion' function calculates the proportion of occurrences for 
# each ecogroup at each time bin
eco_proportion <- function(df){
 return(df = df %>%
  group_by(round.150K, ecogroup) %>%
  summarise(count = n()) %>%
  group_by(round.150K) %>%
  mutate(proportion = count / sum(count)))
}

# the 'combine_data' function combines two data frames

combined_data <- function(df1, df2){
  df1 %>%
  mutate(source = "Raw") %>%
  bind_rows(df2 %>% mutate(source = "Subsampled"))
}

# the 'violin' function generates violin plots
violin <- function(data, title){ 
  ggplot(data, aes(x = source, y = proportion)) + 
  geom_violin(aes(fill = source), color = "darkgray") +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +  
  scale_fill_manual(values = c("Raw" = "#E6A67A", "Subsampled" = "#6E5C42"))+
  labs(title = title, y = "Proportion") 
}

#### 3. Atlantic: Northern Hemisphere ####

# create a list to store time bins
timebinsPleiPli_AO_NH <- list()

for (i in 1:length(roundage150K)) {
  timebinsPleiPli_AO_NH[[i]] <- 
    my_data_AO_NH[which(my_data_AO_NH$round.150K == roundage150K[i]),]
}
# define smallest number of total occurrences across all 14 bins
#nval <- min(sapply(timebinsPleiPli_AO_NH,nrow)) 

# define the smallest number of cores across all 14 bins

n_cores_min <- min(sapply(timebinsPleiPli_AO_NH, function(df) 
  length(unique(df$holeID))))


# calculate resampled count and proportions for each ecogroup within each time bin
for_eco_ResMean_AO_NH <- calculate_proportions_by_core(data_list = timebinsPleiPli_AO_NH, 
                                                       K = K, it = it, 
                                                       n_cores_min = n_cores_min, 
                                                       tb = roundage150K)
# calculate raw count and proportions for each ecogroup within each time bin
proportion_data_eco_raw_AO_NH <- eco_proportion(my_data_AO_NH) 
                          
#calculate the difference between the raw and resampled data
df_diff_eco <- proportion_data_eco_raw_AO_NH$proportion - for_eco_ResMean_AO_NH$proportion

# combine two dataframes
combined_AO_NH <- combined_data(proportion_data_eco_raw_AO_NH, for_eco_ResMean_AO_NH)

# create a boxplot for the Northern Hemisphere in the Atlantic Ocean 
violin_Eco_AO_NH <- violin(combined_AO_NH, title_AO_NH)

print(violin_Eco_AO_NH)

#### 4. Atlantic: Southern Hemisphere ####

# create a list 
timebinsPleiPli_AO_SH = list()

for (i in 1:length(roundage150K)) {
  timebinsPleiPli_AO_SH[[i]]= my_data_AO_SH[which(my_data_AO_SH$round.150K == roundage150K[i]),]
}

# nval = min(sapply(timebinsPleiPli_AO_SH,nrow)) 
#smallest number of cores across 14 bins
n_cores_min <- min(sapply(timebinsPleiPli_AO_SH, function(df) 
  length(unique(df$holeID))))


# calculate resampled count and proportions for each ecogroup within each time bin
for_eco_ResMean_AO_SH <- calculate_proportions_by_core(data_list = timebinsPleiPli_AO_SH, 
                                                       K = K, it = it, 
                                                       n_cores_min = n_cores_min, 
                                                       tb = roundage150K)

proportion_data_eco_raw_AO_SH = eco_proportion(my_data_AO_SH) 

df_diff_eco = proportion_data_eco_raw_AO_SH$proportion - for_eco_ResMean_AO_SH$proportion
combined_AO_SH = combined_data(proportion_data_eco_raw_AO_SH, for_eco_ResMean_AO_SH)

# create a violin graph for the Southern Hemisphere of the Atlantic Ocean
violin_Eco_AO_SH = violin(combined_AO_SH, title_AO_SH)
print(violin_Eco_AO_SH)

#### 5. Pacific: Northern Hemisphere ####

# create a list
timebinsPleiPli_PO_NH = list()

for (i in 1:length(roundage150K)) {
  timebinsPleiPli_PO_NH[[i]]= my_data_PO_NH[which(my_data_PO_NH$round.150K == roundage150K[i]),]
}

# nval = min(sapply(timebinsPleiPli_PO_NH,nrow)) #smallest number of total occurrences across all 14 bins

n_cores_min <- min(sapply(timebinsPleiPli_PO_NH, function(df) 
  length(unique(df$holeID))))

for_eco_ResMean_PO_NH = calculate_proportions_by_core(data_list = timebinsPleiPli_PO_NH, K = K, it = it, 
                                              n_cores_min = n_cores_min, tb = roundage150K)
# calculate raw count and proportions for each ecogroup within each time bin
proportion_data_eco_raw_PO_NH = eco_proportion(my_data_PO_NH) 

#calculate the difference between the raw and subsampled data 
df_diff_eco = proportion_data_eco_raw_PO_NH$proportion - for_eco_ResMean_PO_NH$proportion

combined_PO_NH = combined_data(proportion_data_eco_raw_PO_NH, for_eco_ResMean_PO_NH)

# create a boxplot for the Northern Hemisphere of the Pacific Ocean
violin_Eco_PO_NH = violin(combined_PO_NH, title_PO_NH)

print(violin_Eco_PO_NH)

#### 6. Pacific: Southern Hemisphere ####

# create a list
timebinsPleiPli_PO_SH = list()

for (i in 1:length(roundage150K)) {
  timebinsPleiPli_PO_SH[[i]]= my_data_PO_SH[which(my_data_PO_SH$round.150K == roundage150K[i]),]
}

# nval = min(sapply(timebinsPleiPli_PO_SH,nrow)) 

n_cores_min <- min(sapply(timebinsPleiPli_PO_SH, function(df) 
  length(unique(df$holeID))))

for_eco_ResMean_PO_SH = calculate_proportions_by_core(data_list = timebinsPleiPli_PO_SH, K = K, it = it, 
                                              n_cores_min = n_cores_min, tb = roundage150K)

proportion_data_eco_raw_PO_SH = eco_proportion(my_data_PO_SH) 

df_diff_eco = proportion_data_eco_raw_PO_SH$proportion - for_eco_ResMean_PO_SH$proportion

combined_PO_SH = combined_data(proportion_data_eco_raw_PO_SH, for_eco_ResMean_PO_SH)

violin_Eco_PO_SH = violin(combined_PO_SH, title_PO_SH)
print(violin_Eco_PO_SH)

#### 7. Indian: Northern Hemisphere ####

# create a list
timebinsPleiPli_IO_NH = list()

for (i in 1:length(roundage150K)) {
  timebinsPleiPli_IO_NH[[i]]= my_data_IO_NH[which(my_data_IO_NH$round.150K == roundage150K[i]),]
}

#nval = min(sapply(timebinsPleiPli_IO_NH,nrow)) 
n_cores_min <- min(sapply(timebinsPleiPli_IO_NH, function(df) 
  length(unique(df$holeID))))

for_eco_ResMean_IO_NH = calculate_proportions_by_core(data_list = timebinsPleiPli_IO_NH, K = K, it = it, 
                                           n_cores_min = n_cores_min, tb = roundage150K)

proportion_data_eco_raw_IO_NH = eco_proportion(my_data_IO_NH) 
df_diff_eco = proportion_data_eco_raw_IO_NH$proportion - for_eco_ResMean_IO_NH$proportion

combined_IO_NH = combined_data(proportion_data_eco_raw_IO_NH, for_eco_ResMean_IO_NH)
violin_Eco_IO_NH = violin(combined_IO_NH, title_IO_NH)
print(violin_Eco_IO_NH)

#### 8. Indian: Southern Hemisphere ####

timebinsPleiPli_IO_SH = list()

for (i in 1:length(roundage150K)) {
  timebinsPleiPli_IO_SH[[i]]= my_data_IO_SH[which(my_data_IO_SH$round.150K == roundage150K[i]),]
}

#nval = min(sapply(timebinsPleiPli_IO_SH,nrow)) 
n_cores_min <- min(sapply(timebinsPleiPli_IO_SH, function(df) 
  length(unique(df$holeID))))

for_eco_ResMean_IO_SH = calculate_proportions_by_core(data_list = timebinsPleiPli_IO_SH, K = K, it = it, 
                                              n_cores_min = n_cores_min, tb = roundage150K)
proportion_data_eco_raw_IO_SH = eco_proportion(my_data_IO_SH) 

df_diff_eco = proportion_data_eco_raw_IO_SH$proportion - for_eco_ResMean_IO_SH$proportion
combined_IO_SH = combined_data(proportion_data_eco_raw_IO_SH, for_eco_ResMean_IO_SH)

violin_Eco_IO_SH = violin(combined_IO_SH, title_IO_SH)
print(violin_Eco_IO_SH)

#### 9. display all graphs on one page ####

combined_plot_Eco =  grid.arrange(
violin_Eco_AO_NH, violin_Eco_AO_SH,violin_Eco_PO_NH, violin_Eco_PO_SH, violin_Eco_IO_NH,
violin_Eco_IO_SH, ncol = 2, nrow = 3)



