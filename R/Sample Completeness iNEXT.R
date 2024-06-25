# Sample Completeness Evaluation using iNEXT package

# This script assesses of dataset completeness. Here, we examined 
# the sampling coverage and frequency of each unique ecogroup within a paleolatitudinal 
# band during each time interval, utilizing the iNEXT package.

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

library(vegan)
library(reshape2) 
library(dbplyr)
library(tidyverse)

#### 1. set paths for source files and configure the environment. ####

# set the working directory 
# setwd(" YOUR WOKRING DIRECTORY ")


# load the dataframes containing planktonic foraminiferal data from Triton database
# for the Atlantic, Pacific, and Indian Oceans
load("Triton_df_Oceans.RData")

# define a number of time bins
K = unique(Triton_Pacific$round.150K)

#### 2. run sample completeness test for the Atlantic Ocean ####

# create a list to store time bins
timebins_AO = list()

# populate the list with subsets of data from Triton_Atlantic 
for (i in 1:length(K)) {
  timebins_AO[[i]] = Triton_Atlantic[which(Triton_Atlantic$round.150K == K[i]),]
}

Eco_150k_iNext_AO = NULL

for (i in 1:length(K)){
  pt = as.data.frame(table(timebins_AO[[i]]$round.lat.5D, timebins_AO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  tblb = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  # preparing the table for the analysis in the iNext
  t_df<- as.data.frame(t(tblb))
  list_iNext = iNEXT(t_df, q=0, datatype="abundance")
  Eco_150k_iNext_AO[[i]] = list_iNext
}

# print the Data Info part of the iNEXT result for the current index to see sample completeness 
# results, marked as SC. The 'Assemblage' shows the paleolatitudinal band.
for (i in 1:length(K)){
  print(K[i]) # print time bin
  print(Eco_150k_iNext_AO[[i]]$DataInfo)
}

#### 3. run sample completeness test for the Pacific Ocean ####

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

#### 4. run sample completeness test for the Indian Ocean ####

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


# Example of debugging the apply function
test_output <- apply(Abun.Mat, 2, function(x) invChat.Ind(x, q, goalSC)$qD)
print(length(test_output))
print(dim(test_output))



