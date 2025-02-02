### Null Models: Ecogroup Distribution ###


# This script uses 'econullnetr' package to run null models. 
# This script also produces the raw plots for Figures 10, 11 and 12 of the 
# supplementary material. Final figures were created in Illustrator.

# install relevant packages to run null models
install.packages("econullnetr")
library(econullnetr)

install.packages("gplots")
library(gplots)

#### 1. set paths for source files and configure the environment. ####

# set the working directory 
# setwd(" YOUR WORKING DIRECTORY ")


# load the dataframes containing planktonic foraminiferal data from the Triton database
# for the Atlantic, Pacific, and Indian Oceans
load("Triton_df_Oceans.RData")

# define a number of time bins
K = unique(Triton_Pacific$round.150K)

# define color palette for the plots
colfunc = colorRampPalette(c("#440154FF", "#1F968BFF", "#FDE725FF"))

# define breaks for the legend
legend_breaks <- seq(0, 1, length.out = 81)

#### 2. run null models for the Atlantic Ocean ####

# create a list to store time bins
timebins_AO = list()

# populate the list with subsets of data from the Triton_Atlantic dataframe
for (i in 1:length(K)) {
  timebins_AO[[i]] = Triton_Atlantic[which(Triton_Atlantic$round.150K == K[i]),]
}

# define the latitudinal range for the Atlantic Ocean
lat.range_AO = seq(-40,60,5)
all_bins_5D_AO = data.frame(lat = lat.range_AO)

# create lists to store results for null models with stronger, weaker, and no
# preference
Stronger_AO = NULL
NS_AO = NULL
Weaker_AO = NULL

for (i in 1:length(K)){
  
  pt = as.data.frame(table(timebins_AO[[i]]$round.lat.5D, timebins_AO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  null.1 = generate_null_net(cbind((rownames(pt2)),pt2), as.data.frame(t(colSums(pt2))),
                             sims = 500, data.type="counts")
  Sig = as.data.frame(test_interactions(null.1, 0.95))
  tbla = as.data.frame(table(Sig$Consumer,Sig$Test))
  tbla$Freq = as.numeric(tbla$Freq)
  tblb = as.data.frame(acast(tbla, Var1~Var2, value.var="Freq"))
  tblb2 = cbind(rownames(tblb), tblb)
  colnames(tblb2)[1] = "lat"
  tblb2$lat = as.numeric(tblb2$lat)
  tblb3 = full_join(all_bins_5D_AO,tblb2, by="lat")
  
  if(length(tblb3$Stronger)!=0){
    Stronger_AO = rbind(Stronger_AO, (tblb3$Stronger/dim(pt2)[2]))}
  else{
    placehd = tblb3$ns
    placehd[!is.na(placehd)] = 0
    Weaker_AO = rbind(Weaker_AO, (placehd/dim(pt2)[2]))}
  if(length(tblb3$Weaker)!=0){
    Weaker_AO = rbind(Weaker_AO, (tblb3$Weaker/dim(pt2)[2]))}
  else{
    placehd = tblb3$ns
    placehd[!is.na(placehd)] = 0
    Weaker_AO = rbind(Weaker_AO, (placehd/dim(pt2)[2]))}
  NS_AO = rbind(NS_AO, ((tblb3$ns)/dim(pt2)[2]))
  print(paste("Finished Timebin ", i))
}

colnames(Stronger_AO)= as.character(lat.range_AO)
colnames(Weaker_AO)= as.character(lat.range_AO)
colnames(NS_AO)= as.character(lat.range_AO)

rownames(Stronger_AO)= as.character(K)
rownames(Weaker_AO)= as.character(K)
rownames(NS_AO)= as.character(K)

#### 3. plotting heatmaps with null models for the Atlantic Ocean (Supplementary Material Figure 10) ####

# transpose dataframes
t_Stronger_AO = t(Stronger_AO)
t_Weaker_AO = t(Weaker_AO)
t_NS_AO = t(NS_AO)

heatmap.2(t_Stronger_AO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ stronger pref in AO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "Atlantic Ecogroup - Stronger Preference")

heatmap.2(t_Weaker_AO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ weaker pref",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "Atlantic Eco - Weaker Preference")

heatmap.2(t_NS_AO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ no pref",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "Atlantic Eco- No Preference")

#### 4. run null models for the Atlantic Ocean ####

timebins_PO = list()

for (i in 1:length(K)){
  timebins_PO[[i]] = Triton_Pacific[which(Triton_Pacific$round.150K == K[i]),]
}

lat.range_PO = seq(-45,50,5)
all_bins_5D_PO = data.frame(lat = lat.range_PO)

Stronger_PO = NULL
NS_PO = NULL
Weaker_PO = NULL

for (i in 1:length(K)){
  
  pt = as.data.frame(table(timebins_PO[[i]]$round.lat.5D, timebins_PO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  null.1 = generate_null_net(cbind((rownames(pt2)),pt2), as.data.frame(t(colSums(pt2))),
                             sims = 500, data.type="counts")
  Sig = as.data.frame(test_interactions(null.1, 0.95))
  tbla = as.data.frame(table(Sig$Consumer,Sig$Test))
  tbla$Freq = as.numeric(tbla$Freq)
  tblb = as.data.frame(acast(tbla, Var1~Var2, value.var="Freq"))
  tblb2 = cbind(rownames(tblb), tblb)
  colnames(tblb2)[1] = "lat"
  tblb2$lat = as.numeric(tblb2$lat)
  tblb3 = full_join(all_bins_5D_PO,tblb2, by="lat")
  
  if(length(tblb3$Stronger)!=0){
    Stronger_PO = rbind(Stronger_PO, (tblb3$Stronger/dim(pt2)[2]))}
  else{
    placehd = tblb3$ns
    placehd[!is.na(placehd)] = 0
    Weaker_PO = rbind(Weaker_PO, (placehd/dim(pt2)[2]))}
  if(length(tblb3$Weaker)!=0){
    Weaker_PO = rbind(Weaker_PO, (tblb3$Weaker/dim(pt2)[2]))}
  else{
    placehd = tblb3$ns
    placehd[!is.na(placehd)] = 0
    Weaker_PO = rbind(Weaker_PO, (placehd/dim(pt2)[2]))}
  NS_PO = rbind(NS_PO, ((tblb3$ns)/dim(pt2)[2]))
  print(paste("Finished Timebin ", i))
}

colnames(Stronger_PO)= as.character(lat.range_PO)
colnames(Weaker_PO)= as.character(lat.range_PO)
colnames(NS_PO)= as.character(lat.range_PO)

rownames(Stronger_PO)= as.character(K)
rownames(Weaker_PO)= as.character(K)
rownames(NS_PO)= as.character(K)

#### 5. plotting heatmaps with null models for the Pacific Ocean (Supplementary Material Figure 11)####

t_Stronger_PO = t(Stronger_PO)
t_Weaker_PO = t(Weaker_PO)
t_NS_PO = t(NS_PO)

# plotting species diversity
heatmap.2(t_Stronger_PO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ stronger pref in PO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "PO Ecogroup - Stronger Preference")

heatmap.2(t_Weaker_PO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ weaker pref in PO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "PO Eco - Weaker Preference")


heatmap.2(t_NS_PO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ no pref in PO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "PO Eco- No Preference")


#### 6. run null models for the Indian Ocean ####

timebins_IO = list()

for (i in 1:length(K)) {
  timebins_IO[[i]] = Triton_Indian[which(Triton_Indian$round.150K == K[i]),]
}

lat.range_IO = seq(-45,15,5)
all_bins_5D_IO = data.frame(lat = lat.range_IO)

Stronger_IO = NULL
NS_IO = NULL
Weaker_IO = NULL

for (i in 1:length(K)){
  pt = as.data.frame(table(timebins_IO[[i]]$round.lat.5D, timebins_IO[[i]]$ecogroup))
  pt$Freq = as.numeric(pt$Freq) 
  pt2 = as.data.frame(acast(pt, Var1~Var2, value.var="Freq"))
  null.1 = generate_null_net(cbind((rownames(pt2)),pt2), as.data.frame(t(colSums(pt2))),
                             sims = 500, data.type="counts")
  Sig = as.data.frame(test_interactions(null.1, 0.95))
  tbla = as.data.frame(table(Sig$Consumer,Sig$Test))
  tbla$Freq = as.numeric(tbla$Freq)
  tblb = as.data.frame(acast(tbla, Var1~Var2, value.var="Freq"))
  tblb2 = cbind(rownames(tblb), tblb)
  colnames(tblb2)[1] = "lat"
  tblb2$lat = as.numeric(tblb2$lat)
  tblb3 = full_join(all_bins_5D_IO,tblb2, by="lat")
  
  if(length(tblb3$Stronger)!=0){
    Stronger_IO = rbind(Stronger_IO, (tblb3$Stronger/dim(pt2)[2]))}
  else{
    placehd = tblb3$ns
    placehd[!is.na(placehd)] = 0
    Weaker_IO = rbind(Weaker_IO, (placehd/dim(pt2)[2]))}
  if(length(tblb3$Weaker)!=0){
    Weaker_IO = rbind(Weaker_IO, (tblb3$Weaker/dim(pt2)[2]))}
  else{
    placehd = tblb3$ns
    placehd[!is.na(placehd)] = 0
    Weaker_IO = rbind(Weaker_IO, (placehd/dim(pt2)[2]))}
  NS_IO = rbind(NS_IO, ((tblb3$ns)/dim(pt2)[2]))
  print(paste("Finished Timebin ", i))
}

colnames(Stronger_IO)= as.character(lat.range_IO)
colnames(Weaker_IO)= as.character(lat.range_IO)
colnames(NS_IO)= as.character(lat.range_IO)

rownames(Stronger_IO)= as.character(K)
rownames(Weaker_IO)= as.character(K)
rownames(NS_IO)= as.character(K)

#### 7. plotting heatmaps with null models for the Pacific Ocean (Supplementary Material Figure 12) #### 

t_Stronger_IO = t(Stronger_IO)
t_Weaker_IO = t(Weaker_IO)
t_NS_IO = t(NS_IO)

heatmap.2(t_Stronger_IO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ stronger pref in IO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "Indian Ecogroup - Stronger Preference")

heatmap.2(t_Weaker_IO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ weaker pref in IO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "Indian Eco - Weaker Preference")


heatmap.2(t_NS_IO,col=colfunc(80), trace="none", 
          key.xlab="Prop. of eco. w/ no pref in IO",
          key=TRUE, dendrogram = "none", Rowv = NULL, Colv = NULL, density.info="none", 
          revC = TRUE, breaks = legend_breaks, main = "Indian Eco- No Preference")





