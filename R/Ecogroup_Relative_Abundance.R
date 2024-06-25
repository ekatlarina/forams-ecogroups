
### Ecogroup Relative Abundance ####

# This script calculates ecogroup relative abundance and produces the raw plots 
#for Figure 3 of the manuscript, as well as Figure 2 and 8 of the supplementary material. 
# Final figures were created in Illustrator.

# install relevant packages for calculating ecogroup relative abundance

install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("virdis")

#load libraries
library(ggplot2) # used for plotting data
library(dplyr) # used for data manipulation and transformation
library(gridExtra) # used arranging multiple grid-based plots
library(viridis) # used for the color scales

#### 1. set paths for source files and configure the environment ####

# set the working directory 
# setwd(" YOUR WOKRING DIRECTORY ")

# load dataframes containing planktonic foraminiferal data from Triton database
# for the Atlantic, Pacific, and Indian Oceans

load("Triton_df_Oceans.RData")

# Filter dataframes by hemisphere for each ocean
AO_NH = Triton_Atlantic[Triton_Atlantic$Hemisphere == "Northern",]
AO_SH = Triton_Atlantic[Triton_Atlantic$Hemisphere == "Southern",]
PO_NH = Triton_Pacific[Triton_Pacific$Hemisphere == "Northern",]
PO_SH = Triton_Pacific[Triton_Pacific$Hemisphere == "Southern",]
IO_NH = Triton_Indian[Triton_Indian$Hemisphere == "Northern",]
IO_SH = Triton_Indian[Triton_Indian$Hemisphere == "Southern",]

# define titles for each plot
title_AO_NH = "Atlantic: Northern Hemisphere"
title_AO_SH = "Atlantic: Southern Hemisphere"

title_PO_NH = "Pacific: Northern Hemisphere"
title_PO_SH = "Pacific: Southern Hemisphere"

title_IO_NH = "Indian: Northern Hemisphere"
title_IO_SH = "Indian: Southern Hemisphere"

#### 2. execute the functions defined in the script  #### 

# the 'eco_proportion' function calculates the proportion of occurrences for each ecogroup 
# at each time bin

eco_proportion = function(df){
 return(df = df %>%
  group_by(round.150K, ecogroup) %>%
  summarise(count = n()) %>%
  group_by(round.150K) %>%
  mutate(proportion = count / sum(count)))

}

# the 'stacked_plot' function generates stacked area charts to display the proportions of ecogroups 

stacked_plot <- function(data, plot_title) {
  ggplot(data, aes(x = round.150K, y = proportion, fill = as.factor(ecogroup))) +
    geom_area(position = "stack") +
    geom_vline(xintercept = c(3, 3.3), linetype = "solid", color = "mistyrose1", alpha = 0.5) +
    geom_vline(xintercept = c(2.5, 2.7), linetype = "solid", color = "lightcyan", alpha = 0.5) +
    geom_vline(xintercept = c(3.6, 3.9), linetype = "solid", color = "aliceblue", alpha = 0.5) +
    labs(x = "Time", y = "Proportion", fill = "Ecogroup") +
    scale_fill_manual(values =c("#d0d1e6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")) +
    scale_x_continuous(breaks = seq(1.8, 3.75, by = 0.15), labels = seq(1.8, 3.75, by = 0.15), limits = c(1.75, 3.75), expand = c(0.005,0.005)) +
    ylim(0, 1) +
    ggtitle(plot_title) +
    theme_minimal()
}

# the 'line_plot' function generates line plot charts to display the proportions of ecogroups
line_plot = function(data,plot_title) {
  ggplot(data, aes(x = round.150K, y = proportion)) +
  geom_hline(yintercept = seq(0.05, 0.4, by = 0.05), linetype="solid", color = "grey97") + 
  geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.05) +
  geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.05) +
  geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.5) +
  geom_line(aes(linetype = "solid", group = ecogroup, color = ecogroup)) +
  geom_point(aes(color = ecogroup)) +
  scale_color_manual(values = c("#d0d1e6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) + 
  labs(x = "Age [Ma]", y = "Proportion") +
  ggtitle(plot_title) +
  scale_x_continuous(breaks =  unique(data$round.150K)) +
  guides (linetype = FALSE) 
  
}

# The scater_plot function generates scatter plot charts to display the relationships
# between eoogroup 1 and 5.
scatter_plot <- function(df, eco1, eco5, plot_title) {
  ggplot(df, aes(x = !!sym(eco1), y = !!sym(eco5))) +
    geom_point(aes(colour = age), size = 3, shape = 16) +
    scale_color_viridis(discrete = FALSE, option = "D", direction = 1, name = "Age [Ma]") +
    geom_smooth(method = "loess", se = FALSE, color = "gray21") +
    labs(title = plot_title,
         x = eco1,
         y = eco5)
}

#### 3. calculate count and proportions for each ecogroup within each time bin ####
proportion_dataAO_NH = eco_proportion(AO_NH)
proportion_dataAO_NH$ecogroup <- as.factor(proportion_dataAO_NH$ecogroup) # convert ecogroup to a factor
proportion_dataAO_SH = eco_proportion(AO_SH)
proportion_dataAO_SH$ecogroup <- as.factor(proportion_dataAO_SH$ecogroup) 


proportion_dataPO_NH = eco_proportion(PO_NH)
proportion_dataPO_NH$ecogroup <- as.factor(proportion_dataPO_NH$ecogroup) 
proportion_dataPO_SH = eco_proportion(PO_SH)
proportion_dataPO_SH$ecogroup <- as.factor(proportion_dataPO_SH$ecogroup) 

proportion_dataIO_NH = eco_proportion(IO_NH)
proportion_dataIO_NH$ecogroup <- as.factor(proportion_dataIO_NH$ecogroup) 
proportion_dataIO_SH = eco_proportion(IO_SH)
proportion_dataIO_SH$ecogroup <- as.factor(proportion_dataIO_SH$ecogroup) 

# create a dataframe containing the proportional data for ecogroups 1 and 5 
# across various regions

age = unique(Triton_Atlantic$round.150K)
df_ecogroups1_5 = data.frame(age = age,
                          Ecogroup1_AO_NH = proportion_dataAO_NH$proportion[proportion_dataAO_NH$ecogroup == 1],
                          Ecogroup1_AO_SH = proportion_dataAO_SH$proportion[proportion_dataAO_SH$ecogroup == 1],
                          Ecogroup1_PO_NH = proportion_dataPO_NH$proportion[proportion_dataPO_NH$ecogroup == 1],
                          Ecogroup1_PO_SH = proportion_dataPO_SH$proportion[proportion_dataPO_SH$ecogroup == 1],
                          Ecogroup1_IO_NH = proportion_dataIO_NH$proportion[proportion_dataIO_NH$ecogroup == 1],
                          Ecogroup1_IO_SH = proportion_dataIO_SH$proportion[proportion_dataIO_SH$ecogroup == 1],
                          
                          Ecogroup5_AO_NH = proportion_dataAO_NH$proportion[proportion_dataAO_NH$ecogroup == 5],
                          Ecogroup5_AO_SH = proportion_dataAO_SH$proportion[proportion_dataAO_SH$ecogroup == 5],
                          Ecogroup5_PO_NH = proportion_dataPO_NH$proportion[proportion_dataPO_NH$ecogroup == 5],
                          Ecogroup5_PO_SH = proportion_dataPO_SH$proportion[proportion_dataPO_SH$ecogroup == 5],
                          Ecogroup5_IO_NH = proportion_dataIO_NH$proportion[proportion_dataIO_NH$ecogroup == 5],
                          Ecogroup5_IO_SH = proportion_dataIO_SH$proportion[proportion_dataIO_SH$ecogroup == 5])


#### 4. plot the stacked area graphs: Figure 2 of the supplementary material ####

# Create the stacked area graph for the Northern Hemisphere ecogroups over time in the Atlantic Ocean
stack_plot_AO_NH = stacked_plot(proportion_dataAO_NH, title_AO_NH)
print(stack_plot_AO_NH)
# Create the stacked area graph for the Southern Hemisphere ecogroups over time in the Atlantic Ocean
stack_plot_AO_SH = stacked_plot(proportion_dataAO_SH, title_AO_SH)
print(stack_plot_AO_SH)
# Create the stacked area graph for the Northern Hemisphere ecogroups over time in the Pacific Ocean
stack_plot_PO_NH = stacked_plot(proportion_dataPO_NH, title_PO_NH)
print(stack_plot_PO_NH)
# Create the stacked area graph for the Southern Hemisphere ecogroups over time in the Pacific Ocean
stack_plot_PO_SH = stacked_plot(proportion_dataPO_SH, title_PO_SH)
print(stack_plot_AO_NH)
# Create the stacked area graph for the Northern Hemisphere ecogroups over time in the Indian Ocean
stack_plot_IO_NH = stacked_plot(proportion_dataIO_NH, title_IO_NH)
print(stack_plot_IO_NH)
# Create the stacked area graph for the Southern Hemisphere ecogroups over time in the Indian Ocean
stack_plot_IO_SH = stacked_plot(proportion_dataIO_SH, title_IO_SH)
print(stack_plot_IO_SH)

#Display plots on one page
combined_plot =  grid.arrange(stack_plot_AO_NH, stack_plot_AO_SH, 
                               stack_plot_PO_NH, stack_plot_PO_SH,
                               stack_plot_IO_NH, stack_plot_IO_SH,
                               ncol = 2, nrow = 3)


#### 5. plot the multiple line graphs: Figure 3 of the manuscript ####

# Create a line plot for the proportions of ecogroups in the Northern Hemisphere of the Atlantic Ocean
line_plot_AO_NH = line_plot(proportion_dataAO_NH, title_AO_NH)
print(line_plot_AO_NH)

# Create a plot for the the proportions of ecogroups in the Southern Hemisphere of the Atlantic Ocean
line_plot_AO_SH = line_plot(proportion_dataAO_SH, title_AO_SH)
print(line_plot_AO_SH)

# Create a plot for the proportions of ecogroups in the Northern Hemisphere of the Pacific Ocean
line_plot_PO_NH = line_plot(proportion_dataPO_NH, title_PO_NH)
print(line_plot_PO_NH)

# Create a plot for the proportions of ecogroups in the Southern Hemisphere of the Pacific Ocean
line_plot_PO_SH = line_plot(proportion_dataPO_SH, title_PO_SH)
print(line_plot_PO_SH)

# Create a plot for the proportions of ecogroups in the Northern Hemisphere of the Indian Ocean
line_plot_IO_NH = line_plot(proportion_dataIO_NH, title_IO_NH)
print(line_plot_IO_NH)

# Create a plot for the proportions of ecogroups in the Southern Hemisphere of the Indian Ocean
line_plot_IO_SH = line_plot(proportion_dataIO_SH, title_IO_SH)
print(line_plot_IO_SH)


combined_plot_2 =  grid.arrange(line_plot_AO_NH, line_plot_AO_SH, 
                              line_plot_PO_NH, line_plot_PO_SH,
                              line_plot_IO_NH, line_plot_IO_SH,
                               ncol = 2, nrow = 3)
 
#### 6. run Spearman's rank correlation test ####

# Atlantic: Northern Hemisphere
cor.test(df_ecogroups1_5$Ecogroup1_AO_NH, df_ecogroups1_5$Ecogroup5_AO_NH,
         method = "spearman") 
# Atlantic: Southern Hemisphere
cor.test(df_ecogroups1_5$Ecogroup1_AO_SH, df_ecogroups1_5$Ecogroup5_AO_SH,
         method = "spearman") 

# Pacific: Northern Hemisphere 
cor.test(df_ecogroups1_5$Ecogroup1_PO_NH, df_ecogroups1_5$Ecogroup5_PO_NH,
         method = "spearman") 
# Pacific: Southern Hemisphere 
cor.test(df_ecogroups1_5$Ecogroup1_PO_SH, df_ecogroups1_5$Ecogroup5_PO_SH,
         method = "spearman") 

# Indian: Northern Hemisphere
cor.test(df_ecogroups1_5$Ecogroup1_IO_NH, df_ecogroups1_5$Ecogroup5_IO_NH,
         method = "spearman") 
#Indian: Southern Hemisphere
cor.test(df_ecogroups1_5$Ecogroup1_IO_SH, df_ecogroups1_5$Ecogroup5_IO_SH,
         method = "spearman") 

#### 7. plot the scattered plot graphs: Figure 8 of the supplementary material ####

scatter_AO_NH = scatter_plot(df_ecogroups1_5, "Ecogroup1_AO_NH", "Ecogroup5_AO_NH", title_AO_NH)
print(scatter_AO_NH)

scatter_AO_SH = scatter_plot(df_ecogroups1_5, "Ecogroup1_AO_SH", "Ecogroup5_AO_SH", title_AO_SH)
print(scatter_AO_SH)

scatter_PO_NH = scatter_plot(df_ecogroups1_5, "Ecogroup1_PO_NH", "Ecogroup5_PO_NH", title_PO_NH)
print(scatter_PO_NH)

scatter_PO_SH = scatter_plot(df_ecogroups1_5, "Ecogroup1_PO_SH", "Ecogroup5_PO_SH", title_PO_SH)
print(scatter_PO_SH)

scatter_IO_NH = scatter_plot(df_ecogroups1_5, "Ecogroup1_IO_NH", "Ecogroup5_IO_NH", title_IO_NH)
print(scatter_IO_NH)

scatter_IO_SH = scatter_plot(df_ecogroups1_5, "Ecogroup1_IO_SH", "Ecogroup5_IO_SH", title_IO_SH)
print(scatter_IO_SH)

#Display plots on one page
combined_plot = grid.arrange(scatter_AO_NH, scatter_AO_SH,scatter_PO_NH, scatter_PO_SH,
                             scatter_IO_NH, scatter_IO_SH, ncol = 2, nrow = 3)

