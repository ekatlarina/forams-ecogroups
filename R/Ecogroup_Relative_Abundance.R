
### Ecogroup Relative Abundance ####

# This script calculates ecogroup relative abundance and produces the raw plots 
# for Figure 3 of the manuscript, as well as Figure 2 and 8 of the supplementary material. 
# Final figures were created in Illustrator.

# install relevant packages for calculating ecogroup relative abundance

install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("viridis")

#load libraries
library(ggplot2) # used for plotting data
library(dplyr) # used for data manipulation and transformation
library(gridExtra) # used to arrange multiple grid-based plots
library(viridis) # used for the color scales

#### 1. Set paths for source files and configure the environment ####

# set the working directory 
# setwd(" YOUR WORKING DIRECTORY ")

# load dataframes containing planktonic foraminiferal data from the Triton database
# for the Atlantic, Pacific, and Indian Oceans

load("Triton_df_Oceans.RData")

# Filter data frames by hemisphere for each ocean
AO_NH = Triton_Atlantic[Triton_Atlantic$Hemisphere == "Northern",]
AO_SH = Triton_Atlantic[Triton_Atlantic$Hemisphere == "Southern",]
PO_NH = Triton_Pacific[Triton_Pacific$Hemisphere == "Northern",]
PO_SH = Triton_Pacific[Triton_Pacific$Hemisphere == "Southern",]
IO_NH = Triton_Indian[Triton_Indian$Hemisphere == "Northern",]
IO_SH = Triton_Indian[Triton_Indian$Hemisphere == "Southern",]

# define titles for each plot
title_AO_NH = "Atlantic Northern Hemisphere"
title_AO_SH = "Atlantic Southern Hemisphere"

title_PO_NH = "Pacific Northern Hemisphere"
title_PO_SH = "Pacific Southern Hemisphere"

title_IO_NH = "Indian Northern Hemisphere"
title_IO_SH = "Indian Southern Hemisphere"

#### 2. Execute the functions defined in the script  #### 

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
  
  # Define the ecogroup levels explicitly in the desired order
  ecogroup_levels <- c("5", "4", "3", "2", "1")
  
  # Reverse the order of ecogroups so that Ecogroup 5 is at the top, and explicitly set levels
  data$ecogroup <- factor(data$ecogroup, levels = ecogroup_levels)
  
  ggplot(data, aes(x = round.150K, y = proportion, fill = as.factor(ecogroup))) +
    geom_area(position = "stack", na.rm = TRUE) +  # Remove NAs automatically
    geom_vline(xintercept = c(3, 3.3), linetype = "solid", color = "mistyrose1", alpha = 0.5) +
    geom_vline(xintercept = c(2.5, 2.7), linetype = "solid", color = "lightcyan", alpha = 0.5) +
    geom_vline(xintercept = c(3.6, 3.9), linetype = "solid", color = "aliceblue", alpha = 0.5) +
    labs(x = "Age [Ma]", y = "Proportion", fill = "Ecogroup") +
    
    
    # Reverse the color order to match the ecogroups
   # scale_fill_manual(values = c("#045a8d", "#2b8cbe", "#74a9cf", "#bdc9e1", "#d0d1e6")) +
    
    # Reverse the color order to match the ecogroups
    scale_fill_manual(values = c("#7b3294", "#c2a5cf", "#e0e0e0", "#a6dba0", "#008837")) +
    
    
    scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                       limits = c(1.75, 3.75), expand = c(0.005, 0.005)) +
    ylim(0, 1) +
    ggtitle(plot_title) +
    # Increase axis text size
    theme_minimal() +
     theme(
      axis.text = element_text(size = 14),       # Adjust axis tick label size
      axis.title = element_text(size = 16),      # Adjust axis title size
      legend.text = element_text(size = 12),     # Adjust legend text size
      legend.title = element_text(size = 14),    # Adjust legend title size
      plot.title = element_text(size = 16, hjust = 0.5)  # Match title size with axis titles, center align it
    )
}

# the 'line_plot' function generates line plot charts to display the proportions of ecogroups
line_plot = function(data, plot_title) {
  ggplot(data, aes(x = round.150K, y = proportion)) +
    geom_hline(yintercept = seq(0.05, 0.4, by = 0.05), linetype = "solid", color = "grey97") +
    geom_rect(xmin = 3, xmax = 3.3, ymin = -Inf, ymax = Inf, fill = "mistyrose1", alpha = 0.1) +
    geom_rect(xmin = 2.5, xmax = 2.7, ymin = -Inf, ymax = Inf, fill = "lightcyan", alpha = 0.1) +
    geom_rect(xmin = 3.6, xmax = 3.9, ymin = -Inf, ymax = Inf, fill = "aliceblue", alpha = 0.8) +
    
    # Plot lines and points, only map color to ecogroup without affecting linetype in the legend
    geom_line(aes(group = factor(ecogroup, levels = c(5, 4, 3, 2, 1)), 
                  color = factor(ecogroup, levels = c(5, 4, 3, 2, 1))), 
              size = 1.2) +
    geom_point(aes(color = factor(ecogroup, levels = c(5, 4, 3, 2, 1))), size = 3) +
    
    # Assign colors to ecogroups
    scale_color_manual(values = c("#7b3294", "#c2a5cf", "#e0e0e0", "#a6dba0", "#008837")) +
    
    # Customize the plot
    theme(legend.position = "top",
          legend.key = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent"),
          panel.border = element_rect(fill = NA, color = "black"),
          axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 12),
          legend.title = element_blank()) +  # Remove the "color = factor(...)" part in the legend
    
    labs(x = "Age [Ma]", y = "Proportion") +
    ggtitle(plot_title) +
    
    scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                       limits = c(1.75, 3.85), expand = c(0.005, 0.005))
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

#### 3. Calculate count and proportions for each ecogroup within each time bin ####
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


#### 4. Plot the stacked area graphs: Figure 3 of the manuscript ####

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
print(stack_plot_PO_SH)
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



#### 5. Plot the multiple line graphs: Figure 2 of the supplementary material ####

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

#### 6. Run Spearman's rank correlation test ####

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

#### 7. Plot the scattered plot graphs: Figure 8 of the supplementary material ####

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


