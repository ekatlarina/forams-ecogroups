
# This script calculates fitted linear models of the relative abundances of 
# five ecogroups by hemisphere across the Atlantic, Pacific, and Indian Oceans.
# It produces the raw plots of figures 3 to 7 of the supplementary material. 
# Final figures were created in Illustrator.

#### 1. Set paths for source files and configure the environment. ####

# set the working directory 
# setwd(" YOUR WOKRING DIRECTORY ")

# load the dataframe containing ecogroups' proportional abundance data
# for the Atlantic, Pacific, and Indian Oceans

load("Ecogroup_proportions_one_df.RData")

# creating a dataset to plot with midpoints

# initialize the vector with your starting numbers
numbers <- seq(from = 1.8, to = 3.9, by=0.15)

# create a new vector to store the midpoints
midpoints <- seq(from = 1.875, to= 3.825, by = 0.15)

data_ecogroups$midpoints = midpoints

#### 2. Fitted linear model of Ecogroup 1 ####

# Fit a linear model for each hemisphere in each ocean
lm_eco1_AO_NH <- lm(Ecogroup1_AO_NH ~ age, data = data_ecogroups)
lm_eco1_AO_SH <- lm(Ecogroup1_AO_SH ~ age, data = data_ecogroups)
lm_eco1_PO_NH <- lm(Ecogroup1_PO_NH ~ age, data = data_ecogroups)
lm_eco1_PO_SH <- lm(Ecogroup1_PO_SH ~ age, data = data_ecogroups)
lm_eco1_IO_NH <- lm(Ecogroup1_IO_NH ~ age, data = data_ecogroups)
lm_eco1_IO_SH <- lm(Ecogroup1_IO_SH ~ age, data = data_ecogroups)

# Summarize models
summary_lm_eco1_AO_NH <- summary(lm_eco1_AO_NH)
summary_lm_eco1_AO_SH <- summary(lm_eco1_AO_SH)
summary_lm_eco1_PO_NH <- summary(lm_eco1_PO_NH)
summary_lm_eco1_PO_SH <- summary(lm_eco1_PO_SH)
summary_lm_eco1_IO_NH <- summary(lm_eco1_IO_NH)
summary_lm_eco1_IO_SH <- summary(lm_eco1_IO_SH)

# Extract p-values
p_eco1_AO_NH <- round(summary_lm_eco1_AO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco1_AO_SH <- round(summary_lm_eco1_AO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco1_PO_NH <- round(summary_lm_eco1_PO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco1_PO_SH <- round(summary_lm_eco1_PO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco1_IO_NH <- round(summary_lm_eco1_IO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco1_IO_SH <- round(summary_lm_eco1_IO_SH$coefficients["age", "Pr(>|t|)"], 3)

# Calculate the slope values
slope_eco1_AO_NH <- round(as.numeric(lm_eco1_AO_NH$coefficients[2]) * (-1), 3)
slope_eco1_AO_SH <- round(as.numeric(lm_eco1_AO_SH$coefficients[2]) * (-1), 3)

slope_eco1_PO_NH <- round(as.numeric(lm_eco1_PO_NH$coefficients[2]) * (-1), 3)
slope_eco1_PO_SH <- round(as.numeric(lm_eco1_PO_SH$coefficients[2]) * (-1), 3)

slope_eco1_IO_NH <- round(as.numeric(lm_eco1_IO_NH$coefficients[2]) * (-1), 3)
slope_eco1_IO_SH <- round(as.numeric(lm_eco1_IO_SH$coefficients[2]) * (-1), 3)


#### 3. Plotting Ecogroup 1 (Figure 3 of Supplementary Material)  ####

 # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) 
  

#  Atlantic
plot_eco1_AO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup1_AO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup1_AO_NH, color = "Northern Hemisphere"), 
              method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup1_AO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup1_AO_SH, color = "Southern Hemisphere"), 
              method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
 # scale_color_manual(values = c("Northern Hemisphere" = "#d8b365", "Southern Hemisphere" = "#5ab4ac")) 
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
  
  annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco1_AO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +  # Adjusted vjust
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco1_AO_SH, 3)),
           hjust = -0.1, vjust = 3, color = "black", size = 3) +  # Adjusted vjust
  
   annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco1_AO_NH, 3)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco1_AO_SH, 3)),
           hjust = -0.1, vjust = 6, color = "black", size = 3) +

  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Atlantic Ocean - Relative Abundance of Ecogroup 1")

plot_eco1_AO

#  Pacific
plot_eco1_PO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup1_PO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup1_PO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup1_PO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup1_PO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
 annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco1_PO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco1_PO_SH, 3)),
          hjust = -0.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco1_PO_NH, 5)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco1_PO_SH, 5)),
           hjust = -0.1, vjust = 6, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
 scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                    limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Pacific Ocean - Relative Abundance of Ecogroup 1")

plot_eco1_PO

#  Indian
plot_eco1_IO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup1_IO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup1_IO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup1_IO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup1_IO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  

  annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco1_IO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco1_IO_SH, 3)),
          hjust = -0.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco1_IO_NH, 5)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco1_IO_SH, 5)),
           hjust = -0.1, vjust = 6, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                   limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Indian Ocean - Relative Abundance of Ecogroup 1")

plot_eco1_IO

# Combine the plots using plot_grid

plots_eco1 <- plot_grid(plot_eco1_AO, plot_eco1_PO, plot_eco1_IO, ncol = 1, align = "v")

plots_eco1

#### 4. Fitted linear model of Ecogroup 2 ####

# Fit a linear model for each hemisphere in each ocean
lm_eco2_AO_NH <- lm(Ecogroup2_AO_NH ~ age, data = data_ecogroups)
lm_eco2_AO_SH <- lm(Ecogroup2_AO_SH ~ age, data = data_ecogroups)
lm_eco2_PO_NH <- lm(Ecogroup2_PO_NH ~ age, data = data_ecogroups)
lm_eco2_PO_SH <- lm(Ecogroup2_PO_SH ~ age, data = data_ecogroups)
lm_eco2_IO_NH <- lm(Ecogroup2_IO_NH ~ age, data = data_ecogroups)
lm_eco2_IO_SH <- lm(Ecogroup2_IO_SH ~ age, data = data_ecogroups)

# Summarize models
summary_lm_eco2_AO_NH =summary(lm_eco2_AO_NH)
summary_lm_eco2_AO_SH =summary(lm_eco2_AO_SH)
summary_lm_eco2_PO_NH =summary(lm_eco2_PO_NH)
summary_lm_eco2_PO_SH =summary(lm_eco2_PO_SH)
summary_lm_eco2_IO_NH =summary(lm_eco2_IO_NH)
summary_lm_eco2_IO_SH =summary(lm_eco2_IO_SH)

# Extracting p-value for the predictor 'age'
p_eco2_AO_NH <- round(summary_lm_eco2_AO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco2_AO_SH <- round(summary_lm_eco2_AO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco2_PO_NH <- round(summary_lm_eco2_PO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco2_PO_SH <- round(summary_lm_eco2_PO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco2_IO_NH <- round(summary_lm_eco2_IO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco2_IO_SH <- round(summary_lm_eco2_IO_SH$coefficients["age", "Pr(>|t|)"], 3)

# Calculate the slope 
slope_eco2_AO_NH <- round(as.numeric(lm_eco2_AO_NH$coefficients[2]) * (-1), 3)
slope_eco2_AO_SH <- round(as.numeric(lm_eco2_AO_SH$coefficients[2]) * (-1), 3)
slope_eco2_PO_NH <- round(as.numeric(lm_eco2_PO_NH$coefficients[2]) * (-1), 3)
slope_eco2_PO_SH <- round(as.numeric(lm_eco2_PO_SH$coefficients[2]) * (-1), 3)
slope_eco2_IO_NH <- round(as.numeric(lm_eco2_IO_NH$coefficients[2]) * (-1), 3)
slope_eco2_IO_SH <- round(as.numeric(lm_eco2_IO_SH$coefficients[2]) * (-1), 3)

#### 5. Plotting Ecogroup 2 (Figure 4 of Supplementary mMterial)  ####

#  Atlantic
plot_eco2_AO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +
  
  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup2_AO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup2_AO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup2_AO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup2_AO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
  
  annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco2_AO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +  # Adjusted vjust
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco2_AO_SH, 3)),
           hjust = -0.1, vjust = 3, color = "black", size = 3) +  # Adjusted vjust
  
   annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco2_AO_NH, 3)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco2_AO_SH, 3)),
           hjust = -0.1, vjust = 6, color = "black", size = 3) +

  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Atlantic Ocean - Relative Abundance of Ecogroup 2")

plot_eco2_AO

#  Pacific
plot_eco2_PO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +
  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup2_PO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup2_PO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup2_PO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup2_PO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
 annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco2_PO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco2_PO_SH, 3)),
          hjust = 1.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco2_PO_NH, 5)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco2_PO_SH, 5)),
           hjust = 1.1, vjust = 6, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
 scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                    limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Pacific Ocean - Relative Abundance of Ecogroup 2")

plot_eco2_PO

#  Indian
plot_eco2_IO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup2_IO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup2_IO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup2_IO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup2_IO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
  annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco2_IO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco2_IO_SH, 3)),
          hjust = 1.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco2_IO_NH, 5)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco2_IO_SH, 5)),
           hjust = 1.1, vjust = 6, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                   limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Indian Ocean - Relative Abundance of Ecogroup 2")

plot_eco2_IO

# Combine the plots using plot_grid

plots_eco2 <- plot_grid(plot_eco2_AO, plot_eco2_PO, plot_eco2_IO, ncol = 1, align = "v")

plots_eco2


#### 6. Fitted linear model of Ecogroup 3 ####

# Fit a linear model for each hemisphere in each ocean
lm_eco3_AO_NH <- lm(Ecogroup3_AO_NH ~ age, data = data_ecogroups)
lm_eco3_AO_SH <- lm(Ecogroup3_AO_SH ~ age, data = data_ecogroups)
lm_eco3_PO_NH <- lm(Ecogroup3_PO_NH ~ age, data = data_ecogroups)
lm_eco3_PO_SH <- lm(Ecogroup3_PO_SH ~ age, data = data_ecogroups)
lm_eco3_IO_NH <- lm(Ecogroup3_IO_NH ~ age, data = data_ecogroups)
lm_eco3_IO_SH <- lm(Ecogroup3_IO_SH ~ age, data = data_ecogroups)

# Summarize models
summary_lm_eco3_AO_NH =summary(lm_eco3_AO_NH)
summary_lm_eco3_AO_SH =summary(lm_eco3_AO_SH)
summary_lm_eco3_PO_NH =summary(lm_eco3_PO_NH)
summary_lm_eco3_PO_SH =summary(lm_eco3_PO_SH)
summary_lm_eco3_IO_NH =summary(lm_eco3_IO_NH)
summary_lm_eco3_IO_SH =summary(lm_eco3_IO_SH)

# Extracting p-value for the predictor 'age'
p_eco3_AO_NH <- round(summary_lm_eco3_AO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco3_AO_SH <- round(summary_lm_eco3_AO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco3_PO_NH <- round(summary_lm_eco3_PO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco3_PO_SH <- round(summary_lm_eco3_PO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco3_IO_NH <- round(summary_lm_eco3_IO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco3_IO_SH <- round(summary_lm_eco3_IO_SH$coefficients["age", "Pr(>|t|)"], 3)

# Calculate the slope 
slope_eco3_AO_NH <- round(as.numeric(lm_eco3_AO_NH$coefficients[2]) * (-1), 3)
slope_eco3_AO_SH <- round(as.numeric(lm_eco3_AO_SH$coefficients[2]) * (-1), 3)
slope_eco3_PO_NH <- round(as.numeric(lm_eco3_PO_NH$coefficients[2]) * (-1), 3)
slope_eco3_PO_SH <- round(as.numeric(lm_eco3_PO_SH$coefficients[2]) * (-1), 3)
slope_eco3_IO_NH <- round(as.numeric(lm_eco3_IO_NH$coefficients[2]) * (-1), 3)
slope_eco3_IO_SH <- round(as.numeric(lm_eco3_IO_SH$coefficients[2]) * (-1), 3)

#### 7. Plotting Ecogroup 3 (Figure 5 of Supplementary Material)  ####

#  Atlantic
plot_eco3_AO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +
  
  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup3_AO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup3_AO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup3_AO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup3_AO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  #scale_color_manual(values = c("Northern Hemisphere" = "#d8b365", "Southern Hemisphere" = "#5ab4ac")) +
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
  
  annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco3_AO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +  # Adjusted vjust
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco3_AO_SH, 3)),
           hjust = -0.1, vjust = 3, color = "black", size = 3) +  # Adjusted vjust
  
   annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco3_AO_NH, 3)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco3_AO_SH, 3)),
           hjust = -0.1, vjust = 6, color = "black", size = 3) +

  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                     limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Atlantic Ocean - Relative Abundance of Ecogroup 3")

plot_eco3_AO

#  Pacific
plot_eco3_PO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup3_PO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup3_PO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup3_PO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup3_PO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
 annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco3_PO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco3_PO_SH, 3)),
          hjust = -0.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco3_PO_NH, 5)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco3_PO_SH, 5)),
           hjust = -0.1, vjust = 6.5, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
 scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                    limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Pacific Ocean - Relative Abundance of Ecogroup 3")

plot_eco3_PO

#  Indian
plot_eco3_IO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup3_IO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup3_IO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup3_IO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup3_IO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  

  annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco3_IO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco3_IO_SH, 3)),
          hjust = -0.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco3_IO_NH, 5)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco3_IO_SH, 5)),
           hjust = -0.1, vjust = 6.5, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                   limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Indian Ocean - Relative Abundance of Ecogroup 3")

plot_eco3_IO

# Combine the plots using plot_grid

plots_eco3 <- plot_grid(plot_eco3_AO, plot_eco3_PO, plot_eco3_IO, ncol = 1, align = "v")

plots_eco3


#### 8. Fitter linear model of Ecogroup 4 ####

# Fit a linear model for each hemisphere in each ocean
lm_eco4_AO_NH <- lm(Ecogroup4_AO_NH ~ age, data = data_ecogroups)
lm_eco4_AO_SH <- lm(Ecogroup4_AO_SH ~ age, data = data_ecogroups)
lm_eco4_PO_NH <- lm(Ecogroup4_PO_NH ~ age, data = data_ecogroups)
lm_eco4_PO_SH <- lm(Ecogroup4_PO_SH ~ age, data = data_ecogroups)
lm_eco4_IO_NH <- lm(Ecogroup4_IO_NH ~ age, data = data_ecogroups)
lm_eco4_IO_SH <- lm(Ecogroup4_IO_SH ~ age, data = data_ecogroups)

# Summarize models
summary_lm_eco4_AO_NH =summary(lm_eco4_AO_NH)
summary_lm_eco4_AO_SH =summary(lm_eco4_AO_SH)
summary_lm_eco4_PO_NH =summary(lm_eco4_PO_NH)
summary_lm_eco4_PO_SH =summary(lm_eco4_PO_SH)
summary_lm_eco4_IO_NH =summary(lm_eco4_IO_NH)
summary_lm_eco4_IO_SH =summary(lm_eco4_IO_SH)

# Extracting p-value for the predictor 'age'
p_eco4_AO_NH <- round(summary_lm_eco4_AO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco4_AO_SH <- round(summary_lm_eco4_AO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco4_PO_NH <- round(summary_lm_eco4_PO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco4_PO_SH <- round(summary_lm_eco4_PO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco4_IO_NH <- round(summary_lm_eco4_IO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco4_IO_SH <- round(summary_lm_eco4_IO_SH$coefficients["age", "Pr(>|t|)"], 3)

# Calculate the slope 
slope_eco4_AO_NH <- round(as.numeric(lm_eco4_AO_NH$coefficients[2]) * (-1), 3)
slope_eco4_AO_SH <- round(as.numeric(lm_eco4_AO_SH$coefficients[2]) * (-1), 3)
slope_eco4_PO_NH <- round(as.numeric(lm_eco4_PO_NH$coefficients[2]) * (-1), 3)
slope_eco4_PO_SH <- round(as.numeric(lm_eco4_PO_SH$coefficients[2]) * (-1), 3)
slope_eco4_IO_NH <- round(as.numeric(lm_eco4_IO_NH$coefficients[2]) * (-1), 3)
slope_eco4_IO_SH <- round(as.numeric(lm_eco4_IO_SH$coefficients[2]) * (-1), 3)

#### 9. Plotting Ecogroup 4 (Figure 6 of supplementary material)  ####

#  Atlantic
plot_eco4_AO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +
  
  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup4_AO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup4_AO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup4_AO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup4_AO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
 
  annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco4_AO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +  # Adjusted vjust
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco4_AO_SH, 3)),
           hjust = 1.1, vjust = 3, color = "black", size = 3) +  # Adjusted vjust
  
   annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco4_AO_NH, 3)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco4_AO_SH, 3)),
           hjust = 1.1, vjust = 6, color = "black", size = 3) +

  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                     limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Atlantic Ocean - Relative Abundance of Ecogroup 4")

plot_eco4_AO

#  Pacific
plot_eco4_PO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup4_PO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup4_PO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup4_PO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup4_PO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  #scale_color_manual(values = c("Northern Hemisphere" = "#d8b365", "Southern Hemisphere" = "#5ab4ac")) +
   scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
 
 annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco4_PO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco4_PO_SH, 3)),
          hjust = 1.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco4_PO_NH, 5)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco4_PO_SH, 5)),
           hjust = 1.1, vjust = 6.5, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
 scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                    limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Pacific Ocean - Relative Abundance of Ecogroup 4")

plot_eco4_PO

#  Indian
plot_eco4_IO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup4_IO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup4_IO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup4_IO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup4_IO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  

  annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco4_IO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco4_IO_SH, 3)),
          hjust = 1.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco4_IO_NH, 5)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco4_IO_SH, 5)),
           hjust = 1.1, vjust = 6.5, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                   limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Indian Ocean - Relative Abundance of Ecogroup 4")

plot_eco4_IO

# Combine the plots using plot_grid

plots_eco4 <- plot_grid(plot_eco4_AO, plot_eco4_PO, plot_eco4_IO, ncol = 1, align = "v")

plots_eco4


#### 10. Fitted linear model of Ecogroup 5 ####

# Fit a linear model for each hemisphere in each ocean
lm_eco5_AO_NH <- lm(Ecogroup5_AO_NH ~ age, data = data_ecogroups)
lm_eco5_AO_SH <- lm(Ecogroup5_AO_SH ~ age, data = data_ecogroups)
lm_eco5_PO_NH <- lm(Ecogroup5_PO_NH ~ age, data = data_ecogroups)
lm_eco5_PO_SH <- lm(Ecogroup5_PO_SH ~ age, data = data_ecogroups)
lm_eco5_IO_NH <- lm(Ecogroup5_IO_NH ~ age, data = data_ecogroups)
lm_eco5_IO_SH <- lm(Ecogroup5_IO_SH ~ age, data = data_ecogroups)

# Summarize models
summary_lm_eco5_AO_NH =summary(lm_eco5_AO_NH)
summary_lm_eco5_AO_SH =summary(lm_eco5_AO_SH)
summary_lm_eco5_PO_NH =summary(lm_eco5_PO_NH)
summary_lm_eco5_PO_SH =summary(lm_eco5_PO_SH)
summary_lm_eco5_IO_NH =summary(lm_eco5_IO_NH)
summary_lm_eco5_IO_SH =summary(lm_eco5_IO_SH)

# Extracting p-value for the predictor 'age'
p_eco5_AO_NH <- round(summary_lm_eco5_AO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco5_AO_SH <- round(summary_lm_eco5_AO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco5_PO_NH <- round(summary_lm_eco5_PO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco5_PO_SH <- round(summary_lm_eco5_PO_SH$coefficients["age", "Pr(>|t|)"], 3)
p_eco5_IO_NH <- round(summary_lm_eco5_IO_NH$coefficients["age", "Pr(>|t|)"], 3)
p_eco5_IO_SH <- round(summary_lm_eco5_IO_SH$coefficients["age", "Pr(>|t|)"], 3)

# Calculate the slope 
slope_eco5_AO_NH <- round(as.numeric(lm_eco5_AO_NH$coefficients[2]) * (-1), 3)
slope_eco5_AO_SH <- round(as.numeric(lm_eco5_AO_SH$coefficients[2]) * (-1), 3)
slope_eco5_PO_NH <- round(as.numeric(lm_eco5_PO_NH$coefficients[2]) * (-1), 3)
slope_eco5_PO_SH <- round(as.numeric(lm_eco5_PO_SH$coefficients[2]) * (-1), 3)
slope_eco5_IO_NH <- round(as.numeric(lm_eco5_IO_NH$coefficients[2]) * (-1), 3)
slope_eco5_IO_SH <- round(as.numeric(lm_eco5_IO_SH$coefficients[2]) * (-1), 3)

#### 11. Plotting Ecogroup 5 (Figure 7 of supplementary material)  ####

#  Atlantic
plot_eco5_AO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +
  
  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup5_AO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup5_AO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup5_AO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup5_AO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  #scale_color_manual(values = c("Northern Hemisphere" = "#d8b365", "Southern Hemisphere" = "#5ab4ac")) +
   scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
 
  
  annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco5_AO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +  # Adjusted vjust
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco5_AO_SH, 3)),
           hjust = 1.1, vjust = 3, color = "black", size = 3) +  # Adjusted vjust
  
  annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco5_AO_NH, 3)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco5_AO_SH, 3)),
           hjust = 1.1, vjust = 6.5, color = "black", size = 3) +

  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
  scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                     limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Atlantic Ocean - Relative Abundance of Ecogroup 5")

plot_eco5_AO

#  Pacific
plot_eco5_PO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +

  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup5_PO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup5_PO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup5_PO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup5_PO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  
  annotate("text", x = Inf, y = Inf, label = paste("Slope NH:", round(slope_eco5_PO_NH, 3)), 
           hjust = 1.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("Slope SH:", round(slope_eco5_PO_SH, 3)),
          hjust = 1.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = Inf, y = Inf, label = paste("p-value NH:", round(p_eco5_PO_NH, 5)), 
           hjust = 1.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = Inf, y = Inf, label = paste("p-value SH:", round(p_eco5_PO_SH, 5)),
           hjust = 1.1, vjust = 6.5, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
 scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                    limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Pacific Ocean - Relative Abundance of Ecogroup 5")

plot_eco5_PO

#  Indian
plot_eco5_IO <- ggplot(data = data_ecogroups, aes(x = midpoints)) +
 
  # Plot the points and smooth lines with CIs
  geom_point(aes(y = Ecogroup5_IO_NH, color = "Northern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup5_IO_NH, color = "Northern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  geom_point(aes(y = Ecogroup5_IO_SH, color = "Southern Hemisphere")) +
  geom_smooth(aes(y = Ecogroup5_IO_SH, color = "Southern Hemisphere"), method = "lm", se = TRUE) +  # CI included
  
  # Updated colors
  scale_color_manual(values = c("Northern Hemisphere" = "#242f33", "Southern Hemisphere" = "#2b7dac")) +
  

  annotate("text", x = -Inf, y = Inf, label = paste("Slope NH:", round(slope_eco5_IO_NH, 3)), 
           hjust = -0.1, vjust = 1.5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("Slope SH:", round(slope_eco5_IO_SH, 3)),
          hjust = -0.1, vjust = 3, color = "black", size = 3) +
  
  annotate("text", x = -Inf, y = Inf, label = paste("p-value NH:", round(p_eco5_IO_NH, 5)), 
           hjust = -0.1, vjust = 5, color = "black", size = 3) +
  annotate("text", x = -Inf, y = Inf, label = paste("p-value SH:", round(p_eco5_IO_SH, 5)),
           hjust = -0.1, vjust = 6.5, color = "black", size = 3) +
  
  theme(legend.position = "top",
        legend.key = element_rect(fill = "transparent", colour = NA), 
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 12)) +
  
scale_x_continuous(breaks = seq(1.8, 3.8, by = 0.2), labels = seq(1.8, 3.8, by = 0.2), 
                   limits = c(1.75, 3.85), expand = c(0.005,0.005)) +
  
  labs(x = "Age [Ma]", y = "Relative Abundance", color = "Region") +
  ggtitle("Indian Ocean - Relative Abundance of Ecogroup 5")

plot_eco5_IO

# Combine the plots using plot_grid

plots_eco5 <- plot_grid(plot_eco5_AO, plot_eco5_PO, plot_eco5_IO, ncol = 1, align = "v")

plots_eco5


