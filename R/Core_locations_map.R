
# Map of sediment cores used in the study

# This script depicts the locations of cores used in this study on a global map and 
# generates the raw plot for Supplementary Figure 1. The final figure was refined and 
# created in Illustrator.

# set the working directory 
# setwd(" YOUR WORKING DIRECTORY ")

# load dataframes containing planktic foraminiferal data from the Triton database
# for the Atlantic, Pacific, and Indian Oceans

load("Triton_df_Oceans.RData")

load("Cores_3_Oceans.RData")

cores_Pacific = unique(Triton_Pacific$holeID)
cores_Indian = unique(Triton_Indian$holeID)
cores_Atlantic = unique(Triton_Atlantic$holeID)

cores_all = c(cores_Atlantic, cores_Indian, cores_Pacific)

occs = which(cores_3_Oceans$holeID %in% cores_all)

cores_map = cores_3_Oceans[occs,]

map <- map_data("world")

# Define color palette for the oceans

ocean_colors <- c("Atlantic" = "#1b9e77", "Pacific" = "#d95f02", "Indian" = "#7570b3")

# Plot the data
plot.map <- ggplot() +
  geom_polygon(data = map, aes(long, lat, group = group), 
               fill = "grey22", colour = "grey95") +
  labs(x = "Longitude [DecDeg]", y = "Latitude [DecDeg]") +
  geom_point(data = cores_map, aes(x = pal.long, y = pal.lat, fill = ocean), 
             colour = "grey", size = 3, shape = 21) +  # Using shape 21 with fill
  labs(title = "HoleIDs Map for cores 1.8 to 3.9 Ma") +
  
  # Use scale_fill_manual to apply the fill aesthetic
  scale_fill_manual(values = ocean_colors, drop = FALSE) +
  
  # Remove legend title and customize text size
  theme(legend.title = element_blank(),  # This removes the "ocean" title
        legend.text = element_text(size = 12))  # Adjust text size as needed


plot.map




