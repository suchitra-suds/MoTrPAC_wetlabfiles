
library(readr)
library(ggplate)
samples=read.csv('/Users/ssuds/Desktop/motrpac/tranche_3_muscle_rebatching.csv')
data=samples

# Install ggplate package if not already installed
# devtools::install_github("rolkra/ggplate")

# Load required libraries
library(ggplate)
library(dplyr)


# Function to generate a ggplate for each unique Box
generate_ggplate <- function(data, box_id) {
  # Filter data for the specific box
  box_data <- data %>% filter(Box == box_id)
  
  # Create the ggplate
  p <- plate_plot(
    data = box_data,
    position = Position,
    value = new_batch,
    label = new_batch,
    plate_size = 96,
    plate_type = "round",
    colour =c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", "#E3BAFF"),
    show_legend = TRUE
  ) +
    ggtitle(paste("Plate for Box:", box_id)) +
 # Use a color scale for better visualization
    theme_minimal() 
  
  # Print the plot
  print(p)
  
  # Save the plot as an image file
  ggsave(paste0("ggplate_", box_id, ".png"), plot = p, width = 8, height = 6)
}

# Get unique box IDs
unique_boxes <- unique(data$Box)

# Generate and save ggplate for each unique Box
for (box_id in unique_boxes) {
  generate_ggplate(data, box_id)
}


