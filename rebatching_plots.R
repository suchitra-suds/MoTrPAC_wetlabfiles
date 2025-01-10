
library(readr)
library(ggplate)
samples=read.csv('/Users/ssuds/Desktop/motrpac/tranche_3_muscle_rebatching.csv')
data=samples


# devtools::install_github("rolkra/ggplate")


library(ggplate)
library(dplyr)


generate_ggplate <- function(data, box_id) {
  # Filter data for the specific box
  box_data <- data %>% filter(Box == box_id)
  

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
    theme_minimal() 
  
  print(p)
  
  ggsave(paste0("ggplate_", box_id, ".png"), plot = p, width = 8, height = 6)
}

unique_boxes <- unique(data$Box)

#saves plot for each unique box
for (box_id in unique_boxes) {
  generate_ggplate(data, box_id)
}


