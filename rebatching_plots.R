library(readr)
library(ggplate)
library(dplyr)
library(ggplot2)

# Read data
samples = read.csv('/Volumes/suchitra/tranche_4/blood/rebatching/rebatch_from_chris/rebatched_by_orig_box_tranche4blood.csv')
setwd('/Volumes/suchitra/tranche_4/blood/rebatching/plots/')
data = samples

batch_colors = c("1" = "#EB1E2C", "2" = "#FFA07A", "3" = "#3c7320", "4" = "#a0819d", 
                 "5" = "#FF1493", "6" = "#FA982A", "7" = "#FFD700", "8" = "#FFFACD", 
                 "9" = "#BDB76B", "10" = "#E4CF41", "11" = "#ADFF2F", "12" = "#3CB371", 
                 "13" = "#008B8B", "14" = "#E0FFFF", "15" = "#B0C4DE", "16" = "#00BFFF", 
                 "17" = "#7DD5DC", "18" = "#e2c9f6", "19" = "#CD853F", "20" = "#DEB887", 
                 "21" = "#460f7a", "22" = "#778899", "23" = "#D3D3D3", "24" = "#A9DBBB")


data$new_batch = as.character(data$new_batch)


generate_ggplate <- function(data, box_id, index) {
  box_data <- data %>% filter(Box == box_id)
  
  p <- plate_plot(
    data = box_data,
    position = Position,  
    value = new_batch,    
    label = new_batch,    
    plate_size = 96,
    plate_type = "round",
    colour = batch_colors[box_data$new_batch],  
    show_legend = TRUE
  ) +
    ggtitle(paste("Plate for Box:", box_id)) +
    theme_minimal()
  
  print(p)
  
  filename = paste0(index, "_", box_id, ".png")
  ggsave(filename, plot = p, width = 8, height = 6)
}


unique_boxes <- unique(data$Box)

for (i in seq_along(unique_boxes)) {
  generate_ggplate(data, unique_boxes[i], i)
}


#Calculate progress:

library(dplyr)
library(readr)
library(tidyr)


plots_dir <- "/Volumes/suchitra/tranche_4/blood/rebatching/plots/"
file_names <- list.files(plots_dir, pattern = "\\.png$", full.names = FALSE)

box_id_codes <- file_names %>%
  gsub(".png", "", .) %>%
  .[order(as.numeric(sub("_.*", "", .)))]


data <- read.csv('/Volumes/suchitra/tranche_4/blood/rebatching/rebatch_from_chris/rebatched_by_orig_box_tranche4blood.csv')


data$new_batch <- as.character(data$new_batch)


unique_batches <- sort(unique(data$new_batch))  
batch_progress_counts <- as.data.frame(matrix(0, nrow = length(unique_batches), ncol = length(box_id_codes)))
rownames(batch_progress_counts) <- unique_batches
colnames(batch_progress_counts) <- box_id_codes


cumulative_counts <- setNames(rep(0, length(unique_batches)), unique_batches)


for (box_id_code in box_id_codes) {

  actual_box_id <- sub(".*_", "", box_id_code)
  
  temp_box <- data %>% filter(Box == actual_box_id)

  temp_counts <- table(temp_box$new_batch)
  
  for (batch in unique_batches) {
    count <- ifelse(batch %in% names(temp_counts), temp_counts[batch], 0)
    cumulative_counts[batch] <- cumulative_counts[batch] + count
    batch_progress_counts[batch, box_id_code] <- cumulative_counts[batch]
  }
}
batch_progress_counts <- batch_progress_counts[order(as.numeric(rownames(batch_progress_counts))), ]
print(batch_progress_counts)

final_counts <- batch_progress_counts[, ncol(batch_progress_counts)]
total_counts <- table(data$new_batch)  


validation <- data.frame(
  new_batch = rownames(batch_progress_counts),
  last_column = final_counts,
  total_counts = total_counts[rownames(batch_progress_counts)],
  match = final_counts == total_counts[rownames(batch_progress_counts)]
)

print(validation)

write.csv(batch_progress_counts, "batch_progress_counts.csv", row.names = TRUE)
write.csv(validation, "batch_progress_validation.csv", row.names = FALSE)


