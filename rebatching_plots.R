
library(readr)
library(ggplate)
samples=read.csv('/Users/ssuds/Desktop/motrpac/tranche_3_muscle_rebatching.csv')
data=samples


# devtools::install_github("rolkra/ggplate")


library(ggplate)
library(dplyr)


#use this base code to set up all your folders:
base_dir = "/Volumes/suchitra/tranche_4/blood"

#change 1:x
for (i in 1:23) {
  batch_dir = file.path(base_dir, paste0("batch_", i))
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
  
  rna_dir = file.path(batch_dir, "rna")
  dna_dir = file.path(batch_dir, "dna")
  dir.create(rna_dir, showWarnings = FALSE)
  dir.create(dna_dir, showWarnings = FALSE)
  
  rna_subfolders = c("fragment_analyzer", "qubit", "normalization")
  sapply(file.path(rna_dir, rna_subfolders), dir.create, showWarnings = FALSE)
  
  dna_subfolders = c("fragment_analyzer", "quantit", "pooling", "iseq")
  sapply(file.path(dna_dir, dna_subfolders), dir.create, showWarnings = FALSE)
}

#Rebatching

library(readr)
library(ggplate)
library(ggplate)
library(dplyr)
library(ggplot2)

samples=read.csv('/Volumes/suchitra/tranche_4/blood/rebatching/rebatched_by_orig_box_tranche4blood.csv')
data=samples


generate_ggplate <- function(data, box_id, index) {
  box_data <- data %>% filter(Box == box_id)
  
  p <- plate_plot(
    data = box_data,
    position = Position,
    value = new_batch,
    label = new_batch,
    plate_size = 96,
    plate_type = "round",
    colour = colors,
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
