# Initialize an empty list to store the data from each batch
all_data = list()

# Define the path to the parent directory and the batches of interest
parent_dir = "/Volumes/CLARISA/Blood_Tranche_2/"
batches = c(5, 6, 7, 8, 10, 11, 12)

# Loop over each blood batch
for (batch in batches) {
  
  # Construct the folder path for each batch
  batch_dir = paste0(parent_dir, "batch_", batch, "/iseq/")
  
  # Get the SampleSheet file path (assuming there's only one file starting with SampleSheet)
  sample_file = list.files(batch_dir, pattern = "^SampleSheet", full.names = TRUE)
  
  # Check if a file is found
  if (length(sample_file) == 0) {
    warning(paste("No SampleSheet file found in", batch_dir))
    next
  }
  
  # Read in the CSV file
  data = read.csv(sample_file)
  
  # Add the 'pool' column with the appropriate batch label for blood
  data$pool = paste0("T2_Blood_", batch, "_RNAseq")
  
  # Remove the first 10 rows (i.e., headers)
  data = data[11:nrow(data), ]
  
  # Update column names (make sure the number of names matches the number of columns in your data)
  colnames(data) = c('viallabel', 'viallabel2', 'index', 'index2', 'pool')
  
  # Append the data to the list
  all_data[[length(all_data) + 1]] = data
}

# Now process the muscle_5 batch
muscle_dir = "/Volumes/CLARISA/Muscle-T1/muscle_5/iseq/"
muscle_file = list.files(muscle_dir, pattern = "^SampleSheet", full.names = TRUE)

# Check if the muscle SampleSheet file is found
if (length(muscle_file) == 0) {
  warning(paste("No SampleSheet file found in", muscle_dir))
} else {
  # Read in the CSV file
  muscle_data = read.csv(muscle_file)
  
  # Add the 'pool' column with the appropriate batch label for muscle
  muscle_data$pool = "T2_Muscle_5_RNAseq"
  
  # Remove the first 10 rows (i.e., headers)
  muscle_data = muscle_data[11:nrow(muscle_data), ]
  

  colnames(muscle_data) = c('viallabel', 'viallabel2', 'index', 'index2', 'pool')

  all_data[[length(all_data) + 1]] = muscle_data
}

final_data = do.call(rbind, all_data)

final_data = final_data[!grepl("^Sample", final_data$viallabel), ]

head(final_data)

dim=merge(final_data, dem, by.x='index', by.y='index')
final=dim%>%select(pool, viallabel, index, index2.y, index2.A_rev_complement)
colnames(final)=c('pool', 'viallabel', 'index', 'index2', 'index2.rev')
final = final[order(final$pool), ]
