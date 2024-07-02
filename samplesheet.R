# Prompt user for the batch they are working with
batch <- readline(prompt = "Enter the batch you are working with: ")

# Prompt user for the Traxcer file path, including the batch name in the prompt
traxcer_file_path <- readline(prompt = sprintf("Enter the full path to the %s Traxcer file: ", batch))

# Read the Traxcer file based on its extension
if (grepl("\\.csv$", traxcer_file_path)) {
  traxcer_data <- read.csv(traxcer_file_path, skip = 1, header = FALSE)
  colnames(traxcer_data) <- as.character(unlist(traxcer_data[1, ]))
  traxcer_data <- traxcer_data[-1, ]
} else if (grepl("\\.xlsx$", traxcer_file_path)) {
  library(readxl)
  traxcer_data <- read_excel(traxcer_file_path, skip = 1)
  colnames(traxcer_data) <- as.character(unlist(traxcer_data[1, ]))
  traxcer_data <- traxcer_data[-1, ]
}
traxcer_data <- traxcer_data[traxcer_data$Status != "No Tube", ]



# Prompt user for the Manifest file path
manifest_file_path <- readline(prompt = "Enter the full path to the Manifest file: ")


# Read the Manifest file based on its extension
if (grepl("\\.csv$", manifest_file_path)) {
  manifest <- read.csv(manifest_file_path, header = TRUE)
} else if (grepl("\\.xlsx$", manifest_file_path)) {
  library(readxl)
  manifest <- read_excel(manifest_file_path)
}


# Prompt user for the Demultiplex sequences file path
demultiplex_file_path <- readline(prompt = "Enter the full path to the Demultiplex sequences file: ")

# Read the Demultiplex file based on its extension
if (grepl("\\.csv$", demultiplex_file_path)) {
  demultiplex <- read.csv(demultiplex_file_path, header = TRUE)
} else if (grepl("\\.xlsx$", demultiplex_file_path)) {
  library(readxl)
  demultiplex <- read_excel(demultiplex_file_path)
}


# Prompt user for the DNA Library Summary file path, using the specified batch
dna_library_summary_file_path <- readline(prompt = sprintf("Enter the file path for the DNA library summary for %s: ", batch))

# Read the DNA Library Summary file based on its extension
if (grepl("\\.csv$", dna_library_summary_file_path)) {
  dna_library_summary <- read.csv(dna_library_summary_file_path, header = TRUE)
} else if (grepl("\\.xlsx$", dna_library_summary_file_path)) {
  library(readxl)
  dna_library_summary <- read_excel(dna_library_summary_file_path)
}

# Remove the first two rows
dna_library_summary <- dna_library_summary[-c(1, 2), ]
colnames(dna_library_summary) <- as.character(unlist(dna_library_summary[1, ]))
dna_library_summary <- dna_library_summary[-1, ]

# Merge the traxcer_data and manifest dataframes
merged_data <- merge(traxcer_data, manifest, by.x = "Tube ID", by.y = "2D Barcode", all = TRUE)
sample_info <- merged_data %>% filter(Status == "OK")

# Find the column with "A01", "B01", "C01", "D01" as the first few values
position_col <- which(colSums(sapply(dna_library_summary[1:4, ], function(x) x %in% c("A01", "B01", "C01", "D01"))) == 4)

# Find the column with values containing the pattern "_UDI_"
udi_col <- which(sapply(dna_library_summary, function(x) any(grepl("_UDI_", x))))

# Create the sample_sheet dataframe with the identified columns
sample_sheet <- dna_library_summary[, c(position_col, udi_col)]



for (i in 1:nrow(sample_sheet)) {
  if (!is.na(sample_sheet[i, 1]) && is.na(sample_sheet[i, 2])) {
    print(sample_sheet[i, ])
    
    # Prompt the user to decide whether to delete the row
    user_input <- readline(prompt = "Do you want to delete this row? Type 'yes' to delete or 'no' to keep: ")
    if (tolower(user_input) == "yes") {
      sample_sheet[i, ] <- NA  
    }
  }
}

sample_sheet <- na.omit(sample_sheet)



# Find the column in sample_sheet that contains '_UDI_'
udi_column_name <- names(sample_sheet)[sapply(sample_sheet, function(x) any(grepl("_UDI_", x)))]

sample_sheet <- merge(sample_sheet, demultiplex[, c("index_name", "index", "index2.A_rev_complement")],
                      by.x = udi_column_name, by.y = "index_name", all.x = TRUE)
udi_column <- sample_sheet[,udi_column_name]
prefixes <- as.integer(sub("^(.*?)_UDI_.*", "\\1", sample_sheet[, udi_column_name]))
sample_sheet$Sort_Key <- prefixes
sample_sheet=dplyr::arrange(sample_sheet, Sort_Key)

position_column_name <- which(sapply(sample_sheet, function(x) all(c("A01", "B01", "C01", "D01") %in% x)))
position_to_tubeID <- setNames(traxcer_data$'Tube ID', traxcer_data$Position)
sample_sheet$Tube_ID <- position_to_tubeID[sample_sheet[[position_column_name]]]


filtered_manifest <- manifest[manifest$`2D Barcode` %in% sample_sheet$Tube_ID, ]

viallabel <- data.frame(
  Viallabel = filtered_manifest$Viallabel,
  `2D Barcode` = filtered_manifest$`2D Barcode`,
  stringsAsFactors = FALSE
)

# Rename the column in viallabel to match sample_sheet for merging
names(viallabel)[names(viallabel) == "X2D.Barcode"] <- "Tube_ID"
sample_sheet <- merge(sample_sheet, viallabel, by = "Tube_ID")


names(sample_sheet)[names(sample_sheet) == "Viallabel"] <- "Sample_ID"
sample_sheet$Sample_Name <- sample_sheet$Sample_ID
desired_order <- c("Sample_ID", "Sample_Name", "index", "index2.A_rev_complement")
remaining_columns <- setdiff(names(sample_sheet), desired_order)
final_column_order <- c(desired_order, remaining_columns)
sample_sheet <- sample_sheet[, final_column_order]

# Find the column in sample_sheet that contains '_UDI_'
udi_column_name <- names(sample_sheet)[sapply(sample_sheet, function(x) any(grepl("_UDI_", x)))]

udi_column <- sample_sheet[,udi_column_name]
prefixes <- as.integer(sub("^(.*?)_UDI_.*", "\\1", sample_sheet[, udi_column_name]))
sample_sheet$Sort_Key <- prefixes
sample_sheet=dplyr::arrange(sample_sheet, Sort_Key)
sample_sheet=sample_sheet[,1:4]



#AUDIT

# Checking if Sort_Key is in numeric order
if (all(diff(sample_sheet$Sort_Key) >= 0)) {
  print("Checking Sort_Key is in order...")
  print("Sort_Key is in order.")
} else {
  print("Sort_Key is not in order.")
}

# Assuming we know the column names or have identified them earlier
udi_column_name <- names(sample_sheet)[sapply(sample_sheet, function(x) any(grepl("_UDI_", x)))]
position_column_name <- which(sapply(sample_sheet, function(x) all(c("A01", "B01", "C01", "D01") %in% x)))

# Extract the suffix following "_UDI_" and check against the position column
print("Checking adaptors match Well position...")
udi_suffixes <- sapply(sample_sheet[[udi_column_name]], function(x) sub(".*_UDI_(.*)", "\\1", x))
position_values <- sample_sheet[[position_column_name]]

mismatches <- udi_suffixes != position_values

# Check mismatches
if (any(mismatches)) {
  print("Adaptor mismatches detected:")
  # Print the rows where mismatches occur
  print(sample_sheet[mismatches, ])
} else {
  print("All adaptors match correctly.")
}


names(sample_sheet)[names(sample_sheet) == "index2.A_rev_complement"] <- "index2"

# Specify the expected column names
expected_col_names <- c("Sample_ID", "Sample_Name", "index", "index2")


if (!all(names(sample_sheet) %in% expected_col_names) || length(names(sample_sheet)) != length(expected_col_names)) {
  print("Column names do not match the expected values.")
  print("The actual column names in sample_sheet are:")
  print(names(sample_sheet))
} else {
  print("Column names are correct")
}

# Function to check for duplicates in each column and list rows with duplicates
check_duplicates <- function(df) {
  duplicate_rows <- list()  # List to store rows with duplicates for each column
  
  for (col in names(df)) {
    duplicates <- df[duplicated(df[[col]]) | duplicated(df[[col]], fromLast = TRUE), ]
    if (nrow(duplicates) > 0) {
      duplicate_rows[[col]] <- duplicates
    }
  }
  
  return(duplicate_rows)
}

duplicate_rows <- check_duplicates(sample_sheet)

# Print the results
if (length(duplicate_rows) == 0) {
  cat("No duplicates found in any column.\n")
} else {
  for (col in names(duplicate_rows)) {
    cat("Duplicates found in column:", col, "\n")
    print(duplicate_rows[[col]])
  }
}




experiment_name <- readline(prompt = "Enter the Experiment Name: ")
experiment_date <- as.character(Sys.Date())
description <- readline(prompt = "Enter the Description: ")

new_rows <- data.frame(
  Experiment = c("Experiment Name", "", "Date", "Description", "", "[Reads]", "101", "101", "[Settings]", "", "[Data]"),
  Data = c(experiment_name, "", experiment_date, description, "", "", "", "", "", "", ""),
  Blank1 = rep("",11),  # First additional blank column
  Blank2 = rep("",11),  # Second additional blank column
  stringsAsFactors = FALSE
)




header=colnames(sample_sheet)
sample_sheet <- rbind(header, sample_sheet)
colnames(sample_sheet)=NULL
colnames(new_rows)=NULL


file_path <- readline(prompt = "Enter the file path where you want to save the files: ")

if (substr(file_path, nchar(file_path), nchar(file_path)) != "/") {
  file_path <- paste0(file_path, "/")
}


header_filename <- paste0(file_path, "header_", batch, ".csv")
sample_sheet_filename <- paste0(file_path, "sample_sheet_", batch, ".csv")

write.csv(new_rows, header_filename, row.names = FALSE)

write.csv(sample_sheet, sample_sheet_filename, row.names = FALSE)

cat("Files have been saved:\n")
cat("Header file:", header_filename, "\n")
cat("Sample sheet file:", sample_sheet_filename, "\n")



# Prompt the user for the file path of the pool data file
pool_file_path <- readline(prompt = "Enter the file path for the balance and pool file: ")

# Check the file extension and read the file accordingly
if (grepl("\\.csv$", pool_file_path)) {
  pool_data <- read.csv(pool_file_path)
} else if (grepl("\\.xlsx$", pool_file_path)) {
  pool_data <- read_excel(pool_file_path)
} else {
  stop("Unsupported file format. Please provide a .csv or .xlsx file.")
}

# Print the structure of the read pool data
colnames(pool_data)=pool_data[1,]
pool_data=pool_data[-1,]
pool_data <- pool_data[!is.na(pool_data$'Base Pair Length'), ]

pool_name <- readline(prompt = "Enter pool name: ")

# Create the redcap dataframe with specified columns and populate them
redcap <- data.frame(
  pass_linear_barcode = traxcer_data$`Tube ID`,
  rnaext_tissue_type = pool_data$`Tissue`,
  balance_and_pool_initial_pool_name = rep(pool_name, nrow(traxcer_data)),
  scan_position = traxcer_data$Position,
  balance_and_pool_final_to_mix_ul = pool_data$`Final to mix`,
  stringsAsFactors = FALSE
)

redcap <- redcap[!is.na(redcap$pass_linear_barcode) & redcap$pass_linear_barcode != "", ]
# Print the redcap dataframe to verify
# Prompt the user for the file path to save the redcap CSV
redcap_file_path <- readline(prompt = "Enter the file path to save the redcap file (including filename and .csv extension): ")
batch = readline(prompt = "Enter batch number in format batch_X :  ")

write.csv(redcap, paste0(redcap_file_path, "redcap_", batch, ".csv"), row.names = FALSE)
cat("The redcap file has been saved to:", redcap_file_path, "\n")


library(readxl)

# Prompt the user for the file path of the pool data file
pool_file_path <- readline(prompt = "Enter the file path for the balance and pool file: ")

# Check the file extension and read the file accordingly
if (grepl("\\.csv$", pool_file_path)) {
  pool_data <- read.csv(pool_file_path)
} else if (grepl("\\.xlsx$", pool_file_path)) {
  pool_data <- read_excel(pool_file_path)
} else {
  stop("Unsupported file format. Please provide a .csv or .xlsx file.")
}

# Print the structure of the read pool data
colnames(pool_data)=pool_data[1,]
pool_data=pool_data[-1,]
pool_data <- pool_data[!is.na(pool_data$'Base Pair Length'), ]

pool_name <- readline(prompt = "Enter pool name: ")

# Create the redcap dataframe with specified columns and populate them
redcap <- data.frame(
  pass_linear_barcode = traxcer_data$`Tube ID`,
  rnaext_tissue_type = rep("human blood", nrow(redcap$pass_linear_barcode)),
  balance_and_pool_initial_pool_name = rep(pool_name, nrow(traxcer_data)),
  scan_position = traxcer_data$Position,
  balance_and_pool_final_to_mix_ul = pool_data$`Final to mix`,
  stringsAsFactors = FALSE
)

redcap <- redcap[!is.na(redcap$pass_linear_barcode) & redcap$pass_linear_barcode != "", ]
# Print the redcap dataframe to verify
# Prompt the user for the file path to save the redcap CSV
redcap_file_path <- readline(prompt = "Enter the file path to save the redcap file (including filename and .csv extension): ")
batch = readline(prompt = "Enter batch number in format batch_X :  ")

write.csv(redcap, paste0(redcap_file_path, "redcap_", batch, ".csv"), row.names = FALSE)
cat("The redcap file has been saved to:", redcap_file_path, "\n")









