#!/bin/bash

# Define the paths
fastq_dir="/projects/motrpac/seq_delivery/human_post_covid_tranche_1/muscle/novaseq/novaseq_muscle_1_2_3_4/fastq_raw"
metadata_file="/projects/motrpac/seq_delivery/human_post_covid_tranche_1/muscle/novaseq/novaseq_muscle_1_2_3_4/bic_submission/sample_metadata_muscle.csv"
samplesheet_file="/projects/motrpac/seq_delivery/human_post_covid_tranche_1/muscle/novaseq/novaseq_muscle_1_2_3_4/bic_submission/SampleSheet.csv"

# Task 1: Extract unique IDs from fastq_raw (before the first underscore)
unique_fastq_ids=$(find "$fastq_dir" -type f -name "*.fastq.gz" | sed -r 's|.*/([A-Za-z0-9]+)_.*|\1|' | sort | uniq)

# Task 2: Extract unique vial_label from sample_metadata_muscle.csv
# Find the column number of the vial_label column
vial_label_column=$(head -n 1 "$metadata_file" | sed 's/,/\n/g' | grep -n -w "vial_label" | cut -d':' -f1)

if [ -z "$vial_label_column" ]; then
  echo "vial_label column not found in $metadata_file"
  exit 1
fi

# Extract the unique vial labels from the vial_label column
unique_metadata_ids=$(cut -d',' -f"$vial_label_column" "$metadata_file" | tail -n +2 | sort | uniq)

# Task 3: Extract unique Sample_Name from SampleSheet.csv (from [Data] section)
unique_samplesheet_ids=$(awk -F, '/^\[Data\]/ {flag=1; next} flag' "$samplesheet_file" | cut -d',' -f2 | sort | uniq)

# Compare unique IDs across all three sources

# Task 4: Compare fastq_raw vs sample_metadata_muscle.csv
echo "Checking fastq_raw vs sample_metadata_muscle.csv..."
missing_in_metadata=$(comm -23 <(echo "$unique_fastq_ids") <(echo "$unique_metadata_ids"))
missing_in_fastq=$(comm -13 <(echo "$unique_fastq_ids") <(echo "$unique_metadata_ids"))

if [ -n "$missing_in_metadata" ]; then
  echo "The following IDs are in fastq_raw but not in sample_metadata_muscle.csv:"
  echo "$missing_in_metadata"
else
  echo "All fastq_raw IDs are present in sample_metadata_muscle.csv."
fi

if [ -n "$missing_in_fastq" ]; then
  echo "The following IDs are in sample_metadata_muscle.csv but not in fastq_raw:"
  echo "$missing_in_fastq"
else
  echo "All sample_metadata_muscle.csv IDs are present in fastq_raw."
fi

# Task 5: Compare SampleSheet.csv vs sample_metadata_muscle.csv
echo "Checking SampleSheet.csv vs sample_metadata_muscle.csv..."
missing_in_samplesheet=$(comm -23 <(echo "$unique_metadata_ids") <(echo "$unique_samplesheet_ids"))
missing_in_metadata_from_samplesheet=$(comm -13 <(echo "$unique_metadata_ids") <(echo "$unique_samplesheet_ids"))

if [ -n "$missing_in_samplesheet" ]; then
  echo "The following IDs are in sample_metadata_muscle.csv but not in SampleSheet.csv:"
  echo "$missing_in_samplesheet"
else
  echo "All sample_metadata_muscle.csv IDs are present in SampleSheet.csv."
fi

if [ -n "$missing_in_metadata_from_samplesheet" ]; then
  echo "The following IDs are in SampleSheet.csv but not in sample_metadata_muscle.csv:"
  echo "$missing_in_metadata_from_samplesheet"
else
  echo "All SampleSheet.csv IDs are present in sample_metadata_muscle.csv."
fi
