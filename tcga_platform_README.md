# README: The Cancer Genome Atlas Integrated Data Analysis and Machine Learning Platform

## Purpose
This application provides a streamlined platform for researchers and data scientists to access, preprocess, visualize, and analyze data from The Cancer Genome Atlas (TCGA). Its key objectives include:

1. **Automated Data Download:** Simplify access to TCGA data, including gene expression, clinical metadata, and associated experimental details.
2. **Data Viewing and Preprocessing:** Enable efficient visualization and preparation of datasets for downstream analysis.
3. **Machine Learning Analysis:** Facilitate exploratory and predictive modeling using integrated datasets, with minimal setup.

## Features
1. **Data Download:**
   - Query and download gene expression, metadata, and clinical data for TCGA projects.
   - Supports multiple steps for filtering and selecting data.

2. **Data Visualization:**
   - Preview and filter datasets by gene or clinical attributes.
   - Generate histograms and bar charts for selected variables.

3. **Machine Learning Analysis:**
   - Encode categorical variables.
   - Train a Random Forest model on selected predictors and outcomes.
   - Evaluate model performance using metrics, feature importance, and confusion matrices.

## Installation

### Prerequisites
- Python 3.8+
- R with `TCGAbiolinks` and required packages installed.
- Libraries:
  - Python: `streamlit`, `pandas`, `matplotlib`, `seaborn`, `scikit-learn`
  - R: `TCGAbiolinks`, `SummarizedExperiment`, `DESeq2`

### Steps
1. Clone the repository:
   ```bash
   git clone https://github.com/your-repo/tcga-platform.git
   cd tcga-platform
   ```
2. Install Python dependencies:
   ```bash
   pip install -r requirements.txt
   ```
3. Ensure R and the required packages are installed:
   ```R
   install.packages("BiocManager")
   BiocManager::install("TCGAbiolinks")
   BiocManager::install("SummarizedExperiment")
   BiocManager::install("DESeq2")
   ```

## Usage

### Launch the Application
Run the Streamlit application:
```bash
streamlit run tcga_platform.py
```

## Contact
For questions or feedback, please contact [ssuds@stanford.edu].

