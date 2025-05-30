required_packages <- c("dplyr",      # Data manipulation and transformation
                      "readr",       # Fast CSV reading/writing
                      "ggplot2",     # Data visualization (for potential plots)
                      "stringr",     # String manipulation and cleaning
                      "lubridate")   # Date/time handling

# Optional packages for enhanced functionality
optional_packages <- c("VIM",        # Visualization and Imputation of Missing values
                      "corrplot")    # Correlation plot visualization

# Function to safely load packages with automatic installation
load_package <- function(pkg) {
  # Check if package is already installed and loaded
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    # If not available, install from CRAN
    cat("Installing package:", pkg, "\n")
    install.packages(pkg)
    # Load after installation
    library(pkg, character.only = TRUE)
  }
}

# Apply package loading function to all required packages
invisible(lapply(required_packages, load_package))

# Check availability of optional packages (don't force installation)
vim_available <- require("VIM", quietly = TRUE)
corrplot_available <- require("corrplot", quietly = TRUE)

# Inform user about optional package status
if (!vim_available) cat("Note: VIM package not available - missing data visualizations will be skipped\n")
if (!corrplot_available) cat("Note: corrplot package not available - correlation plots will be skipped\n")

# =============================================================================
# 1. DATA LOADING AND INITIAL SETUP
# =============================================================================

# Load the vaccination dataset from CSV file
# Using read_csv for better parsing and type detection than base read.csv
df <- read_csv("data/raw/vaccination.csv")

# Remove the first column which is typically an auto-generated row index
# This column (often named ...1 or X) adds no analytical value
df <- df %>% select(-...1)

cat("✓ Data loaded successfully\n")

# =============================================================================
# 2. INITIAL DATA EXPLORATION AND PROFILING
# =============================================================================

# Display basic dataset structure information
cat("Dataset dimensions:", dim(df), "\n")  # Shows [rows, columns]

# Show all column names for structure understanding
cat("Column names:\n")
print(colnames(df))

# Display data types of each column to understand data structure
cat("Data types:\n")
print(sapply(df, class))

# Preview first 6 rows to understand data format and content
cat("First 6 rows:\n")
print(head(df))

# Preview last 6 rows to check for data consistency throughout
cat("Last 6 rows:\n")
print(tail(df))

# Generate comprehensive summary statistics for all columns
cat("\nSummary statistics:\n")
print(summary(df))

# =============================================================================
# 3. COMPREHENSIVE MISSING DATA ANALYSIS
# =============================================================================

# Calculate absolute count of missing values per column
missing_counts <- df %>% 
  summarise_all(~sum(is.na(.))) %>%  # Count NA values in each column
  pivot_longer(everything(),          # Convert to long format for analysis
               names_to = "column", 
               values_to = "missing_count") %>%
  arrange(desc(missing_count))        # Sort by highest missing count first

cat("Missing data counts by column:\n")
print(missing_counts)

# Calculate percentage of missing values per column (more interpretable)
missing_percentages <- df %>% 
  summarise_all(~round(mean(is.na(.)) * 100, 2)) %>%  # Convert to percentage
  pivot_longer(everything(), 
               names_to = "column", 
               values_to = "missing_percentage") %>%
  arrange(desc(missing_percentage))   # Sort by highest percentage first

cat("Missing data percentages by column:\n")
print(missing_percentages)

# Create visual missing data pattern analysis (if VIM package available)
# This helps identify systematic vs random missing data patterns
if (vim_available) {
  cat("Generating missing data visualization...\n")
  VIM::aggr(df, 
            col = c('navyblue', 'red'),    # Colors for present/missing data
            numbers = TRUE,                # Show percentage numbers on plot
            sortVars = TRUE)              # Sort variables by missing data amount
}

# =============================================================================
# 4. DUPLICATE DATA DETECTION AND REMOVAL
# =============================================================================

# Count duplicates before removal for reporting
initial_rows <- nrow(df)

# Remove exact duplicate rows (keeps first occurrence)
# distinct() removes rows where ALL columns have identical values
df <- df %>% distinct()

# Report on duplicate removal
final_rows <- nrow(df)
removed_duplicates <- initial_rows - final_rows
cat("Removed", removed_duplicates, "duplicate rows\n")

# =============================================================================
# 5. DATA TYPE CONVERSIONS AND TEMPORAL PROCESSING
# =============================================================================

# Convert date column from character to proper Date format
# Assumes MM/DD/YYYY format in source data
df$date <- as.Date(df$date, format = "%m/%d/%Y")

# Extract only the year from dates for annual analysis
# This aggregates data to yearly level rather than daily
df$date <- format(df$date, "%Y")

# Validate ISO country codes by length (should be 2-3 characters)
# Removes invalid entries that don't match standard ISO codes
initial_countries <- nrow(df)
df <- df[nchar(df$iso_code) <= 3, ]
removed_invalid_iso <- initial_countries - nrow(df)
if (removed_invalid_iso > 0) {
  cat("Removed", removed_invalid_iso, "rows with invalid ISO codes\n")
}

# Aggregate data to country-year level by averaging numeric values
# This handles cases where multiple records exist for same country-year
df <- df %>%
  group_by(iso_code, date) %>%
  summarise(across(where(is.numeric),           # Apply to all numeric columns
                   ~mean(.x, na.rm = TRUE)),    # Calculate mean, ignoring NAs
            .groups = "drop")                   # Remove grouping after operation

cat("✓ Data aggregated to country-year level\n")

# =============================================================================
# 6. OUTLIER DETECTION AND ANALYSIS
# =============================================================================

# Define function to detect outliers using Interquartile Range (IQR) method
# This is a robust method that works well for non-normal distributions
detect_outliers <- function(x) {
  # Calculate first and third quartiles
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  # Calculate Interquartile Range
  IQR <- Q3 - Q1
  
  # Define outliers as values beyond 1.5 * IQR from quartiles
  # This is the standard "box plot" definition of outliers
  outlier_flags <- x < Q1 - 1.5 * IQR | x > Q3 + 1.5 * IQR
  
  return(outlier_flags)
}

# Apply outlier detection to all numeric columns and report results
numeric_columns <- names(df)[sapply(df, is.numeric)]

cat("Outlier analysis by column:\n")
for (col in numeric_columns) {
  outlier_count <- sum(detect_outliers(df[[col]]), na.rm = TRUE)
  outlier_percentage <- round((outlier_count / sum(!is.na(df[[col]]))) * 100, 2)
  cat("  ", col, ":", outlier_count, "outliers (", outlier_percentage, "%)\n")
}

# Note: This script detects but does not remove outliers
# Domain expertise should guide outlier treatment decisions

# =============================================================================
# 7. STRING DATA CLEANING AND STANDARDIZATION
# =============================================================================

# Identify all character/text columns for cleaning
char_columns <- names(df)[sapply(df, is.character)]

# Define comprehensive text cleaning function
clean_text <- function(x) {
  x %>% 
    str_trim() %>%                          # Remove leading/trailing whitespace
    str_to_lower() %>%                      # Convert to lowercase for consistency
    str_replace_all("[[:punct:]]", "") %>%  # Remove all punctuation marks
    str_replace_all("\\s+", " ")            # Replace multiple spaces with single space
}

# Apply text cleaning to all character columns
cat("Cleaning text in columns:", paste(char_columns, collapse = ", "), "\n")
for (col in char_columns) {
  df[[col]] <- clean_text(df[[col]])
}

# Remove rows that are completely empty or contain only missing values
# any_vars() checks if ANY variable meets the condition
df <- df %>% 
  filter_all(any_vars(!is.na(.) & . != ""))

cat("✓ Text data standardized and empty rows removed\n")

# =============================================================================
# 8. MISSING VALUE IMPUTATION STRATEGY
# =============================================================================

# Create imputed version of dataset using domain-appropriate methods
df_imputed <- df %>%
  # For numeric columns: use median imputation (robust to outliers)
  mutate_if(is.numeric, ~ifelse(is.na(.), 
                                median(., na.rm = TRUE),  # Replace NA with median
                                .)) %>%
  # For character columns: use explicit "Unknown" category
  mutate_if(is.character, ~ifelse(is.na(.) | . == "", 
                                  "Unknown",              # Replace NA/empty with "Unknown"
                                  .))

# Report on imputation results
cat("Imputation summary:\n")
cat("  Numeric columns: Missing values replaced with column median\n")
cat("  Character columns: Missing values replaced with 'Unknown'\n")

# Verify no missing values remain after imputation
remaining_nas <- sum(is.na(df_imputed))
cat("  Remaining missing values:", remaining_nas, "\n")

# =============================================================================
# 9. DATA EXPORT AND PERSISTENCE
# =============================================================================

# Save cleaned dataset to CSV for further analysis
# Using write_csv for consistent formatting and UTF-8 encoding
write_csv(df_imputed, "cleaned_data.csv")
cat("✓ Cleaned data exported to 'cleaned_data.csv'\n")

# =============================================================================
# 10. DATA QUALITY REPORTING SYSTEM
# =============================================================================

# Function to generate comprehensive data quality report
create_data_quality_report <- function(data) {
  # Return structured list with key quality metrics
  list(
    # Basic structure information
    dimensions = dim(data),
    
    # Data type summary for each column
    column_types = sapply(data, class),
    
    # Missing data summary (should be 0 after imputation)
    missing_summary = data %>% 
      summarise_all(~sum(is.na(.))) %>%
      pivot_longer(everything(), 
                   names_to = "column", 
                   values_to = "missing_count"),
    
    # Statistical summary for numeric columns
    numeric_summary = data %>% 
      select_if(is.numeric) %>% 
      summary(),
    
    # Count of any remaining duplicate rows
    duplicate_count = sum(duplicated(data))
  )
}

# Generate quality report for cleaned data
data_quality_report <- create_data_quality_report(df_imputed)
cat("✓ Data quality report generated\n")

# =============================================================================
# 11. COLUMN NAME STANDARDIZATION
# =============================================================================

# Function to create database-friendly column names
standardize_column_names <- function(data) {
  # Apply systematic naming conventions
  names(data) <- names(data) %>%
    str_to_lower() %>%                      # Convert to lowercase
    str_replace_all("[[:space:]]+", "_") %>% # Replace spaces with underscores
    str_replace_all("[[:punct:]]", "") %>%   # Remove punctuation characters
    str_replace_all("_{2,}", "_") %>%        # Collapse multiple underscores
    str_remove("_$")                         # Remove trailing underscores
  
  return(data)
}

# Apply standardized naming to final dataset
df_final <- df_imputed %>% standardize_column_names()

# Display final column names for verification
cat("Standardized column names:\n")
print(colnames(df_final))

# Export final version with standardized names
write_csv(df_final, "cleaned_data_final.csv")

# =============================================================================
# COMPLETION SUMMARY
# =============================================================================

cat("\n" %+% strrep("=", 60) %+% "\n")
cat("DATA CLEANING PROCESS COMPLETED SUCCESSFULLY!\n")
cat(strrep("=", 60) %+% "\n")

# Final summary statistics
cat("Final dataset summary:\n")
cat("  Rows:", nrow(df_final), "\n")
cat("  Columns:", ncol(df_final), "\n")
cat("  Missing values:", sum(is.na(df_final)), "\n")
cat("  Duplicate rows:", sum(duplicated(df_final)), "\n")

# List output files created
cat("\nFiles created:\n")
cat("  - cleaned_data.csv (with original column names)\n")
cat("  - cleaned_data_final.csv (with standardized column names)\n")

cat("\nReady for analysis! \n")
