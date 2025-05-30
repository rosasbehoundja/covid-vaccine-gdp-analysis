# COVID - VACCINE - GDP - ANALYSIS

## Description

This project analyzes the relationship between COVID-19 vaccination rates and Gross Domestic Product (GDP) from 2020 to 2023.

## Data

The project uses the following data files:

* **Raw Data:**
  * `data/raw/vaccination.csv`: Raw vaccination data.
  * `data/raw/world_bank_economic_indicators_2020_2023.csv`: Raw economic indicators data from the World Bank.
* **Cleaned Data:**
  * `data/cleaned/vaccination-cleaned.csv`: Cleaned vaccination data.
  * `data/cleaned/wbe_cleaned.csv`: Cleaned World Bank economic indicators data.
* **Processed Data:**
  * `data/processed/merged_data.csv`: Merged and processed data used for analysis.

## Scripts

The analysis is performed using the following R scripts:

* `scripts/01_fetch_data.r`: Fetches the raw data.
* `scripts/02_data_cleaning/vaccination_cleaning.r`: Cleans the raw vaccination data.
* `scripts/02_data_cleaning/wbe_cleaning.r`: Cleans the raw World Bank economic indicators data.
* `scripts/03_merged_data.r`: Merges the cleaned datasets.

## How to Run

1. Ensure you have R installed on your system.
2. Open an R environment.
3. Set the working directory to the root of this project.
4. Run the scripts in the following order:
   * `source("scripts/01_fetch_data.r")`
   * `source("scripts/02_data_cleaning/vaccination_cleaning.r")`
   * `source("scripts/02_data_cleaning/wbe_cleaning.r")`
   * `source("scripts/03_merged_data.r")`
5. The final merged dataset will be available at `data/processed/merged_data.csv`.
