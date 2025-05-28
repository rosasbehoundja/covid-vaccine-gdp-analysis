if (!require(WDI)) install.packages("WDI")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(stringr)) install.packages("stringr")

library(stringr)
library(WDI)
library(dplyr)
library(readr)

wb_indicators <- c(
  "NY.GDP.PCAP.CD",        # PIB par capitale (en USD)
  "SL.UEM.TOTL.ZS",        # Taux de chômage (% de la population active totale)
  "FP.CPI.TOTL.ZG",        # Taux d'inflation (% annuel)
  "SH.XPD.CHEX.GD.ZS",     # Dépenses de santé courantes (% du PIB)
  "SP.POP.TOTL",           # Population totale
  "NY.GDP.MKTP.CD",        # PIB (en USD)
  "SH.XPD.CHEX.PC.CD",     # Dépenses de santé courantes par habitant (en USD)
  "SI.POV.GINI"            # Indice de GINI (estimation de la Banque mondiale)
)

# Téléchargement des données de la Banque mondiale
cat("World Bank data - 2020-2023...\n")

economic_data_wb <- WDI(
  country = "all",           # Télcharger pour tous les pays
  indicator = wb_indicators, # Les indicateurs que nous voulons
  start = 2020,             # Année de début
  end = 2023,               # Année de fin
  extra = TRUE,             # Inclure des variables supplémentaires comme la région, le niveau de revenu
  cache = NULL              # Utiliser des données fraîches (mettre à list() pour mettre en cache)
)

# Renommage des colonnes et nettoyage des données
economic_data_clean <- economic_data_wb %>%
  rename(
    gdp_per_capita_usd = NY.GDP.PCAP.CD,
    unemployment_rate = SL.UEM.TOTL.ZS,
    inflation_rate = FP.CPI.TOTL.ZG,
    health_expenditure_pct_gdp = SH.XPD.CHEX.GD.ZS,
    population = SP.POP.TOTL,
    gdp_total_usd = NY.GDP.MKTP.CD,
    health_expenditure_per_capita = SH.XPD.CHEX.PC.CD,
    gini_index = SI.POV.GINI
  ) %>%

  # Filtrer les données
  filter(!is.na(iso2c),                    # Supprimer les pays sans code ISO valide
         !region %in% c("Aggregates"),     # Supprimer les agrégats régionaux
         !str_detect(country, "income|Income")) %>%  # Supprimer les agrégats de groupes de revenus

  # Sélectionner et organiser les colonnes
  select(
    country, iso2c, iso3c, year, region, income,
    gdp_per_capita_usd, unemployment_rate, inflation_rate,
    health_expenditure_pct_gdp, health_expenditure_per_capita,
    population, gdp_total_usd, gini_index
  ) %>%

  # Supprimer les lignes où tous les indicateurs économiques sont manquants
  filter(
    !is.na(gdp_per_capita_usd) |
      !is.na(unemployment_rate) |
      !is.na(inflation_rate) |
      !is.na(health_expenditure_pct_gdp)
  )

# Sauvegarde des données
write_csv(economic_data_clean, "../data/raw/world_bank_economic_indicators_2020_2023.csv")


##########################
# Téléchargement des données de vaccination COVID-19

covid_vax <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
write_csv(covid_vax, "../data/raw/vaccinations.csv")
