# Divine
library(tidyverse)
library(psych) #pour le describe

#Charger le dataset
wbe <- read.csv("../data/raw/world_bank_economic_indicators_2020_2023.csv",stringsAsFactors = FALSE)

#Structure du dataset
str(wbe)
glimpse(wbe)

# 5 prmières lignes
head(wbe, 5)

#5 dernières lignes
tail(wbe, 5)

#Nbre de ligne et de colonnes
dim(wbe)

#Nom des colonnes
nom_colonnes_wbe <- colnames(wbe)
nom_colonnes_wbe
#Description du dataset
summary(wbe)
describe(wbe)
colSums(is.na(wbe))

#Valeurs aberrantes

ggplot(wbe, aes(x = region, y = gdp_per_capita_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB par habitant (en rouge)",
       x = "Région",
       y = "PIB par habitant") +
  theme_minimal()

ggplot(wbe, aes(x = region, y = gdp_total_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB total (en rouge)",
       x = "Région",
       y = "PIB total") +
  theme_minimal()

ggplot(wbe, aes(x = region, y =  unemployment_rate)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux de chomage (en rouge)",
       x = "Région",
       y = "Taux de chomage") +
  theme_minimal()

ggplot(wbe, aes(x = region, y = inflation_rate )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux d\'inflation (en rouge)",
       x = "Région",
       y = "Taux d\'inflation") +
  theme_minimal()

ggplot(wbe, aes(x = region, y = health_expenditure_pct_gdp)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé en % du PIB (en rouge)",
       x = "Région",
       y = "Dépenses de santé en % du PIB") +
  theme_minimal()

ggplot(wbe, aes(x = region, y = health_expenditure_per_capita )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé par habitants (en rouge)",
       x = "Région",
       y = "Dépenses de santé par habitant (USD)") +
  theme_minimal()

ggplot(wbe, aes(x = region, y =  population)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes de la population (en rouge)",
       x = "Région",
       y = "Population") +
  theme_minimal()

ggplot(wbe, aes(x = region, y =  gini_index  )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des  indices de Gini(en rouge)",
       x = "Région",
       y = "Indices de Gini(mesure des inégalité)") +
  theme_minimal()

#Suppression de gini_index
wbe <- wbe %>%
  select(-gini_index)
head(wbe)

numeric_cols <-sapply(wbe, is.numeric)
wbe[numeric_cols]<- scale(wbe[numeric_cols]) # Normalisation initiale, peut-être à revoir après traitement des outliers

# Traitement des valeurs aberrantes
winsorize <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1 # Renommé pour éviter conflit avec la fonction IQR
  
  return(if_else(x < (Q1 - 1.5 * IQR_val), Q1 - 1.5 * IQR_val,
                 if_else(x > (Q3 + 1.5 * IQR_val), Q3 + 1.5 * IQR_val, x)))
}

wbe <- wbe %>%
  mutate(across(c(gdp_per_capita_usd, inflation_rate, health_expenditure_per_capita), winsorize))

#Fonction pour remplacer les outliers par la médiane
impute_outliers_median <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  med <- median(x, na.rm = TRUE)
  
  return(if_else(x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val), med, x))
}
#Fonction pour remplacer les outliers par la moyenne
impute_outliers_mean <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  avg <- mean(x, na.rm = TRUE)
  
  return(if_else(x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val), avg, x))
}
wbe <- wbe %>%
  mutate(
    health_expenditure_pct_gdp = impute_outliers_mean(health_expenditure_pct_gdp),
    unemployment_rate = impute_outliers_median(unemployment_rate),
    gdp_total_usd = impute_outliers_median(gdp_total_usd)
  )
colSums(is.na(wbe))

# Visualisation après traitement des outliers (optionnel pour un étudiant, mais bon pour vérifier)
ggplot(wbe, aes(x = region, y = gdp_per_capita_usd)) + geom_boxplot(outlier.color = "blue") + theme_minimal()
ggplot(wbe, aes(x = region, y = gdp_total_usd)) + geom_boxplot(outlier.color = "blue") + theme_minimal()
ggplot(wbe, aes(x = region, y = unemployment_rate)) + geom_boxplot(outlier.color = "blue") + theme_minimal()
ggplot(wbe, aes(x = region, y = inflation_rate)) + geom_boxplot(outlier.color = "blue") + theme_minimal()
ggplot(wbe, aes(x = region, y = health_expenditure_pct_gdp)) + geom_boxplot(outlier.color = "blue") + theme_minimal()
ggplot(wbe, aes(x = region, y = health_expenditure_per_capita)) + geom_boxplot(outlier.color = "blue") + theme_minimal()

# Normalisation Z-score des colonnes numériques (après traitement des outliers et avant imputation des NA)
numeric_cols <-sapply(wbe, is.numeric)
wbe[numeric_cols]<- scale(wbe[numeric_cols])

#Valeurs manquantes
colSums(is.na(wbe))

#Remplacer les valeurs manquantes
#iso2c: 4 valeurs manquantes (Namibie 'NA' interprété comme NA)
wbe <- wbe %>%
  mutate(iso2c = if_else(is.na(iso2c) & country == "Namibia", "NA", iso2c)) # Correction plus ciblée

sum(is.na(wbe$iso2c))

#region: 8 valeurs manquantes
wbe <- wbe %>%
  mutate(region = if_else(is.na(region) & iso3c == "LCN", "Latin America & Caribbean", region),
         region = if_else(is.na(region) & iso3c == "SSF", "Sub-Saharan Africa", region))
sum(is.na(wbe$region))

#income: 8 valeurs manquantes
most_common_income <- wbe %>%
  filter(!is.na(income)) %>% # Exclure les NA avant de calculer le mode
  group_by(region) %>%
  summarise(most_common_income = names(which.max(table(income)))) # Mode plus robuste

wbe <- wbe %>%
  left_join(most_common_income, by = "region") %>%
  mutate(income = if_else(is.na(income), most_common_income, income)) %>%
  select(-most_common_income)
sum(is.na(wbe$income))

# gdp_per_capita_usd et gdp_total_usd
wbe <- wbe %>%
  mutate(gdp_total_usd = if_else(is.na(gdp_total_usd) & !is.na(gdp_per_capita_usd) & !is.na(population), gdp_per_capita_usd * population, gdp_total_usd),
         gdp_per_capita_usd = if_else(is.na(gdp_per_capita_usd) & !is.na(gdp_total_usd) & !is.na(population), gdp_total_usd / population, gdp_per_capita_usd))

wbe <- wbe %>%
  group_by(region,income)  %>%
  mutate(gdp_per_capita_usd = if_else(is.na(gdp_per_capita_usd), median(gdp_per_capita_usd, na.rm = TRUE),gdp_per_capita_usd),
         gdp_total_usd = if_else(is.na(gdp_total_usd),median(gdp_total_usd, na.rm = TRUE), gdp_total_usd)) %>%
  ungroup()
sum(is.na(wbe$gdp_per_capita_usd))
sum(is.na(wbe$gdp_total_usd))

# Imputation pour Corée du Nord et Venezuela si encore NA
wbe <- wbe %>%
  group_by(region) %>%
  mutate(gdp_per_capita_usd= if_else(is.na(gdp_per_capita_usd) & (iso3c == "PRK" | iso3c == "VEN"),mean(gdp_per_capita_usd[iso3c != "PRK" & iso3c != "VEN"],na.rm = TRUE),gdp_per_capita_usd),
         gdp_total_usd = if_else(is.na(gdp_total_usd) & (iso3c == "PRK" | iso3c == "VEN"),mean(gdp_total_usd[iso3c != "PRK" & iso3c != "VEN"], na.rm = TRUE), gdp_total_usd)) %>%
  ungroup()
sum(is.na(wbe$gdp_per_capita_usd))
sum(is.na(wbe$gdp_total_usd))

# Imputation par la médiane pour les autres colonnes numériques
wbe <- wbe %>%
  group_by(region, income) %>%
  mutate(
    unemployment_rate = if_else(is.na(unemployment_rate), median(unemployment_rate, na.rm = TRUE), unemployment_rate),
    inflation_rate = if_else(is.na(inflation_rate), median(inflation_rate, na.rm = TRUE), inflation_rate),
    health_expenditure_pct_gdp = if_else(is.na(health_expenditure_pct_gdp), median(health_expenditure_pct_gdp, na.rm = TRUE), health_expenditure_pct_gdp),
    health_expenditure_per_capita = if_else(is.na(health_expenditure_per_capita), median(health_expenditure_per_capita, na.rm = TRUE), health_expenditure_per_capita)
  ) %>%
  ungroup()
colSums(is.na(wbe))

# Vérification finale des NA
wbe %>%
  filter(if_any(everything(),is.na))

# Sauvegarde du fichier nettoyé
write.csv(wbe, "../data/cleaned/wbe_cleaned.csv", row.names = FALSE)

