# Divine
library(tidyverse)
library(psych) #pour le describe
#Charger le dataset
wbe <- read.csv("covid-vaccine-gdp-analysis/data/raw/world_bank_economic_indicators_2020_2023.csv",stringsAsFactors = FALSE)
#Voir le dataset
View(wbe)

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
"""
- gdp_per_capita_usd : Très dispersé (sd = 29354.96) avec une forte asymétrie (skew = 3.26), ce qui indique que certains pays ont un PIB par habitant beaucoup plus élevé que les autres.
- inflation_rate : Très élevée et extrêmement variable (sd = 29.91), avec une forte asymétrie (skew = 12.31). Certains pays ont des taux d'inflation très extrêmes.
- gini_index (indice d'inégalité) : n = 168, ce qui signifie qu’il manque beaucoup de valeurs. Distribution légèrement asymétrique (skew = 0.56), indiquant une légère concentration vers des valeurs élevées.


"""

#Valeurs aberrantes

#gdp_per_capita_usd

ggplot(wbe, aes(x = region, y = gdp_per_capita_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB par habitant (en rouge)",
       x = "Région",
       y = "PIB par habitant") +
  theme_minimal()

#gdp_total_usd
ggplot(wbe, aes(x = region, y = gdp_total_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB total (en rouge)",
       x = "Région",
       y = "PIB total") +
  theme_minimal()

#unemployment_rate
ggplot(wbe, aes(x = region, y =  unemployment_rate)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux de chomage (en rouge)",
       x = "Région",
       y = "Taux de chomage") +
  theme_minimal()

#inflation_rate 
ggplot(wbe, aes(x = region, y = inflation_rate )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux d'inflation (en rouge)",
       x = "Région",
       y = "Taux d'inflation") +
  theme_minimal()

#health_expenditure_pct_gdp
ggplot(wbe, aes(x = region, y = health_expenditure_pct_gdp)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé en % du PIB (en rouge)",
       x = "Région",
       y = "Dépenses de santé en % du PIB") +
  theme_minimal()

#health_expenditure_per_capita
ggplot(wbe, aes(x = region, y = health_expenditure_per_capita )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé par habitants (en rouge)",
       x = "Région",
       y = "Dépenses de santé par habitant (USD)") +
  theme_minimal()

#population
ggplot(wbe, aes(x = region, y =  population)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes de la population (en rouge)",
       x = "Région",
       y = "Population") +
  theme_minimal()

# gini_index
ggplot(wbe, aes(x = region, y =  gini_index  )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des  indices de Gini(en rouge)",
       x = "Région",
       y = "Indices de Gini(mesure des inégalité)") +
  theme_minimal()


"""
On remarque au niveau de l'indice de Gini que les valeurs manquantes dépassent 80% du dataset 
 Avec 80 % de NA, toute moyenne ou régression incluant cette variable sera biaisée ou inutile.
 On ne peut pas l'imputer de manière fiable le Gini dépend de la structure des revenus, ce qu’aucune autre variable ne permet d’estimer correctement ici.
 Il introduit des lignes inexploitables ou rend les modèles plus complexes sans réel gain analytique.
 Conclusion : Nous allons supprimer cette colonne.Mais le Gini est indispensable si ton objectif est de comprendre ou visualiser les disparités économiques entre pays ou région
"""
#Suppression de gini_index
wbe <- wbe %>%
  select(-gini_index)
head(wbe)

numeric_cols <-sapply(wbe, is.numeric)
wbe[numeric_cols]<- scale(wbe[numeric_cols])

wbe_outliers <- wbe %>%
  mutate(
    Q1 = quantile(gdp_per_capita_usd, 0.25, na.rm = TRUE),
    Q3 = quantile(gdp_per_capita_usd, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    outlier = if_else(gdp_per_capita_usd < (Q1 - 1.5 * IQR) | gdp_per_capita_usd > (Q3 + 1.5 * IQR),
                      "Aberrant", "Normal")
  )

"""
Nous allons mtn remplacer les valeurs aberrantes avec la winsorisation et l'inputation par la médiane ou la moyenne selon le cas

| gdp_per_capita_usd | Winsorisation des extrêmes pour éviter les biais | 
| unemployment_rate | Imputation par la médiane régionale pour stabiliser | 
| inflation_rate | Winsorisation ou remplacement par tendance moyenne | 
| health_expenditure_pct_gdp | Imputation par moyenne si peu de valeurs aberrantes | 
| health_expenditure_per_capita | Winsorisation des valeurs élevées pour éviter distorsion | 
| population | Aucune modification (données précises sauf erreur) | 
| gdp_total_usd | Ajustement via médiane régionale pour limiter extrêmes | 

"""
winsorize <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  return(if_else(x < (Q1 - 1.5 * IQR), Q1 - 1.5 * IQR,
                 if_else(x > (Q3 + 1.5 * IQR), Q3 + 1.5 * IQR, x)))
}

wbe <- wbe %>%
  mutate(across(c(gdp_per_capita_usd, inflation_rate, health_expenditure_per_capita), winsorize))
##Fonction pour remplacer les ouliers par la médiane
impute_outliers_median <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  med <- median(x, na.rm = TRUE)
  
  return(if_else(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR), med, x))
}
#Fonction pour remplacer les ouliers par la moyenne
impute_outliers_mean <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  avg <- mean(x, na.rm = TRUE)
  
  return(if_else(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR), avg, x))
}
wbe <- wbe %>%
  mutate(
    health_expenditure_pct_gdp = impute_outliers_mean(health_expenditure_pct_gdp),
    unemployment_rate = impute_outliers_median(unemployment_rate),
    gdp_total_usd = impute_outliers_median(gdp_total_usd)
  )
colSums(is.na(wbe))

ggplot(wbe, aes(x = region, y = gdp_per_capita_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB par habitant (en rouge)",
       x = "Région",
       y = "PIB par habitant") +
  theme_minimal()

#gdp_total_usd
ggplot(wbe, aes(x = region, y = gdp_total_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB total (en rouge)",
       x = "Région",
       y = "PIB total") +
  theme_minimal()

#unemployment_rate
ggplot(wbe, aes(x = region, y =  unemployment_rate)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux de chomage (en rouge)",
       x = "Région",
       y = "Taux de chomage") +
  theme_minimal()

#inflation_rate 
ggplot(wbe, aes(x = region, y = inflation_rate )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux d'inflation (en rouge)",
       x = "Région",
       y = "Taux d'inflation") +
  theme_minimal()

#health_expenditure_pct_gdp
ggplot(wbe, aes(x = region, y = health_expenditure_pct_gdp)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé en % du PIB (en rouge)",
       x = "Région",
       y = "Dépenses de santé en % du PIB") +
  theme_minimal()

#health_expenditure_per_capita
ggplot(wbe, aes(x = region, y = health_expenditure_per_capita )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé par habitants (en rouge)",
       x = "Région",
       y = "Dépenses de santé par habitant (USD)") +
  theme_minimal()

#population
ggplot(wbe, aes(x = region, y =  population)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes de la population (en rouge)",
       x = "Région",
       y = "Population") +
  theme_minimal()

boxplot.stats(wbe$gdp_per_capita_usd)$out
summary(wbe$gdp_per_capita_usd)
#Même après le nettoyage, il reste des valeurs considérées comme aberrantes selon la méthode IQR.C’est normal avec des données économiques mondiales : certains pays (ex. Luxembourg, Suisse) ont des PIB par habitant légitimement très élevés
#On va donc normaliser les données avec le z-score c'est la formule la plus adaptée pour ce jeu de donné
numeric_cols <-sapply(wbe, is.numeric)
wbe[numeric_cols]<- scale(wbe[numeric_cols])
ggplot(wbe, aes(x = region, y = gdp_per_capita_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB par habitant (en rouge)",
       x = "Région",
       y = "PIB par habitant") +
  theme_minimal()

#gdp_total_usd
ggplot(wbe, aes(x = region, y = gdp_total_usd)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du PIB total (en rouge)",
       x = "Région",
       y = "PIB total") +
  theme_minimal()

#unemployment_rate
ggplot(wbe, aes(x = region, y =  unemployment_rate)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux de chomage (en rouge)",
       x = "Région",
       y = "Taux de chomage") +
  theme_minimal()

#inflation_rate 
ggplot(wbe, aes(x = region, y = inflation_rate )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes du taux d'inflation (en rouge)",
       x = "Région",
       y = "Taux d'inflation") +
  theme_minimal()

#health_expenditure_pct_gdp
ggplot(wbe, aes(x = region, y = health_expenditure_pct_gdp)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé en % du PIB (en rouge)",
       x = "Région",
       y = "Dépenses de santé en % du PIB") +
  theme_minimal()

#health_expenditure_per_capita
ggplot(wbe, aes(x = region, y = health_expenditure_per_capita )) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes des dépenses de santé par habitants (en rouge)",
       x = "Région",
       y = "Dépenses de santé par habitant (USD)") +
  theme_minimal()

#population
ggplot(wbe, aes(x = region, y =  population)) +
  geom_boxplot(outlier.color = "red") +
  labs(title = "Valeurs aberrantes de la population (en rouge)",
       x = "Région",
       y = "Population") +
  theme_minimal()

#Valeurs manquantes

colSums(is.na(wbe))



#Remplacer les valeurs manquantes

#iso2c on remarque qu'on a 4 valeurs manquantes
wbe  %>%
  filter(is.na(iso2c)) #pour afficher les lignes qui ont des valeurs manquantes dans iso2c
#On remarque que ces 4 valeurs ne sont la mais R les voit comme des valeurs manquantes car l'iso2c de la Namibie est NA 
unique(wbe$iso2c) #On voit clairement que NA n'est pas entre les cotes ce pourquoi il est considéré comme une valeur manquantes
#Nous allons corriger ça en ajoutant le cotes
wbe <- wbe %>%
  mutate(iso2c = if_else(is.na(iso2c), "NA", iso2c))

sum(is.na(wbe$iso2c))
unique(wbe$iso2c)
colSums(is.na(wbe))

#region il y a 8 valeurs manquantes 
wbe  %>%
  filter(is.na(region))# les pays des régions manquantes sont Latin America & Caribbean et Sub-Saharan Africa et ce sont également des régions donc on va remplacer régions par le nom du pays
wbe <- wbe %>%
  mutate(region = if_else(is.na(region) & iso3c == "LCN", "Latin America & Caribbean", region),
         region = if_else(is.na(region) & iso3c == "SSF", "Sub-Saharan Africa", region))
sum(is.na(wbe$region))
#colSums(is.na(wbe))


#income il y a 8 valeurs manquantes
wbe  %>%
  filter(is.na(income)) #Les valeurs manquantes de income correspondent à celles de région on va donc remplir income avec la catégorie la plus courante chaque région

most_common_income <- wbe %>%  #Stocke dans most_common_income le résultat de la transformation
  group_by(region) %>%  #Regroupe le dataset wbe par région (region), pour traiter les valeurs manquantes par zone.
  summarise(most_common_income = first(na.omit(income))) #- Crée un nouveau dataframe avec une colonne most_common_income contenant le premier income valide de chaque région.
wbe <- wbe %>%
  left_join(most_common_income, by = "region") %>%
           mutate(income = if_else(is.na(income),most_common_income, income)) %>%
             select(-most_common_income)
sum(is.na(wbe$income))

# gdp_per_capita_usd et gdp_total_usd 
"""
Ce sont des valeurs qu'on eut imputer par calcul mais les valeurs manquent dans presque les meme lignes donc on fera le calcul si ça se peut et le reste sera remplacé par la médiane en fonction des groupes similaires dans région et income


"""
wbe  %>%
  filter(is.na(gdp_per_capita_usd))
wbe  %>%
  filter(is.na(gdp_total_usd))

wbe <- wbe %>%
  mutate(gdp_total_usd = if_else(is.na(gdp_total_usd) , gdp_per_capita_usd*population,gdp_total_usd),
         gdp_per_capita_usd = if_else(is.na(gdp_per_capita_usd) , gdp_total_usd / population, gdp_per_capita_usd))

wbe <- wbe %>%
  group_by(region,income)  %>%  #- Regrouper les données par région et niveau de revenu
  mutate(gdp_per_capita_usd = if_else(is.na(gdp_per_capita_usd), median(gdp_per_capita_usd, na.rm = TRUE),gdp_per_capita_usd),
         gdp_total_usd = if_else(is.na(gdp_total_usd),median(gdp_total_usd, na.rm = TRUE), gdp_total_usd)) %>% #- Remplacer les valeurs manquantes (NA) par la médiane des valeurs dans chaque groupe 
  ungroup()
sum(is.na(wbe$gdp_per_capita_usd))
sum(is.na(wbe$gdp_total_usd))

# On constate qu'il nous reste toujours 8 valeurs manquantes des deux cotés pour la Corée et le Venzuela.Ces pays sont connus pour des lacunes de transparence dans leurs données économiques, ce qui explique pourquoi aucune estimation fiable n’est disponible.Nous allons tenter de remplir ces valeurs avec une approche basée sue la moyenne régionale hors pays concernés
wbe <- wbe %>%
  group_by(region) %>%
  mutate(gdp_per_capita_usd= if_else(is.na(gdp_per_capita_usd),mean(gdp_per_capita_usd[iso3c != "PRK" & iso3c != "VEN"],na.rm = TRUE),gdp_per_capita_usd),
         gdp_total_usd = if_else(is.na(gdp_total_usd),mean(gdp_total_usd[iso3c != "PRK" & iso3c != "VEN"], na.rm = TRUE), gdp_total_usd)) %>%
  ungroup()
sum(is.na(wbe$gdp_per_capita_usd))
sum(is.na(wbe$gdp_total_usd))
#Pour le reste des colonnes  nous allons faire une imputation par la médiane
wbe <- wbe %>%
  group_by(region, income) %>%
  mutate(
    unemployment_rate = if_else(is.na(unemployment_rate), median(unemployment_rate, na.rm = TRUE), unemployment_rate),
    inflation_rate = if_else(is.na(inflation_rate), median(inflation_rate, na.rm = TRUE), inflation_rate),
    health_expenditure_pct_gdp = if_else(is.na(health_expenditure_pct_gdp), median(health_expenditure_pct_gdp, na.rm = TRUE), health_expenditure_pct_gdp),
    health_expenditure_per_capita = if_else(is.na(health_expenditure_per_capita), median(health_expenditure_per_capita, na.rm = TRUE), health_expenditure_per_capita),
  ) %>%
  ungroup()
colSums(is.na(wbe))
wbe %>%
  filter(if_any(everything(),is.na))

