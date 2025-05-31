library(readr)      # pour lire les fichiers CSV
library(dplyr)      # pour manipuler les données
library(ggplot2)    # pour les visualisations

data <- read.csv("../data/processed/merged_data.csv") 

View(data)
glimpse(data)#afficher la structure du dataset
summary(data)#resumé statistique par colonne

#identification des variables importantes
#(gdp_per_capita_usd) qui est le PIB par habitant
#(peoplefullyvaccinatedperhundred) qui est le taux de vaccination 
#et income pour différencier les pays riche des pays pauvres
#Vérification de valeurs manquantes
sum(is.na(data$gdp_per_capita_usd))
sum(is.na(data$peoplevaccinatedperhundred))

#Examination de la distribution du PIB par habitant
summary(data$gdp_per_capita_usd)
# Histogramme
ggplot(data, aes(x =gdp_per_capita_usd )) +
  geom_histogram(bins = 40, fill = "pink", color = "black") +
  labs(title = "Histogramme du PIB par habitant", x = "PIB/habitant", y = "Fréquence(%)")

#Examination du taux de vacination 
summary(data$peoplefullyvaccinatedperhundred)
# Histogramme
ggplot(data, aes(x =peoplefullyvaccinatedperhundred)) +
  geom_histogram(bins = 40, fill = "pink", color = "black") +
  labs(title = "Histogramme des taux de vaccination", x = "Taux de vaccination (%)")

#Catégorisation des pays selon le PIB
data <- data %>%
  mutate(
    pib_categorie = case_when(
      gdp_per_capita_usd < 1086 ~ "Low_income",
      gdp_per_capita_usd < 4255 ~ "Lower_middle_income",
      gdp_per_capita_usd < 13205 ~ "Upper middle income",
      TRUE ~ "High_income"
    )
  )
data %>%
  group_by(pib_categorie) %>%
  summarise(
    nmb_pays = n(),
    pib_moyen = mean(gdp_per_capita_usd, na.rm = TRUE),
    taux_vacc_moyen = mean(peoplevaccinatedperhundred, na.rm = TRUE),
    taux_vacc_median = median(peoplevaccinatedperhundred, na.rm = TRUE),
    .groups = 'drop'
  )

# Tableau croisé
table(data$pib_categorie)

data %>%
  group_by(pib_categorie) %>%
  summarise(taux_moyen = mean(peoplevaccinatedperhundred, na.rm = TRUE)) %>%
  ggplot(aes(x = pib_categorie, y = taux_moyen, fill = pib_categorie)) +
  geom_col() +
  labs(title = "Taux de vaccination moyen par catégorie de PIB")



