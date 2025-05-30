# Data Analysis Script on the merged dataset
# -----------------------------------------------------------
# Analyse de la relation entre PIB par habitant et taux de vaccination COVID-19
# -----------------------------------------------------------
packages <- c("dplyr", "ggplot2", "readr", "scales")
installed <- packages %in% installed.packages()
if(any(!installed)) install.packages(packages[!installed])

# 1. Chargement des bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# 2. Importation du jeu de données
data <- read_csv("merged_data.csv", show_col_types = FALSE)

# 3. Préparation des données
data_latest <- data %>%
  group_by(isocode) %>%
  filter(date == max(date)) %>%
  ungroup()

# 4. Sélection des variables d'intérêt et nettoyage
data_viz <- data_latest %>%
  select(isocode, income, gdp_per_capita_usd, peoplefullyvaccinatedperhundred, peoplevaccinatedperhundred) %>%
  filter(!is.na(gdp_per_capita_usd), 
         !is.na(peoplefullyvaccinatedperhundred),
         gdp_per_capita_usd > 0,
         peoplefullyvaccinatedperhundred > 0)

# 5. CATÉGORISATION DES PAYS SELON LE PIB (ANALYSE AJOUTÉE)
data_viz <- data_viz %>%
  mutate(
    pib_categorie = case_when(
      gdp_per_capita_usd < 1086 ~ "Low_income",
      gdp_per_capita_usd < 4255 ~ "Lower_middle_income",
      gdp_per_capita_usd < 13205 ~ "Upper middle income",
      TRUE ~ "High_income"
    )
  )

# Analyse descriptive par catégorie
cat("=== ANALYSE PAR CATÉGORIE DE PIB ===\n")
resume_categories <- data_viz %>%
  group_by(pib_categorie) %>%
  summarise(
    nmb_pays = n(),
    pib_moyen = mean(gdp_per_capita_usd, na.rm = TRUE),
    taux_vacc_moyen = mean(peoplevaccinatedperhundred, na.rm = TRUE),
    taux_vacc_median = median(peoplevaccinatedperhundred, na.rm = TRUE),
    .groups = 'drop'
  )
print(resume_categories)

# Tableau croisé
cat("\n=== RÉPARTITION DES PAYS PAR CATÉGORIE ===\n")
print(table(data_viz$pib_categorie))

# Graphique en barres par catégorie
p_categories <- data_viz %>%
  group_by(pib_categorie) %>%
  summarise(taux_moyen = mean(peoplevaccinatedperhundred, na.rm = TRUE)) %>%
  ggplot(aes(x = pib_categorie, y = taux_moyen, fill = pib_categorie)) +
  geom_col() +
  labs(title = "Taux de vaccination moyen par catégorie de PIB",
       x = "Catégorie de PIB",
       y = "Taux de vaccination moyen (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_categories)
ggsave("taux_vaccination_par_categorie.png", plot = p_categories, width = 10, height = 6)

# 6. Analyse statistique de la relation
cat("\n=== ANALYSE STATISTIQUE ===\n")
cor_test <- cor.test(data_viz$gdp_per_capita_usd, data_viz$peoplefullyvaccinatedperhundred)
print(cor_test)

modele <- lm(peoplefullyvaccinatedperhundred ~ gdp_per_capita_usd, data = data_viz)
print(summary(modele))

# 7. Création de la visualisation graphique principale
p <- ggplot(data_viz, aes(x = gdp_per_capita_usd, y = peoplefullyvaccinatedperhundred, color = income)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Relation entre PIB par habitant et taux de vaccination COVID-19",
    subtitle = "Données par pays - dernière année disponible",
    x = "PIB par habitant (USD)",
    y = "Taux de vaccination complet (%)",
    color = "Niveau de revenu"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

print(p)

# 8. Sauvegarde des graphiques
ggsave("relation_pib_vaccination.png", plot = p, width = 9, height = 6)
cat("Graphiques sauvegardés :\n")
cat("- 'relation_pib_vaccination.png'\n")
cat("- 'taux_vaccination_par_categorie.png'\n")

# -----------------------------------------------------------
# Fin du script
# -----------------------------------------------------------
