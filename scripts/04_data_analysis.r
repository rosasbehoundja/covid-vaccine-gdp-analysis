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
  select(isocode, income, gdp_per_capita_usd, peoplefullyvaccinatedperhundred) %>%
  filter(!is.na(gdp_per_capita_usd), 
         !is.na(peoplefullyvaccinatedperhundred),
         gdp_per_capita_usd > 0,
         peoplefullyvaccinatedperhundred > 0)

# 5. Analyse statistique de la relation
cor_test <- cor.test(data_viz$gdp_per_capita_usd, data_viz$peoplefullyvaccinatedperhundred)
print(cor_test)

modele <- lm(peoplefullyvaccinatedperhundred ~ gdp_per_capita_usd, data = data_viz)
print(summary(modele))

# 6. Création de la visualisation graphique
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

# Afficher le graphique dans la session R (si possible)
print(p)

# 7. Sauvegarde du graphique en PNG pour visualisation dans VS Code
ggsave("relation_pib_vaccination.png", plot = p, width = 9, height = 6)

cat("Graphique sauvegardé dans 'relation_pib_vaccination.png'\n")

# -----------------------------------------------------------
# Fin du script
# -----------------------------------------------------------
