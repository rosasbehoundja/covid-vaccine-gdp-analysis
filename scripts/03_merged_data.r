library(readr)
library(dplyr)
library(lubridate)

wbe_file_path <- "../data/cleaned/wbe_cleaned.csv"
vaccination_file_path <- "../data/cleaned/vaccination-cleaned.csv"

# Charger les datasets
wbe_data <- read_csv(wbe_file_path)
vaccination_data <- read_csv(vaccination_file_path)

# Afficher les premières lignes
print(head(wbe_data, 3))

print(head(vaccination_data, 3))
# --- Préparation des données pour la fusion ---

if (colnames(wbe_data)[1] == "...1" || grepl("^X[0-9]*$", colnames(wbe_data)[1]) || colnames(wbe_data)[1] == "") {
  wbe_data <- wbe_data %>% select(-1)
  print("Retrait de la colonne index de wbe_data.")
}

# --- Fusion des datasets ---
# Nous utilisons inner_join pour ne conserver que les lignes où 'isocode' et 'date'
# correspondent dans les deux datasets.

merged_data <- inner_join(wbe_data, vaccination_data, by = c("isocode", "date"))

# --- Vérification du résultat ---
print("Dimensions du nouveau dataset fusionné:")
print(dim(merged_data))

print("Premières lignes du nouveau dataset fusionné:")
print(head(merged_data))

print("Structure du nouveau dataset fusionné:")
str(merged_data)

# Compter le nombre de pays uniques et d'années dans le jeu fusionné
print(paste("Nombre de codes ISO uniques dans merged_data:", n_distinct(merged_data$isocode)))
print(paste("Plage d'années dans merged_data:", min(merged_data$date), "-", max(merged_data$date)))

write.csv(merged_data, "../data/processed/merged_data.csv")