packages_requis <- c("dplyr",
                     "readr",
                     "ggplot2",
                     "stringr",
                     "lubridate")
packages_optionnels <- c("VIM",
                         "corrplot")

# Fonction pour charger les packages
charger_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installation du package:", pkg, "\\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Chargement des packages requis
invisible(lapply(packages_requis, charger_package))

# Vérification des packages optionnels
vim_disponible <- require("VIM", quietly = TRUE)
corrplot_disponible <- require("corrplot", quietly = TRUE)

if (!vim_disponible) cat("Note: Package VIM non disponible\\n")
if (!corrplot_disponible) cat("Note: Package corrplot non disponible\\n")

# =============================================================================
# 1. CHARGEMENT DES DONNÉES
# =============================================================================

# Chargement du jeu de données
df <- read_csv("../data/raw/vaccination.csv")

# Suppression de la première colonne (index)
df <- df %>% select(-...1)

cat("✓ Données de vaccination chargées\\n")

# =============================================================================
# 2. EXPLORATION INITIALE
# =============================================================================

cat("Dimensions du dataset:", dim(df), "\\n")
cat("Noms des colonnes:\\n")
print(colnames(df))
cat("Types de données:\\n")
print(sapply(df, class))
cat("6 premières entrées:\\n")
print(head(df))
cat("6 dernières entrées:\\n")
print(tail(df))
cat("\\nStatistiques descriptives:\\n")
print(summary(df))

# =============================================================================
# 3. ANALYSE DES DONNÉES MANQUANTES
# =============================================================================

# Nombre de valeurs manquantes par indicateur
compteurs_manquants <- df %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "indicateur",
               values_to = "nombre_manquant") %>%
  arrange(desc(nombre_manquant))

cat("Comptage des données manquantes par indicateur:\\n")
print(compteurs_manquants)

# Pourcentage de valeurs manquantes par indicateur
pourcentages_manquants <- df %>%
  summarise_all(~round(mean(is.na(.)) * 100, 2)) %>%
  pivot_longer(everything(),
               names_to = "indicateur",
               values_to = "pourcentage_manquant") %>%
  arrange(desc(pourcentage_manquant))

cat("Pourcentages de données manquantes par indicateur:\\n")
print(pourcentages_manquants)

# =============================================================================
# 4. DÉTECTION ET SUPPRESSION DES DOUBLONS
# =============================================================================

lignes_initiales <- nrow(df)
df <- df %>% distinct() # Suppression des lignes identiques
lignes_finales <- nrow(df)
doublons_supprimes <- lignes_initiales - lignes_finales
cat("Supprimé", doublons_supprimes, "doublons\\n")

# =============================================================================
# 5. CONVERSIONS DE TYPES ET TRAITEMENT TEMPOREL
# =============================================================================

# Conversion de la colonne date en format Date (format source: MM/DD/YYYY)
df$date <- as.Date(df$date, format = "%m/%d/%Y")

# Extraction de l'année
df$date <- format(df$date, "%Y")

# Validation des codes ISO (longueur <= 3)
pays_initiaux <- nrow(df)
df <- df[nchar(df$iso_code) <= 3, ]
iso_invalides_supprimes <- pays_initiaux - nrow(df)
if (iso_invalides_supprimes > 0) {
  cat("Supprimé", iso_invalides_supprimes, "entrées avec codes ISO invalides\\n")
}

# Agrégation des données par pays-année (moyenne des indicateurs numériques)
df <- df %>%
  group_by(iso_code, date) %>%
  summarise(across(where(is.numeric),
                   ~mean(.x, na.rm = TRUE)),
            .groups = "drop")

cat("✓ Données agrégées au niveau pays-année\\n")

# =============================================================================
# 6. DÉTECTION DES VALEURS ABERRANTES (MÉTHODE IQR)
# =============================================================================

detecter_aberrantes <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  drapeaux_aberrants <- x < Q1 - 1.5 * IQR | x > Q3 + 1.5 * IQR
  return(drapeaux_aberrants)
}

colonnes_numeriques <- names(df)[sapply(df, is.numeric)]

cat("Analyse des valeurs aberrantes:\\n")
for (col in colonnes_numeriques) {
  nombre_aberrantes <- sum(detecter_aberrantes(df[[col]]), na.rm = TRUE)
  pourcentage_aberrantes <- round((nombre_aberrantes / sum(!is.na(df[[col]]))) * 100, 2)
  cat("  ", col, ":", nombre_aberrantes, "valeurs aberrantes (", pourcentage_aberrantes, "%)\\n")
}
# Note: Les valeurs aberrantes sont détectées mais non supprimées ici.

# =============================================================================
# 7. NETTOYAGE DES DONNÉES TEXTUELLES
# =============================================================================

colonnes_caracteres <- names(df)[sapply(df, is.character)]

nettoyer_texte <- function(x) {
  x %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("\\\\s+", " ")
}

cat("Nettoyage du texte dans les colonnes:", paste(colonnes_caracteres, collapse = ", "), "\\n")
for (col in colonnes_caracteres) {
  df[[col]] <- nettoyer_texte(df[[col]])
}

# Suppression des lignes entièrement vides
df <- df %>%
  filter_all(any_vars(!is.na(.) & . != ""))

cat("✓ Données textuelles standardisées et lignes vides supprimées\\n")

# =============================================================================
# 8. IMPUTATION DES VALEURS MANQUANTES
# =============================================================================

df_impute <- df %>%
  # Numérique: imputation par médiane
  mutate_if(is.numeric, ~ifelse(is.na(.),
                                median(., na.rm = TRUE),
                                .)) %>%
  # Caractères: imputation par "inconnu"
  mutate_if(is.character, ~ifelse(is.na(.) | . == "",
                                  "inconnu",
                                  .))

cat("Résumé de l'imputation:\\n")
cat("  Indicateurs numériques: NA remplacés par la médiane\\n")
cat("  Colonnes textuelles: NA/vide remplacés par 'inconnu'\\n")
nas_restantes <- sum(is.na(df_impute))
cat("  Valeurs manquantes restantes:", nas_restantes, "\\n")

# =============================================================================
# 9. EXPORT DES DONNÉES NETTOYÉES
# =============================================================================

write_csv(df_impute, "donnees_vaccination_nettoyees.csv")
cat("✓ Données nettoyées exportées vers 'donnees_vaccination_nettoyees.csv'\\n")

# =============================================================================
# 10. RAPPORT DE QUALITÉ DES DONNÉES
# =============================================================================

creer_rapport_qualite_donnees <- function(donnees) {
  list(
    dimensions = dim(donnees),
    types_colonnes = sapply(donnees, class),
    resume_manquantes = donnees %>%
      summarise_all(~sum(is.na(.))) %>%
      pivot_longer(everything(),
                   names_to = "indicateur",
                   values_to = "nombre_manquant"),
    resume_numerique = donnees %>%
      select_if(is.numeric) %>%
      summary(),
    nombre_doublons = sum(duplicated(donnees))
  )
}

rapport_qualite_donnees <- creer_rapport_qualite_donnees(df_impute)
cat("✓ Rapport de qualité des données généré\\n")

# =============================================================================
# 11. STANDARDISATION DES NOMS DE COLONNES
# =============================================================================

standardiser_noms_colonnes <- function(donnees) {
  names(donnees) <- names(donnees) %>%
    str_to_lower() %>%
    str_replace_all("[[:space:]]+", "_") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove("_$")
  return(donnees)
}

df_final <- df_impute %>% standardiser_noms_colonnes()

cat("Noms de colonnes standardisés:\\n")
print(colnames(df_final))

write_csv(df_final, "../data/cleaned/vaccination-cleaned.csv")

# =============================================================================
# RÉSUMÉ
# =============================================================================

cat("\\n" %+% strrep("=", 50) %+% "\\n")
cat("NETTOYAGE DES DONNÉES TERMINÉ AVEC SUCCÈS!\\n")
cat(strrep("=", 50) %+% "\\n")

cat("Résumé du jeu de données final:\\n")
cat("  Entrées (pays-années):", nrow(df_final), "\\n")
cat("  Indicateurs:", ncol(df_final), "\\n")
cat("  Valeurs manquantes:", sum(is.na(df_final)), "\\n")
cat("  Doublons:", sum(duplicated(df_final)), "\\n")

cat("\\nFichiers créés:\\n")
cat("  - donnees_vaccination_nettoyees.csv\\n")
cat("  - donnees_vaccination_nettoyees_final.csv\\n")

cat("\\nDonnées prêtes pour l'analyse! \\n")
