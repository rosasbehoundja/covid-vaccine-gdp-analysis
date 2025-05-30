# Script Complet de Nettoyage des Données de Vaccination
# Auteur: Analyste de Données
# Objectif: Charger, explorer et nettoyer les données CSV pour l'analyse vaccinale
# Dernière mise à jour: 2025

# =============================================================================
# GESTION DES PACKAGES POUR L'ANALYSE VACCINALE
# =============================================================================

# Packages essentiels pour le traitement des données épidémiologiques
packages_requis <- c("dplyr",      # Manipulation des données de vaccination par pays/région
                     "readr",       # Lecture rapide des fichiers CSV d'organismes de santé
                     "ggplot2",     # Visualisation des tendances vaccinales
                     "stringr",     # Nettoyage des noms de pays et codes ISO
                     "lubridate")   # Gestion des dates de campagnes vaccinales

# Packages optionnels pour analyses avancées des données sanitaires
packages_optionnels <- c("VIM",        # Visualisation des données manquantes dans les rapports vaccinaux
                         "corrplot")    # Analyse de corrélation entre indicateurs vaccinaux

# Fonction pour charger automatiquement les packages d'analyse sanitaire
charger_package <- function(pkg) {
  # Vérifier si le package est déjà installé et chargé
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    # Installation depuis CRAN si non disponible
    cat("Installation du package:", pkg, "\n")
    install.packages(pkg)
    # Chargement après installation
    library(pkg, character.only = TRUE)
  }
}

# Application du chargement à tous les packages requis pour l'analyse vaccinale
invisible(lapply(packages_requis, charger_package))

# Vérification de la disponibilité des packages optionnels (pas d'installation forcée)
vim_disponible <- require("VIM", quietly = TRUE)
corrplot_disponible <- require("corrplot", quietly = TRUE)

# Information sur l'état des packages optionnels pour l'analyse
if (!vim_disponible) cat("Note: Package VIM non disponible - visualisations des données manquantes ignorées\n")
if (!corrplot_disponible) cat("Note: Package corrplot non disponible - graphiques de corrélation ignorés\n")

# =============================================================================
# 1. CHARGEMENT DES DONNÉES VACCINALES ET CONFIGURATION INITIALE
# =============================================================================

# Chargement du jeu de données de vaccination depuis le fichier CSV
# Utilisation de read_csv pour un meilleur parsing des données épidémiologiques
df <- read_csv("data/raw/vaccination.csv")

# Suppression de la première colonne qui est un index auto-généré
# Cette colonne n'apporte aucune valeur pour l'analyse vaccinale
df <- df %>% select(-...1)

cat("✓ Données de vaccination chargées avec succès\n")

# =============================================================================
# 2. EXPLORATION INITIALE ET PROFILAGE DES DONNÉES VACCINALES
# =============================================================================

# Affichage des informations de base sur le jeu de données vaccinales
cat("Dimensions du dataset vaccinal:", dim(df), "\n")  # Affiche [lignes, colonnes]

# Affichage de tous les noms de colonnes pour comprendre les indicateurs vaccinaux
cat("Noms des colonnes (indicateurs vaccinaux):\n")
print(colnames(df))

# Affichage des types de données pour comprendre la structure des indicateurs
cat("Types de données des indicateurs:\n")
print(sapply(df, class))

# Aperçu des 6 premières entrées pour comprendre le format des données vaccinales
cat("6 premières entrées vaccinales:\n")
print(head(df))

# Aperçu des 6 dernières entrées pour vérifier la cohérence des données
cat("6 dernières entrées vaccinales:\n")
print(tail(df))

# Génération de statistiques descriptives complètes pour tous les indicateurs vaccinaux
cat("\nStatistiques descriptives des indicateurs vaccinaux:\n")
print(summary(df))

# =============================================================================
# 3. ANALYSE APPROFONDIE DES DONNÉES MANQUANTES DANS LES RAPPORTS VACCINAUX
# =============================================================================

# Calcul du nombre absolu de valeurs manquantes par indicateur vaccinal
compteurs_manquants <- df %>% 
  summarise_all(~sum(is.na(.))) %>%  # Comptage des valeurs NA par indicateur
  pivot_longer(everything(),          # Conversion en format long pour l'analyse
               names_to = "indicateur",
               values_to = "nombre_manquant") %>%
  arrange(desc(nombre_manquant))      # Tri par nombre décroissant de valeurs manquantes

cat("Comptage des données manquantes par indicateur vaccinal:\n")
print(compteurs_manquants)

# Calcul du pourcentage de valeurs manquantes par indicateur (plus interprétable)
pourcentages_manquants <- df %>% 
  summarise_all(~round(mean(is.na(.)) * 100, 2)) %>%  # Conversion en pourcentage
  pivot_longer(everything(), 
               names_to = "indicateur", 
               values_to = "pourcentage_manquant") %>%
  arrange(desc(pourcentage_manquant))   # Tri par pourcentage décroissant

cat("Pourcentages de données manquantes par indicateur vaccinal:\n")
print(pourcentages_manquants)

# =============================================================================
# 4. DÉTECTION ET SUPPRESSION DES DOUBLONS DANS LES RAPPORTS VACCINAUX
# =============================================================================

# Comptage des doublons avant suppression pour le rapport de nettoyage
lignes_initiales <- nrow(df)

# Suppression des lignes exactement identiques (garde la première occurrence)
# distinct() supprime les lignes où TOUS les indicateurs ont des valeurs identiques
df <- df %>% distinct()

# Rapport sur la suppression des doublons dans les données vaccinales
lignes_finales <- nrow(df)
doublons_supprimes <- lignes_initiales - lignes_finales
cat("Supprimé", doublons_supprimes, "entrées vaccinales dupliquées\n")

# =============================================================================
# 5. CONVERSIONS DE TYPES ET TRAITEMENT TEMPOREL DES CAMPAGNES VACCINALES
# =============================================================================

# Conversion de la colonne date des campagnes du format caractère vers le format Date
# MM/DD/YYYY est le format de date dans les données sources des campagnes
df$date <- as.Date(df$date, format = "%m/%d/%Y")

# Extraction uniquement de l'année pour l'analyse annuelle des campagnes vaccinales
# Cela agrège les données au niveau annuel plutôt que quotidien
df$date <- format(df$date, "%Y")

# Validation des codes ISO des pays par longueur (doit être 2-3 caractères)
# Suppression des entrées invalides qui ne correspondent pas aux codes ISO standard
pays_initiaux <- nrow(df)
df <- df[nchar(df$iso_code) <= 3, ]
iso_invalides_supprimes <- pays_initiaux - nrow(df)
if (iso_invalides_supprimes > 0) {
  cat("Supprimé", iso_invalides_supprimes, "entrées avec codes ISO de pays invalides\n")
}

# Agrégation des données au niveau pays-année en moyennant les indicateurs numériques
# Cela gère les cas où plusieurs enregistrements existent pour le même pays-année
df <- df %>%
  group_by(iso_code, date) %>%
  summarise(across(where(is.numeric),           # Application à tous les indicateurs numériques
                   ~mean(.x, na.rm = TRUE)),    # Calcul de la moyenne, ignorant les NAs
            .groups = "drop")                   # Suppression du groupement après opération

cat("✓ Données vaccinales agrégées au niveau pays-année\n")

# =============================================================================
# 6. DÉTECTION ET ANALYSE DES VALEURS ABERRANTES DANS LES INDICATEURS VACCINAUX
# =============================================================================

# Définition d'une fonction pour détecter les valeurs aberrantes avec la méthode IQR
# Cette méthode robuste fonctionne bien pour les distributions non-normales d'indicateurs vaccinaux
detecter_aberrantes <- function(x) {
  # Calcul des premier et troisième quartiles des indicateurs
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  # Calcul de l'écart interquartile pour les données vaccinales
  IQR <- Q3 - Q1
  
  # Définition des valeurs aberrantes comme dépassant 1.5 * IQR des quartiles
  # C'est la définition standard des valeurs aberrantes en "boîte à moustaches"
  drapeaux_aberrants <- x < Q1 - 1.5 * IQR | x > Q3 + 1.5 * IQR
  
  return(drapeaux_aberrants)
}

# Application de la détection d'aberrantes à tous les indicateurs numériques vaccinaux
colonnes_numeriques <- names(df)[sapply(df, is.numeric)]

cat("Analyse des valeurs aberrantes par indicateur vaccinal:\n")
for (col in colonnes_numeriques) {
  nombre_aberrantes <- sum(detecter_aberrantes(df[[col]]), na.rm = TRUE)
  pourcentage_aberrantes <- round((nombre_aberrantes / sum(!is.na(df[[col]]))) * 100, 2)
  cat("  ", col, ":", nombre_aberrantes, "valeurs aberrantes (", pourcentage_aberrantes, "%)\n")
}

# Note: Ce script détecte mais ne supprime pas les valeurs aberrantes
# L'expertise du domaine vaccinal devrait guider les décisions de traitement des aberrantes

# =============================================================================
# 7. NETTOYAGE ET STANDARDISATION DES DONNÉES TEXTUELLES (NOMS DE PAYS, ETC.)
# =============================================================================

# Identification de toutes les colonnes caractères/texte pour le nettoyage
colonnes_caracteres <- names(df)[sapply(df, is.character)]

# Définition d'une fonction de nettoyage complète pour les données textuelles
nettoyer_texte <- function(x) {
  x %>% 
    str_trim() %>%                          # Suppression des espaces de début/fin
    str_to_lower() %>%                      # Conversion en minuscules pour cohérence
    str_replace_all("[[:punct:]]", "") %>%  # Suppression de tous les signes de ponctuation
    str_replace_all("\\s+", " ")            # Remplacement des espaces multiples par un seul
}

# Application du nettoyage textuel à toutes les colonnes caractères
cat("Nettoyage du texte dans les colonnes:", paste(colonnes_caracteres, collapse = ", "), "\n")
for (col in colonnes_caracteres) {
  df[[col]] <- nettoyer_texte(df[[col]])
}

# Suppression des lignes complètement vides ou ne contenant que des valeurs manquantes
# any_vars() vérifie si TOUTE variable remplit la condition
df <- df %>% 
  filter_all(any_vars(!is.na(.) & . != ""))

cat("✓ Données textuelles standardisées et lignes vides supprimées\n")

# =============================================================================
# 8. STRATÉGIE D'IMPUTATION DES VALEURS MANQUANTES POUR LES INDICATEURS VACCINAUX
# =============================================================================

# Création d'une version imputée du jeu de données avec des méthodes appropriées au domaine
df_impute <- df %>%
  # Pour les indicateurs numériques: utilisation de l'imputation par médiane (robuste aux aberrantes)
  mutate_if(is.numeric, ~ifelse(is.na(.), 
                                median(., na.rm = TRUE),  # Remplacement NA par médiane
                                .)) %>%
  # Pour les colonnes caractères: utilisation de la catégorie explicite "Inconnu"
  mutate_if(is.character, ~ifelse(is.na(.) | . == "", 
                                  "inconnu",              # Remplacement NA/vide par "inconnu"
                                  .))

# Rapport sur les résultats de l'imputation des données vaccinales
cat("Résumé de l'imputation des indicateurs vaccinaux:\n")
cat("  Indicateurs numériques: Valeurs manquantes remplacées par la médiane de l'indicateur\n")
cat("  Colonnes textuelles: Valeurs manquantes remplacées par 'inconnu'\n")

# Vérification qu'aucune valeur manquante ne subsiste après imputation
nas_restantes <- sum(is.na(df_impute))
cat("  Valeurs manquantes restantes:", nas_restantes, "\n")

# =============================================================================
# 9. EXPORT ET PERSISTANCE DES DONNÉES VACCINALES NETTOYÉES
# =============================================================================

# Sauvegarde du jeu de données nettoyé en CSV pour analyse ultérieure
# Utilisation de write_csv pour un formatage cohérent et encodage UTF-8
write_csv(df_impute, "donnees_vaccination_nettoyees.csv")
cat("✓ Données vaccinales nettoyées exportées vers 'donnees_vaccination_nettoyees.csv'\n")

# =============================================================================
# 10. SYSTÈME DE RAPPORT DE QUALITÉ DES DONNÉES VACCINALES
# =============================================================================

# Fonction pour générer un rapport complet de qualité des données vaccinales
creer_rapport_qualite_donnees <- function(donnees) {
  # Retourne une liste structurée avec les métriques clés de qualité
  list(
    # Informations de structure de base du dataset vaccinal
    dimensions = dim(donnees),
    
    # Résumé des types de données pour chaque indicateur vaccinal
    types_colonnes = sapply(donnees, class),
    
    # Résumé des données manquantes (devrait être 0 après imputation)
    resume_manquantes = donnees %>% 
      summarise_all(~sum(is.na(.))) %>%
      pivot_longer(everything(), 
                   names_to = "indicateur", 
                   values_to = "nombre_manquant"),
    
    # Résumé statistique pour les indicateurs numériques vaccinaux
    resume_numerique = donnees %>% 
      select_if(is.numeric) %>% 
      summary(),
    
    # Comptage des lignes dupliquées restantes
    nombre_doublons = sum(duplicated(donnees))
  )
}

# Génération du rapport de qualité pour les données vaccinales nettoyées
rapport_qualite_donnees <- creer_rapport_qualite_donnees(df_impute)
cat("✓ Rapport de qualité des données vaccinales généré\n")

# =============================================================================
# 11. STANDARDISATION DES NOMS DE COLONNES POUR L'ANALYSE VACCINALE
# =============================================================================

# Fonction pour créer des noms de colonnes compatibles avec les bases de données
standardiser_noms_colonnes <- function(donnees) {
  # Application de conventions de nommage systématiques
  names(donnees) <- names(donnees) %>%
    str_to_lower() %>%                       # Conversion en minuscules
    str_replace_all("[[:space:]]+", "_") %>% # Remplacement des espaces par des underscores
    str_replace_all("[[:punct:]]", "") %>%   # Suppression des caractères de ponctuation
    str_replace_all("_{2,}", "_") %>%        # Réduction des underscores multiples
    str_remove("_$")                         # Suppression des underscores finaux
  
  return(donnees)
}

# Application du nommage standardisé au jeu de données final
df_final <- df_impute %>% standardiser_noms_colonnes()

# Affichage des noms de colonnes finaux pour vérification
cat("Noms d'indicateurs vaccinaux standardisés:\n")
print(colnames(df_final))

# Export de la version finale avec noms standardisés
write_csv(df_final, "donnees_vaccination_nettoyees_final.csv")

# =============================================================================
# RÉSUMÉ DE COMPLETION DU NETTOYAGE DES DONNÉES VACCINALES
# =============================================================================

cat("\n" %+% strrep("=", 70) %+% "\n")
cat("PROCESSUS DE NETTOYAGE DES DONNÉES VACCINALES COMPLÉTÉ AVEC SUCCÈS!\n")
cat(strrep("=", 70) %+% "\n")

# Statistiques finales du jeu de données vaccinal
cat("Résumé du jeu de données vaccinal final:\n")
cat("  Entrées (pays-années):", nrow(df_final), "\n")
cat("  Indicateurs vaccinaux:", ncol(df_final), "\n")
cat("  Valeurs manquantes:", sum(is.na(df_final)), "\n")
cat("  Lignes dupliquées:", sum(duplicated(df_final)), "\n")

# Liste des fichiers de sortie créés
cat("\nFichiers de données vaccinales créés:\n")
cat("  - donnees_vaccination_nettoyees.csv (avec noms de colonnes originaux)\n")
cat("  - donnees_vaccination_nettoyees_final.csv (avec noms d'indicateurs standardisés)\n")

cat("\nDonnées vaccinales prêtes pour l'analyse épidémiologique! \n")
