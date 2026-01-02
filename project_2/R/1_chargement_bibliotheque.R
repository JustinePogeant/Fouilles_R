# Installation des packages (à exécuter une seule fois)
install.packages(c("tidyverse", "tm", "tidytext", "MASS", 
                   "ggplot2", "caret", "wordcloud", "cluster"))

# on charge les bibliothèques
library(tidyverse)  # Manipulation de données
library(tm)   # Text mining
library(tidytext)  # Text mining moderne
library(MASS)   # Pour LDA (AFD)
library(ggplot2)   # Visualisation
library(caret)   # Évaluation du modèle
library(wordcloud)  # Nuages de mots
library(cluster)    # Calcul de silhouetteP