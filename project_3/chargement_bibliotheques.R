#mini projet 3
# on installe les bibliotheques

packages <- c(
  "tm",           # textmining our le nettoyage de texte
  "SnowballC",    # stemming ==> pour reduire les mots à l'heure racine
  "tidyverse",    # manipulation de données
  "tidytext",     # traitement de texte
  "textclean",    # nettoyage de texte
  "Matrix"       # matrices creuses (sparse) pour notre matrice TF-IDF
)

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

invisible(sapply(packages, install_if_missing))