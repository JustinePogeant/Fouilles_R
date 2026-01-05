# 1. Prétraitement des données :
# Nettoyage du texte : Supprimez les mots d'arrêt (stopwords), la ponctuation et
#effectuez un stemming ou une lemmatisation.
#Vectorisation : Convertir les données textuelles sous forme numérique à l'aide de TF-
#IDF ou le plongement lexical « plongement de mots ou word embeddings » tels que
#Word2Vec.


#on charge les données

donnees <- read.csv("/Users/justinepogeant/Documents/E4/COURS_S1/P2/Fouilles de donnée R/Fouilles_R/project_3/data/french_thesis_20231021_metadata.csv", 
                     stringsAsFactors = FALSE,
                     encoding = "UTF-8")


#pour faire sur moins de donnée sinon ca prend enormément de temps
set.seed(123)

donnees <- donnees[sample(1:nrow(donnees), min(3000, nrow(donnees))), ]                     

# on regarde les noms de colonnes pour savoir si c'est bon et pour connaître le format
cat(" Colonnes du dataset \n")
print(names(donnees))
cat("\n")
cat("\n")
# on regarde les données
cat("premières lignes du fichier\n")
print(head(donnees, 3))
cat("\n")

# on commence le prétraitement et nettoyage

donnees$Description[is.na(donnees$Description)] <- "" # on remplace les valeurs NA par des chaines vides

#on crée une nouvelle colonne qui concatène le titre et la description ==> plus d'info pour le classifieur : c'est mieux
donnees$texte_complet <- paste(donnees$Title, donnees$Description, sep = " ")

# on a combien de données :
cat("Avant nettoyage :\n")
cat("  - Nombre total de thèses :", nrow(donnees), "\n")
cat("  - Nombre de domaines différents :", length(unique(donnees$Domain)), "\n\n")

# on supprime les lignes avec texte vide
donnees <- donnees[donnees$texte_complet != "" & donnees$texte_complet != " ", ]

# on supprime les lignes sans domaine ( besoin pour la classification)
donnees <- donnees[!is.na(donnees$Domain) & donnees$Domain != "", ]


# on a combien de données à nouveau:
cat("Avant nettoyage :\n")
cat("  - Nombre total de thèses :", nrow(donnees), "\n")
cat("  - Nombre de domaines différents :", length(unique(donnees$Domain)), "\n\n")


#on crée le corpus pour la matrice TF-IDF

cat("=== Création du corpus ===\n\n")

# le corpus = collection de documents textuels

corpus <- VCorpus(VectorSource(donnees$texte_complet))

cat("✓ Corpus créé avec", length(corpus), "documents\n\n")


#nettoyage


# on met tout en minuscules
corpus <- tm_map(corpus, content_transformer(tolower))

# on supprime les chiffres
corpus <- tm_map(corpus, removeNumbers)

# On supprime la ponctuation
corpus <- tm_map(corpus, removePunctuation)

# on enlève les espaces multiples
corpus <- tm_map(corpus, stripWhitespace)

# on supprimeles stopwords (mots vides)
corpus <- tm_map(corpus, removeWords, stopwords("fr"))

# on applique le stemming (on réduit les mots à leur racine : "apprentissage", "apprendre", "apprenti" → "appren" comme ca on regroupe les mots de même famille
corpus <- tm_map(corpus, stemDocument, language = "french")

# Regardons le même exemple APRÈS nettoyage
cat("exemple après nettoyage\n")
exemple_apres <- as.character(corpus[[100]])
cat(substr(exemple_apres, 1, 300), "...\n\n")



# on crée la matrice TF-IDF

# on crée la matrice dtm Document-Term
# les lignes ==> documents : les thèses
# les colonnes ==> mots
# les valeurs ==> fréquences

dtm <- DocumentTermMatrix(corpus)

# on supprime les termes trop rares parce qu'ils sont inutiles et n'offrent pas d'infos
# On garde seulement les mots présents dans au moins 2% des documents

dtm <- removeSparseTerms(dtm, 0.98)


# on calcule les poids TF-IDF
# on donne plus de poids aux mots importants

tfidf <- weightTfIdf(dtm)

# on convertie tfid en matrice classique pour la manipulation
matrice_tfidf <- as.matrix(tfidf)
# on supprime les colonnes avec que des zéros
cols_non_zero <- colSums(matrice_tfidf) > 0
matrice_tfidf <- matrice_tfidf[, cols_non_zero]


# on crée un nouveau dataframe avec la matrice TF-IDF pour la suite

dataframe <- as.data.frame(matrice_tfidf)


# on ajoute la variable cible ( le domaine) puisque c'est ce qu'on veut prédire
dataframe$domaine <- donnees$Domain[1:nrow(dataframe)]

# on convertie en catégorie pour la classification
dataframe$domaine <- as.factor(dataframe$domaine)

cat("on regarde notre dataset final avant la suite\n")
cat("Nb de thèses :", nrow(dataframe), "\n")
cat("Nb de features (mots) :", ncol(dataframe) - 1, "\n")
cat("Nb de classes (domaines) :", nlevels(dataframe$domaine), "\n\n")

distribution <- table(dataframe$domaine)
print(sort(distribution, decreasing = TRUE))
cat("\n")

#on enregistre tout

# on sauvegarde tout dans un fichier .RData (format R)
# on le telecharge comme ca :  load("donnees_preprocessed.RData")
save(dataframe, matrice_tfidf, donnees, corpus, dtm,
     file = "project_3/data/donnees_preprocessed.RData")

# Sauvegarder aussi en CSV (pour ouvrir dans Excel si besoin)
write.csv(dataframe, "project_3/data/donnees_traitees.csv", row.names = FALSE)
# Sauvegarder les données originales aussi
write.csv(donnees, "project_3/data/donnees_initiales.csv", row.names = FALSE)