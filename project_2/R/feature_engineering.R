#Convertissez les étiquettes de sentiment dans un format numérique adapté à DFA.
# on transforme le texte en nombres pour l'AFD avec TF-IDF

# Ingénierie des caractéristiques (feature engineering):
#Extraire les caractéristiques à partir des données textuelles, éventuellement à l’aide de TF-IDF
#d’intégrations de mots ou de scores de sentiment.

library(tm)

print("on crée la matrice TF-IDF")

# on crée un ID pour chaque tweet
train_data_grouped <- train_data %>%
  mutate(doc_id = row_number()) %>%
  dplyr::select(doc_id, Sentiment, TweetText_clean)

# on cree une collection des tweets dans un format que la librairie tm peut traiter
collection_train <- Corpus(VectorSource(train_data_grouped$TweetText_clean))

# on crée la matrice TF-IDF

# la TF-IDF calcule :
#TF (Term Frequency) ==> Combien de fois le mot apparaît dans un tweet ?
#IDF (Inverse Document Frequency) ==> Le mot est-il rare ou commun dans tous les tweets ?

# plus le score TF-IDF est élevé, plus le mot est important

# on limite aux 500 termes les plus importants pour l'AFD
dtm_train <- DocumentTermMatrix(collection_train,  # documentTermMatrix = tableau géant
                                control = list(
                                  weighting = weightTfIdf,
                                  bounds = list(global = c(10, Inf)),  # Mots apparaissant au moins 10 fois
                                  removeNumbers = TRUE,
                                  removePunctuation = TRUE,
                                  stopwords = TRUE
                                ))

print(paste("dim de la matrice TF-IDF:", dim(dtm_train)[1], "documents x", dim(dtm_train)[2], "termes"))

# on supprime les termes inutiles pour réduire la taille (< 0.5% des documents)
dtm_train <- removeSparseTerms(dtm_train, 0.995)
print(paste("Après réduction des mots inutiles:", dim(dtm_train)[1], "documents x", dim(dtm_train)[2], "termes"))

# on converti en matrice ==> pouor ensuite le dataframe
tfidf_matrix <- as.matrix(dtm_train) # lignes = tweets, colonnes = mots importants
print(paste("matrice utilisable:", nrow(tfidf_matrix), "x", ncol(tfidf_matrix)))

# on ajoute 1 colonnes pour les labels de sentiment
sentiment_labels <- train_data_grouped$Sentiment

# on créé avec notre matrice un dataframe pour l'AFD
dataframe_tfidf <- data.frame(tfidf_matrix, Sentiment = sentiment_labels)

# on va vérifier qu'il n'y a pas de valeurs manquantes
print(paste("nb valeurs manquantes:", sum(is.na(dataframe_tfidf))))

# on va supprimer les lignes avec uniquement des zéros (tweets vides après nettoyage)
row_sums <- rowSums(tfidf_matrix)
non_empty_rows <- row_sums > 0
dataframe_tfidf <- dataframe_tfidf[non_empty_rows, ]

# on regarde la distribution des sentiments après  le nettoyage
table(dataframe_tfidf$Sentiment)

# on visualise la matrice TF-IDF
print("\naperçu de la matrice TF-IDF")
print("les termes avec les scores TF-IDF les plus élevés:")
term_scores <- colSums(tfidf_matrix[non_empty_rows, ])
higher_terms <- sort(term_scores, decreasing = TRUE)[1:20]
print(higher_terms)

# on visualise les 20 termes avec les scores TF-IDF les plus élevés
terms_df <- data.frame(
  term = names(higher_terms),
  score = as.numeric(higher_terms)
)

p3 <- ggplot(terms_df, aes(x = reorder(term, score), y = score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "les 20 termes avec les scores TF-IDF les plus élevés",
       x = "Terme",
       y = "Score TF-IDF cumulé")

print(p3)
