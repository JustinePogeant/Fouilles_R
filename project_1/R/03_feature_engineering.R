dtm <- DocumentTermMatrix(
  corpus,
  control = list(weighting = weightTfIdf)
)

# Supprimer les termes trop rares
dtm <- removeSparseTerms(dtm, 0.99)

dtm_matrix <- as.matrix(dtm)
dtm_df <- as.data.frame(dtm_matrix)
dtm_df$emotion <- as.factor(data$emotion)
