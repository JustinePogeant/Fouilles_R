#Analyse factorielle discriminante :
# Utilisez les fonctions du package MASS dans R pour effectuer une analyse discriminante
# linéaire (LDA) afin de projeter les données dans un espace de dimension inférieure qui
# maximise la séparation des classes de sentiment.


library(MASS)
print("analyse Factorielle Discriminante (LDA)")

# on utilise le dataframe créé dans le feature engineering : dataframe_tfidf

# on sépare les features (X) et les labels (y)
X <- dataframe_tfidf[, -ncol(dataframe_tfidf)]  # on prends toutes les colonnes sauf Sentiment
y <- as.factor(dataframe_tfidf$Sentiment)     # et là on prends la variable cible Sentiment

#print(paste("dim des features", nrow(X), "x", ncol(X)))
#print("distribution des classes")
#print(table(y))

# on divise train/test (80/20) pour évaluer la performance
set.seed(123)  # Pour reproductibilité des résultats
train_indices <- sample(1:nrow(X), size = 0.8 * nrow(X))

X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

#print(paste("taille du training set", nrow(X_train)))
#print(paste("taille du test set", nrow(X_test)))


# on applique l'afd avec paquage MASS
# on créé le dataframe pour la LDA
lda_train_data <- data.frame(X_train, Sentiment = y_train)

# on applique l'Analyse Discriminante Linéaire lda
# L'AFD créera au maximum k-1 axes discriminants (k = nombre de classes)
# Ici : 4 sentiments → maximum 3 axes discriminants (LD1, LD2, LD3)
lda_model <- lda(Sentiment ~ ., data = lda_train_data)
print(lda_model)
#

# on calcule la proportion de variance expliquée par chaque axe discriminant.
proportions <- lda_model$svd^2 / sum(lda_model$svd^2)
names(proportions) <- paste0("LD", 1:length(proportions))

# on calcule la variance cumulée
var_cumulee <- cumsum(proportions)

# on applique le modèle LDA aux données d'entraînement
lda_pred_train <- predict(lda_model, newdata = X_train)

# on applique le modèle LDA aux  données de test
lda_pred_test <- predict(lda_model, newdata = X_test)

# Créer les dataframes avec les coordonnées projetées
lda_train_proj <- data.frame(
  LD1 = lda_pred_train$x[, 1],
  LD2 = lda_pred_train$x[, 2],
  LD3 = if(ncol(lda_pred_train$x) >= 3) lda_pred_train$x[, 3] else NA,
  Sentiment_Real = y_train,
  Sentiment_Pred = lda_pred_train$class,
  Type = "Train"
)

lda_test_proj <- data.frame(
  LD1 = lda_pred_test$x[, 1],
  LD2 = lda_pred_test$x[, 2],
  LD3 = if(ncol(lda_pred_test$x) >= 3) lda_pred_test$x[, 3] else NA,
  Sentiment_Real = y_test,
  Sentiment_Pred = lda_pred_test$class,
  Type = "Test"
)

print("Aperçu du dataframe (Training):")
print(head(lda_train_proj))

print("\nAperçu du dataframe (Test):")
print(head(lda_test_proj))

# on va juste sauvegarder les projections pour la visualisation
lda_all_proj <- rbind(lda_train_proj, lda_test_proj)
