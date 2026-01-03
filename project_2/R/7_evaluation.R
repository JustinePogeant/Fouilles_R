# 5. Évaluation du modèle :
#Si DFA est utilisé pour la classification, évaluez la précision (accuracy) du modèle.
#S’il est utilisé uniquement à des fins de visualisation, évaluez la qualité de l’agrégation 
#à l’aide de scores de silhouette (silhouette scores) ou d’une mesure similaire.



print("On évalue les performances de l'AFD")

print("On compare ce que l'AFD a prédit vs la réalité\n")

# On crée une matrice de confusion pour ensuite calculer l'accuracy
#on va pouvoir voir le nombre de tweets bien classés vs mal classés
mat_confusion <- confusionMatrix(
  data = lda_test_proj$Sentiment_Pred,      # Ce que l'AFD a prédit
  reference = lda_test_proj$Sentiment_Real  # La vraie réponse
)

print(mat_confusion)

# L'ACCURACY = pourcentage de tweets bien classés
accuracy <- mat_confusion$overall['Accuracy']
print(paste("Accuracy :", round(accuracy * 100, 2), "%"))


library(cluster) # pour la fonction silhouette
library(ggplot2) # pour visualiser

# on fait le score de silhouette pour évaluer la qualité de la séparation des clusters


# on prend un échantillon
set.seed(123)
sample_size <- min(10000, nrow(lda_all_proj))
sample_indices <- sample(1:nrow(lda_all_proj), sample_size)

# on récupère les coordonnées des tweets échantillonnés
coordonnee <- as.matrix(lda_all_proj[sample_indices, c("LD1", "LD2", "LD3")])

# On extrait les sentiments en nombres avec as.numeric
sentiments <- as.numeric(lda_all_proj$Sentiment_Real[sample_indices])
# ====> 1 = Irrelevant, 2 = Negative, 3 = Neutral, 4 = Positive

# on calule les distances entre tous les tweets
distances <- dist(coordonnee)

# Calculer le score de silhouette
silhouette_resultat <- silhouette(sentiments, distances)

# on calcule la moyenne
score_moyen <- mean(silhouette_resultat[, "sil_width"])

print(paste("Score moyen", round(score_moyen, 3)))