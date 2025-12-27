# Chargement et pré-traitement des données :

# Chargez le jeu de données dans R ou R Studio ou VS Code.
# Nettoyez les données textuelles du tweet en supprimant les caractères spéciaux, les URL et
# les mots vides.

# on charge nos données en lisant les fichiers CSV
train_data <- read_csv("/Users/justinepogeant/Documents/E4/COURS_S1/P2/Fouilles de donnée R/Fouilles_R/project_2/data/twitter_training.csv", 
                       col_names = c("TweetID", "Entity", "Sentiment", "TweetText"),
                       show_col_types = FALSE)

validation_data <- read_csv("/Users/justinepogeant/Documents/E4/COURS_S1/P2/Fouilles de donnée R/Fouilles_R/project_2/data/twitter_validation.csv",
                           col_names = c("TweetID", "Entity", "Sentiment", "TweetText"),
                           show_col_types = FALSE)

# on regarde nos données
print("les données d'entraînement:")
print(paste("Tweets d'entraînement:", nrow(train_data)))

print("les données de validation:")
print(paste("Tweets de validation:", nrow(validation_data)))

# on regarde la structure des données
print("Structure des données d'entraînement:")
sentiment_table <- table(train_data$Sentiment)
print(sentiment_table)
print("Pourcentages:")
print(round(prop.table(sentiment_table) * 100, 2))

# les entités les plus fréquentes
print("\nles entités les plus fréquentes")
entity_table <- sort(table(train_data$Entity), decreasing = TRUE)
print(head(entity_table, 10))

# on affiche quelques exemples de tweets pour chaque sentiment
print("\nexemples de tweets pour chaque sentiment")
for(sent in unique(train_data$Sentiment)) {
  print(paste("\n--- Sentiment:", sent, "---"))
  examples <- train_data %>% 
    filter(Sentiment == sent) %>% 
    dplyr::select(TweetText) %>%  # Utilisation explicite de dplyr::select
    head(3)
  print(examples)
}