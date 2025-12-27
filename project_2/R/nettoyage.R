
# Nettoyez les données textuelles du tweet en supprimant les caractères spéciaux, les URL et les mots vides.


#  on crée une fonction de nettoyage du texte
clean_text <- function(text) {
  text <- tolower(text)  # on met tout en minuscules
  text <- gsub("http\\S+|www\\S+", "", text)   # on supprime les URLs
  text <- gsub("@\\w+", "", text)  # on supprime les mentions @user
  text <- gsub("#\\w+", "", text)  # on supprime les hashtags
  text <- gsub("[[:punct:]]", " ", text)  # on supprime la ponctuation
  text <- gsub("[[:digit:]]", "", text)  # on supprime les chiffres
  text <- gsub("\\s+", " ", text)  # on supprime les espaces multiples
  text <- trimws(text)  # on supprime les espaces en début/fin
  return(text)
}

# on utilise la fonction sur nos données
train_data <- train_data %>%
  mutate(TweetText_clean = clean_text(TweetText))

validation_data <- validation_data %>%
  mutate(TweetText_clean = clean_text(TweetText))

# on comparaison avant et après le nettoyage
print("comparaison avant et après le nettoyage")
comparison <- train_data %>%
  filter(Sentiment == "Positive") %>%
  dplyr::select(TweetText, TweetText_clean) %>%
  head(5)

print("AVANT:")
print(comparison$TweetText)
print("APRÈS:")
print(comparison$TweetText_clean)

# on enlève les mots vides
print("supression des mots vides")
data("stop_words")  # Stopwords en anglais c'est les mots vides

# on supprime des mots vides
train_tokens <- train_data %>%
  unnest_tokens(word, TweetText_clean) %>% #transforme chaque tweet en lignes de mots
  anti_join(stop_words, by = "word") %>% #supprime les mots vides
  filter(nchar(word) > 2)  # enlève les tokens très courts (<= 2 caractères) parce qu'ils servent à rien