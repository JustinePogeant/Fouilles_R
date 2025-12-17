library(tm)
library(SnowballC)

corpus <- VCorpus(VectorSource(data$text))

corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

corpus
