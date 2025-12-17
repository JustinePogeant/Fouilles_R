library(caret)
library(e1071)

set.seed(123)

train_index <- createDataPartition(dtm_df$emotion, p = 0.8, list = FALSE)
train_data <- dtm_df[train_index, ]
test_data <- dtm_df[-train_index, ]

model_nb <- naiveBayes(
  emotion ~ ., 
  data = train_data,
  laplace = 1
)

predictions <- predict(model_nb, test_data)
