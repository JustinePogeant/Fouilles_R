control <- trainControl(
  method = "cv",
  number = 5
)

nb_cv <- train(
  emotion ~ .,
  data = train_data,
  method = "nb",
  trControl = control,
  tuneGrid = expand.grid(
    fL = c(0, 1),
    usekernel = c(TRUE, FALSE),
    adjust = c(0.5, 1)
  )
)

nb_cv
