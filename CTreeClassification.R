set.seed(081302)
# Define the control method
ctrl <- trainControl(method="cv", n=3)

# center and scale numeric variables then do pca for consistency
preProc <- c("center", "scale")

# estimating the CTree1
m1_ctree <- train(HeartDisease ~ ., data=HDn, method="ctree", trControl = ctrl, preProcess=preProc)

plot((m1_ctree$finalModel), main = "Ctree Model")