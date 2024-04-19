highlycorr<- findCorrelation(cor(HDreg), cutoff =0.80) #Find strong correlation

set.seed(081302)
# Create data partitions
trainIndex <- createDataPartition(HDreg$HeartDisease, p = .8, list = FALSE, times = 1)
train_data <- HDreg[trainIndex,]
test_data <- HDreg[-trainIndex,]

# Define control method for all future regression models
ctrl <- trainControl(method = "repeatedcv", repeats = 3)

# Define tuning grids for models
grid_rpart <- expand.grid(cp = seq(0.01, 0.5, 0.05))
grid_rf <- expand.grid(mtry = seq(1, ncol(train_data) - 1, by = 3))
grid_mars <- expand.grid(degree = 1:3, nprune = 1:10)
grid_nn <- expand.grid(layer1=c(2,3), layer2=c(2,3), layer3=c(2,3))
grid_svm <- expand.grid(sigma = c(0.01, 0.2, 0.5, 1), C = c(1, seq(from = 5, to = 15, by = 5)))

# Train individual models
models <- caretList(
  HeartDisease ~ ., data = train_data,
  trControl = ctrl,  # Corrected typo here
  preProcess = c("center", "scale"),
  metric = "Rsquared",
  tuneList = list(
    lm = caretModelSpec(method = "lm"),
    tree = caretModelSpec(method = "ctree2"),
    rf = caretModelSpec(method = "rf", tuneGrid = grid_rf),
    steplm = caretModelSpec(method = "glmStepAIC", direction = "both"),
    elastic = caretModelSpec(method = "glmnet", tuneLength = 10),
    mars = caretModelSpec(method = "earth", tuneGrid = grid_mars),
    svm = caretModelSpec(method = "svmRadial", tuneGrid = grid_svm)
  )
)

# Compare results of the ensemble with individual models
# Which models predict the best?
results <- resamples(models)
summary(results)
bwplot(results)