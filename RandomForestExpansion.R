set.seed(081302)
m3_rf <-train(HeartDisease ~ ., data = train_data, method = "rf", trControl = ctrl,  tuneGrid = grid_rf)

m3_rf$bestTune
m3_rf$results

m3_rf <- randomForest(HeartDisease ~ ., data = train_data)

# Get variable importance
var_importance <- importance(m3_rf)

# Create a data frame with variable names and importance scores
var_importance_df <- data.frame(
  Variable = rownames(var_importance),
  Importance = var_importance[, 1]  # Using MeanDecreaseGini
)

# Ordering the data frame by importance
var_importance_df <- var_importance_df[order(var_importance_df$Importance, decreasing = TRUE), ]

ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#2E75B6") +
  labs(x = "Predictor Variable", y = "Performance in Nodes", title = "Variable Importance Plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








