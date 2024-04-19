sqrt(920)/2 #Calculate starting K

k_values <- data.frame(k = c(1,3,6,9,12,15,18,21,24,27,30))

set.seed(081302)
trainIndex <- createDataPartition(HDn$HeartDisease, p = .8, 
                                  list = FALSE, 
                                  times = 1)

HDdf_train <- HDn[trainIndex,]
HDdf_test <- HDn[-trainIndex,]

set.seed(081302)
ctrl <- trainControl(method="cv", n=5)
knnFit <- train(HeartDisease ~ ., data = HDdf_train, method = "knn", 
                trControl = ctrl, preProcess = c("range"), 
                tuneGrid= k_values)


# print best k
knnFit$bestTune
# print results for each k tested
knnFit$results

# Predict on test data
HDdf_test_pred <- predict(knnFit, newdata = HDdf_test)

# Evaluate model performance in TESTING sample
confusionMatrix(HDdf_test_pred, HDdf_test$HeartDisease)

# obtain predicted probabilities
probs <- predict(knnFit, newdata=HDdf_test, type="prob")[,2]
roc_obj <- roc(HDdf_test$HeartDisease, probs)

plot(roc_obj, print.thres = "best", print.thres.best.method = "closest.topleft",
     main = "Receiver Operator Curve (ROC)", # Title
     col = "blue", # Color of the curve
     lwd = 2, # Line width
     print.auc = TRUE, # Display AUC value
     print.auc.cex = 1.5, # AUC text size
     print.auc.offset = c(0.1, 0.1), # AUC text offset
     print.auc.just = c("left", "top"), # AUC text justification
     print.auc.col = "black", # AUC text color
     print.thres.pch = 20, # Threshold point shape
     print.thres.col = "red", # Threshold point color
     print.thres.cex = 1.2, # Threshold point size
     print.thres.adj = c(0.5, -0.5), # Threshold point text adjustment
     col.thres = "red", # Threshold line color
     print.thres.pattern = "/", # Threshold point pattern
     print.thres.pattern.cex = 1, # Threshold point pattern size
     xlab = "False Positive Rate (1 - Specificity)", # X-axis label
     ylab = "True Positive Rate (Sensitivity)", # Y-axis label
     cex.axis = 1.2, # Axis label size
     cex.lab = 1.2, # Axis tick label size
     cex.main = 1.5, # Title size
     cex.sub = 1.2, # Subtitle size
     grid = TRUE # Add grid
)
legend("bottomright", legend = c("ROC Curve"), col = "blue", lty = 1, lwd = 2, cex = 1.2)

