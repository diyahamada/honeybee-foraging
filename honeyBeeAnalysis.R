data = read.csv('df.csv', header = TRUE)
head(data)
colnames(data)

# simplify into factor-specific runs for further insight into weighting and decision-making process
#double check padma's methods

# Decision tree (rpart)
library(rpart)
library(rpart.plot)

status_tree <- rpart(status ~ Day..Seasonal.Cycle. + Second..Daily.Cycle.+Age+mins_since_df_visit, data = data, method = "class")
summary(status_tree)
rpart.plot(status_tree, cex = 1)

visit_tree <- rpart(mins_since_df_visit ~ Day..Seasonal.Cycle. + Second..Daily.Cycle.+Age, data = data)
summary(visit_tree)
rpart.plot(visit_tree, cex = 1)



# simplify into factor-specific runs for further insight into weighting and decision-making process
#double check padma's methods

linear = lm(status~.-uid -X -YYYYMMDD -HHMMSS, data = data)
summary(linear)

library(tree)

tree_class = tree(formula = as.factor(status) ~ Day..Seasonal.Cycle. + Second..Daily.Cycle.+Age+mins_since_df_visit, data = data)
summary(tree_class)
plot(tree_class)
text(tree_class, pretty = 0)

#----------------------------------------------------------------------

predictions <- predict(tree_model, newdata = new_data)
predictions_class <- predict(tree_model_class, newdata = new_data, type = "class")


# Random forest
library(randomForest)
rf_classification = randomForest(status~., data=data, importance=TRUE)

print(rf_attempt)
print(rf_class_model)
predictions = predict(rf_attempt, withheld_data)

# xgboost
library(xgboost)
data_matrix= as.matrix(data[, -which(names(data) == "status")])
target = data$status
#80-20
set.seed(42)
train_index = sample(1:nrow(data), 0.8 * nrow(data))
train_data = data_matrix[train_index, ]
test_data = data_matrix[-train_index, ]
train_target= target[train_index]
test_target = target[-train_index]

gbm_model = xgboost(data = train_data, label = train_target, nrounds = 100, objective = "reg:squarederror")
predictions = predict(gbm_model, test_data)

#e1071 SV machine
library(e1071)

svm_classification_model =svm(status ~ ., data = data, type = 'C-classification', kernel = 'radial')

predictions = predict(svm_model, new_data)

# KNN
library(class)

knn_predictions = knn(train = data_train, test = data_test, cl = target_train, k = 5)

knn_predictions = knn.reg(train = data_train, test = data_test, y = target_train, k = 5)
