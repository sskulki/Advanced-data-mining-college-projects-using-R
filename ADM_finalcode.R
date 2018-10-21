#import all libraries
library(randomForest)
library(caret)
library(ROCR)
library(Metrics)
library(gbm)
library(xgboost)
library(lmSupport)
library(FSelector)
library(gplots)
library(lattice)
library(readr)
library(doBy)
library(stats)
library(ggplot2)
#Import dataset
train_3 <- read_csv("train_3.csv")  
dataset1 <- train_3 #Original dataset, backup
#Remove ID from the dataset
dataset1$ID <- NULL

################################ Dependent variable transformation ###############
#Plot the target variable
plot(dataset1$target) #Plot showes right skewed, need to be transformed

#Convert the target variable to log as 
dataset1$target <- log(dataset1$target) #Bell shaped and residuals are distributed randomly

################################## Data cleaning ###############################
#Check for constants and zero standard deviations
same <- sapply(dataset1, function(.col){ 
  all(is.na(.col))  || all(.col[1L] == .col) 
}) 
dataset1 <- dataset1[!same] # Removed 256 constant features from the dataset
#Rename the columns, as they have anonymous dataset
colnames(dataset1) <- paste("var", 1:4736, sep="")
#Check for NA values
sum(is.na(dataset1)) # Zero NA values

############################### Feature selection ############################

#Dimensionality reduction by remove the near-zero variance features
dataset2 <- dataset1[,-nearZeroVar(dataset1)] #Dimensions reduced to 41, need to chek if we have 
#missed important features. We will check for important features using random forest

#Check for variable importance
forest <- randomForest(var1 ~ ., data=dataset1, importance=TRUE, ntree=1000)
imp1 <- varImpPlot(forest, col = "pink", sort = TRUE)
imp2 <- (sort(forest$importance, decreasing = TRUE))
impvars1 <- dataset1[,imp2]
colnames(dataset2)
colnames(impvars1)
#Combining skipped important variables from the near-zero variance
dataset3 <- Reduce(setdiff, list(colnames(dataset2),colnames(impvars1)))
#Combine missed important variables
impvars1 <- cbind(impvars1, dataset2)

################################  PCA  ######################################
#PCA didnot give enough dimensionality reduction as 95%-97% data is sparse
pca <- prcomp(impvars1[,-62], scale. = T, center = T)
screeplot(pca, type="lines")

############################### Create training and testing dataset ###############
sample <- createDataPartition(impvars1$target, p = .75, list = FALSE)
train <- impvars1[sample, ]
test <- impvars1[-sample, ]

############################### Modeling #########################################
#Simple linear model, considering as base model for benchmarking
lm.model <- lm(target ~., data = train)
plot(lm.model)
pred_lm <- predict(lm.model, test[,-62])
(rmsle(pred_lm, test[,62]))   #0.1116363
(rmse(pred_lm, test[,62]))    #1.659465

#Plot the best fit line of lm.model and plot predictions vs test data
options(repr.plot.width=8, repr.plot.height=4)
my_data1 = as.data.frame(cbind(predicted = pred_lm,
                               observed = test$target))
ggplot(my_data1,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Linear regression: Prediction vs Test Data") +
  xlab("Predecited transactions") + ylab("Observed transactions") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

#Apply boosting with GBM
#Started with 2000
gbm.model <- gbm(train$target ~ ., data = train, distribution = "gaussian", n.trees = 2000,
                 interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, verbose = FALSE)
pred_gbm <- predict(gbm.model, newdata = test[,-62], n.trees = 2000) #Overfit 
#Then with 1500
gbm.model <- gbm(train$target ~ ., data = train, distribution = "gaussian", n.trees = 1500,
                 interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, verbose = FALSE)
pred_gbm <- predict(gbm.model, newdata = test[,-62], n.trees = 1500)
#Then with 1000
gbm.model <- gbm(train$target ~ ., data = train, distribution = "gaussian", n.trees = 1000,
                 interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, verbose = FALSE)
pred_gbm <- predict(gbm.model, newdata = test[,-62], n.trees = 1000)
#Then with 800
gbm.model <- gbm(train$target ~ ., data = train, distribution = "gaussian", n.trees = 800,
                 interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, verbose = FALSE)
pred_gbm <- predict(gbm.model, newdata = test[,-62], n.trees = 800)
#Then with 400
gbm.model <- gbm(train$target ~ ., data = train, distribution = "gaussian", n.trees = 400,
                 interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, verbose = FALSE)
pred_gbm <- predict(gbm.model, newdata = test[,-62], n.trees = 400)
(rmsle(pred_gbm, test[,62]))     #0.101141
(rmse(pred_gbm, test[,62]))      #1.507025
print(ntress)
print(summary(gbm.model))
#Plot the gbm model for best fit line
ntress <- gbm.perf(gbm.model) # plot the squared error loss vs iteration
#predicted vs true values
options(repr.plot.width=8, repr.plot.height=4)
my_data2 = as.data.frame(cbind(predicted = pred_gbm,
                               observed = test$target))
ggplot(my_data2,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("GBM: Prediction vs Test Data") +
  xlab("Predecited transactions") + ylab("Observed transactions") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


#Apply boosting with XGBoost, as GBM was overfitting
train <- as.matrix(train) #convert to matrix
test1 <- test$target
test <- as.matrix(test) #convert to matrix
train_xgb <- xgb.DMatrix(data = train, label = train[,"target"], missing = NA)
params <- list(booster = "gblinear", obejctive = "reg:linear", eval_metric = "rmse", eta = 0.1, max_depth = 15,
               min_child_weight = 0.6, subsample = 0.5, colsample_bytree = 0.8, alpha = 0.01, 
               verbose = 0)
#Above parameter are finalyzed after tuning
xgb.model <- xgboost(params = params, data = train_xgb, nrounds = 1000,
                     verbose = 0,early_stopping_rounds = 100)
pred <- predict(xgb.model, test, ntreelimit = xgb.model$best_ntreelimit)
(rmsle(pred, test1)) #0.03243058
(rmse(pred, test1)) #0.4610087

#Plot the xgboost
options(repr.plot.width=8, repr.plot.height=4)
my_data2 = as.data.frame(cbind(predicted = pred_gbm,
                               observed = test$target))
ggplot(my_data2,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("XGBoost: Prediction vs Test Data") +
  xlab("Predecited transactions") + ylab("Observed transactions") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


