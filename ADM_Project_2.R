setwd("/Users/shridhar/Downloads/CA") #change this to where you downloaded the .csv
ca_dataset <- read.csv("/Users/shridhar/Downloads/PASSNYC.csv", stringsAsFactors = F, na.strings = "")
# remove columns that don't carry any predictive value whatsoever
ca_dataset <- ca_dataset[,-c(2:5)] 
# ok, now we need to make a dataset unique to you
set.seed(17153581) # <-- put your student number here WITHOUT the x!! Leave off a starting zero if you have one
# e.g.: set.seed(16245678)

index <- sample(1:nrow(ca_dataset), 1260, replace=FALSE)
my_ca_dataset <- ca_dataset[index, ] # here we're subsetting your part of the dataset

#Unfortunately, due to a technical error, some columns of the data were lost :(
dependent <- my_ca_dataset$Target
my_ca_dataset$Target <- NULL

easier <- my_ca_dataset[, c(4,6,8,15,16,18,19,20,36)]
harder <- my_ca_dataset[, -c(4,6,8,15,16,18,19,20,36)]

index2 <- sample(1:(ncol(easier)), 5, replace=FALSE)
easier <- easier[, index2]
index2 <- sample(1:(ncol(harder)), 35, replace=FALSE)
harder <- harder[, index2]
my_ca_dataset <- cbind(dependent, easier, harder)

names(my_ca_dataset)[1] <- "Target"

print(paste("I have:", names(my_ca_dataset)))

# Unfortunately, there was another incident. The intern split their coffee
# on your keyboard and may have deleted data from a number of the remaining columns

v <- round(runif(ncol(my_ca_dataset), min=1, max=6))
if (max(v) > 4) {
  v <- cut(v, breaks = c(0,4,max(v)), labels = c("a","b"))
} else {
  v <- cut(v, breaks = c(0,max(v)-1,max(v)), labels = c("a","b")) 
}


Pna <- runif(1000, min=0, max=0.13)
Pna <- Pna - .03
Pna[Pna < 0] <- 0
v[1] <- "a"

for (i in 1:length(v)) {
  if (v[i] == "b") {
    nadex <- sample(1:nrow(my_ca_dataset), nrow(my_ca_dataset) * Pna[sample(1:length(Pna), 1, replace=FALSE)], replace=FALSE)
    my_ca_dataset[nadex, i] <- NA
    v[i] <- "a"
  }
}


# Clean up

rm(v)
rm(Pna)
rm(ca_dataset)
rm(index)
rm(index2)
rm(i)
rm(nadex)
rm(dependent)
rm(easier)
rm(harder)

###### Backup your data set

#In case anything goes wrong, we'll store a copy to disk.
write.csv(my_ca_dataset, file="my_ca_dataset.csv", row.names = F, na = " ")

#To reload it run (you may need to rerun your foundations code again)
my_ca_dataset <- read.csv("my_ca_dataset.csv", stringsAsFactors = T, na.strings = " ")

#Now please begin, and good luck!
#Foundations

#F1
names(my_ca_dataset)
#Removing ZIP and lattitude columns as it will not make sense to use them
my_ca_dataset$Zip <- NULL
my_ca_dataset$Latitude <- NULL
my_ca_dataset$Address..Full. <- NULL
DF1 <- my_ca_dataset
my_ca_dataset <- DF1
#Adress cleaning
address <- my_ca_dataset$Address..Full.
address <- as.character(address)
addressnew <- sapply(strsplit(address, split = ' ', fixed = TRUE), function(x) (x[-3]))
#Cleaning percentage from Student.Attendance.Rate
my_ca_dataset$Student.Attendance.Rate <- (sapply(my_ca_dataset$Student.Attendance.Rate, function(x) as.numeric(gsub("%", "", x))))
my_ca_dataset$Student.Attendance.Rate <- as.numeric(my_ca_dataset$Student.Attendance.Rate)

##Cleaning Percent.of.Students.Chronically.Absent
my_ca_dataset$Percent.of.Students.Chronically.Absent <- (sapply(my_ca_dataset$Percent.of.Students.Chronically.Absent, function(x) as.numeric(gsub("%", "", x))))
my_ca_dataset$Percent.of.Students.Chronically.Absent <- as.numeric(my_ca_dataset$Percent.of.Students.Chronically.Absent)
#Cleaning percentage from Trust
my_ca_dataset$Trust.. <- (sapply(my_ca_dataset$Trust, function(x) as.numeric(gsub("%", "", x)))) 
my_ca_dataset$Trust.. <- as.numeric(my_ca_dataset$Trust..)
#Encoded Strong.Family.Community.Ties.Rating
table(my_ca_dataset$Strong.Family.Community.Ties.Rating)
my_ca_dataset$Strong.Family.Community.Ties.Rating <- factor(my_ca_dataset$Strong.Family.Community.Ties.Rating, levels = c("Approaching Target","Exceeding Target","Meeting Target","N/A","Not Meeting Target"), labels = c(1,2,3,4,5))
levels(my_ca_dataset$Strong.Family.Community.Ties.Rating)[4] <- c(levels(my_ca_dataset$Strong.Family.Community.Ties.Rating)[5])
#Encoding Collaborative.Teachers.Rating
table(my_ca_dataset$Collaborative.Teachers.Rating)
my_ca_dataset$Collaborative.Teachers.Rating <- factor(my_ca_dataset$Collaborative.Teachers.Rating, levels = c("Approaching Target","Exceeding Target","Meeting Target","N/A","Not Meeting Target"),labels = c(1,2,3,4,5))
levels(my_ca_dataset$Collaborative.Teachers.Rating)[4] <- c(levels(my_ca_dataset$Collaborative.Teachers.Rating)[5])
#Missing vvalues
summary(is.na(my_ca_dataset))
#missing values are in the following columns:
#Student.Attendance.Rate, Grade.8.ELA.4s...Black.or.African.American, Trust, Grade.High, Percent.of.Students.Chronically.Absent, Grade.4.ELA.4s...American.Indian.or.Alaska.Native
library(mice)
mice_mod2 <- mice(my_ca_dataset[, !names(my_ca_dataset) %in% c('Target','Grade.6.Math.4s...Asian.or.Pacific.Islander','Grade.6.ELA.4s...American.Indian.or.Alaska.Native','Grade.5.Math.4s...White')], method='rf')
mice_output2 <- complete(mice_mod2)

my_ca_dataset$Student.Attendance.Rate <- mice_output2$Student.Attendance.Rate
my_ca_dataset$Grade.8.ELA.4s...Black.or.African.American <- mice_output2$Grade.8.ELA.4s...Black.or.African.American
my_ca_dataset$Trust.. <- mice_output2$Trust..
my_ca_dataset$Grade.High <- mice_output2$Grade.High
my_ca_dataset$Percent.of.Students.Chronically.Absent <- mice_output2$Percent.of.Students.Chronically.Absent
my_ca_dataset$Grade.4.ELA.4s...American.Indian.or.Alaska.Native <- mice_output2$Grade.4.ELA.4s...American.Indian.or.Alaska.Native
my_ca_dataset$Grade.3.Math.4s...All.Students <- mice_output2$Grade.3.Math.4s...All.Students
my_ca_dataset$Average.Math.Proficiency <- mice_output2$Average.Math.Proficiency
#grade.high
table(my_ca_dataset$Grade.High)
my_ca_dataset$Grade.High <- as.factor(my_ca_dataset$Grade.High)
levels(my_ca_dataset$Grade.High)[9] <- c(levels(my_ca_dataset$Grade.High)[10]) # combining 2 levels
#Average math proficiency
my_ca_dataset$Average.Math.Proficiency[my_ca_dataset$Average.Math.Proficiency=="N/A"] <- 3
my_ca_dataset$Average.Math.Proficiency[my_ca_dataset$Average.Math.Proficiency=="N/A"] <- NULL
table(my_ca_dataset$Average.Math.Proficiency)
my_ca_dataset$Average.Math.Proficiency <- as.numeric(my_ca_dataset$Average.Math.Proficiency)

my_ca_dataset$Community.School. <- as.factor(my_ca_dataset$Community.School.)
my_ca_dataset$Target <- as.factor(my_ca_dataset$Target)
#Split the data into 
library(ggplot2)
library(caret)
DF1 <- my_ca_dataset
index <- createDataPartition(my_ca_dataset$Target, p=0.75, list = FALSE)
#Normal dataset
training_ca <- my_ca_dataset[index,]
testing_ca <- my_ca_dataset[-index,]
#Numerical dataset
nums <- sapply(my_ca_dataset, function(x){is.numeric(x)})
numerics <- my_ca_dataset[,nums]
training_ca_n <- numerics[index,]
testing_ca_n <- numerics[-index,]
################################################################################
#A3
#C5.0
library(C50)
cFiftyNew <- C5.0(Target ~., data=training_ca, trials=50)
cFiftyPredictionNew <- predict(cFiftyNew, newdata = testing_ca[, -1])
(cFiftyAccuracyNew <- 1- mean(cFiftyPredictionNew != testing_ca$Target))
#[1] 0.7252396

#Random Forest
library(randomForest)
forest <- randomForest(Target ~ ., data=training_ca, importance=TRUE, ntree=100) #Due to time contraint choosing ntree=100
forestPred <- predict(forest, newdata = testing_ca[,-1])
(forestAcc <- 1 - mean(forestPred != testing_ca$Target))
#[1] 0.7476038
#According to the results random forest plays better role due to number of trees
################################################################################

#A4
library(kernlab)
svm.model <- ksvm(Target ~ ., data = training_ca, kernel = "vanilladot", type = "C-svc")
SVMpred <- predict(svm.model, newdata = testing_ca[, -1])
(accuracySVM <- 1-mean(SVMpred != testing_ca$Target))
#[1] 0.7507987

#Polynomial
svm.modelpoly <- ksvm(Target ~ ., data = training_ca, kernel = "polydot", type = "C-svc")
SVMpredpoly <- predict(svm.modelpoly, newdata = testing_ca[, -1])
(accuracySVMpoly <- 1-mean(SVMpredpoly != testing_ca$Target))
#[1] 0.7507987
#Unfortunately there is no much difffence between linear and polynomial svm's both giving same results
################################################################################
#A2
#Agglomerative algorithm
d <- dist(numerics, method = "euclidean") 
fit <- hclust(d, method="ward.D")
plot(fit) 
groups <- cutree(fit, k=3)
rect.hclust(fit, k=3, border="red")

#Density based clustering
install.packages("fpc")
library(fpc)
ds <- dbscan(numerics, eps=.25, MinPts=5)
table(ds$cluster, my_ca_dataset$Target)
my_ca_dataset$Target <- factor(my_ca_dataset$Target, levels = c("High","Medium","Low","NA"),labels = c("High","Medium","Low","NA"))
plot(ds, my_ca_dataset) #Getting Error in plot.new() : figure margins too large
################################################################################

#S1
#Using only numerics data in dimension reduction
kmeansClusters <- list()
kmeansScores <- c()
for (i in 2:10) {
  clusters <- kmeans(numerics, i)
  name <- paste0("kmeans", i)
  kmeansClusters[[name]] <- clusters
  kmeansScores <- rbind(kmeansScores, sum(clusters$withinss))
}

row.names(kmeansScores) <- c(2:10)
colnames(kmeansScores) <- c("k")
plot(kmeansScores, xlab="k", ylab="within groups sum of squares")
#K=4 seems to be good here
library(clusterSim)
kmeansScores <- c()
for (i in 2:10) {
  clusters <- kmeans(numerics, i)
  name <- paste0("kmeans", i)
  dbindex <- index.DB(numerics, clusters$cluster, centrotypes="centroids")
  kmeansScores <- rbind(kmeansScores, dbindex$DB)
}

row.names(kmeansScores) <- c(2:10)
colnames(kmeansScores) <- c("k")

plot(kmeansScores, xlab="k", ylab="DBIndex")
#Here K=2 seems works very well
library(fpc)
plotcluster(numerics, kmeansClusters[["kmeans2"]]$cluster)
plotcluster(numerics, kmeansClusters[["kmeans4"]]$cluster)
plotcluster(numerics, kmeansClusters[["kmeans5"]]$cluster)

#FOr a full dataset and n using kmedoids
library(cluster)

kmedoidsClusters <- list()
kmedoidsScores <- c()

gower_dist <- daisy(my_ca_dataset, metric = "gower", type = list(logratio = 3))

for (i in 2:20) { #for fun let's also increase the max k value as well
  clusters <- pam(gower_dist, k=i, diss=T) #note we switched to the full set of attibutes now so need to use medoids
  name <- paste0("kmedoids", i)
  kmedoidsClusters[[name]] <- clusters
  dbindex <- index.DB(gower_dist, clusters$clustering, centrotypes = "medoids", d=gower_dist)
  kmedoidsScores <- rbind(kmedoidsScores, dbindex$DB)
}

row.names(kmedoidsScores) <- c(2:20)
colnames(kmedoidsScores) <- c("k")

plot(kmedoidsScores, xlab="k", ylab="DBIndex")
#K=3 and K=4 looks better here so we have done the dimensionality reduction by having 
#cluster 3 or 4 which fairly better

################################################################################
#A1
tuneParams <- trainControl(method = "cv",number = 10, savePredictions = "final")

#Ensemble methods buliding CART
rpartTree <- train(training_ca[,-1], training_ca$Target, method="rpart", trControl=tuneParams, tuneLength=3)
rpart.pred <- predict(rpartTree, newdata = testing_ca[,-1])
confusionMatrix(rpart.pred, testing_ca$Target)
#Accuracy of 65.18%

#Ensemble methods buliding C5.0
c50Tree <- train(training_ca[,-1], training_ca$Target, method="C5.0", trControl=tuneParams, tuneLength=3)
c50.pred <- predict(c50Tree, newdata = testing_ca[,-1])
confusionMatrix(c50.pred, testing_ca$Target)
#Accuracy of 71.25%

#Ensemble methods buliding KNN : running on numerics
numerics = numerics[,c(34,1:33)]
training_ca_knn <- numerics[index,]
testing_ca_knn <- numerics[-index,]
knn <- train(training_ca_knn[,-1],training_ca_knn$Target,method="knn",trControl=tuneParams,tuneLength=10)
knn.pred <- predict(knn, newdata = testing_ca_knn[,-1])
confusionMatrix(knn.pred, testing_ca_knn$Target)
#Accuracy : 0.6358

#Here C5.0 > CART > KNN 

#Average of all 3 models
testing_ca$pred_rpart_prob<-predict(object = rpartTree,testing_ca,type="prob")
testing_ca$pred_c50_prob<-predict(object = c50Tree,testing_ca,type="prob")
testing_ca$pred_knn_prob<-predict(object = knn,testing_ca_knn,type="prob")

testing_ca$pred_avg<-(testing_ca$pred_rpart_prob+testing_ca$pred_knn_prob+testing_ca$pred_c50_prob)/3

testing_ca$preds <- apply(testing_ca$pred_avg, 1, FUN=function(x) {which.max(x)})
testing_ca$preds <- factor(testing_ca$preds, levels=c(1:3), labels=c(1:3))

table(testing_ca$Target, testing_ca$preds)
(1- mean(testing_ca$Target != testing_ca$preds))
#89%Accuracy
################################################################################