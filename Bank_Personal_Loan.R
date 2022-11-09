
dataset = read.csv('Bank_Personal_Loan_Modelling.csv')
#dataset2 = dataset


###### data before processing########3
str(dataset)
summary(dataset)
View(dataset)

############## Removing  Outliers ###############

#age no outliers

boxplot(dataset$Age)$out # = 0 

#Experience no outliers

boxplot(dataset$Experience)$out # = 0 

#mortgage drop

dataset <- dataset[,-c(1,5,9)]

#income

boxplot(dataset$Income)$out #? yes

dataset=subset(dataset, Income<=155)

boxplot(dataset$Income)

boxplot(dataset$Income)$out #=0

#ccavg

boxplot(dataset$CCAvg)$out #? yes

boxplot(dataset$CCAvg)

dataset=subset(dataset, CCAvg<=3.9)

boxplot(dataset$CCAvg)$out #=0




######boxplot After removing outliers

boxplot(dataset$Income)
boxplot(dataset$CCAvg)


############duplicate####################

nrow(dataset)
dataset[duplicated(dataset),]
dataset<- dataset [!duplicated(dataset),]

nrow(dataset)  #duplicates are removed


#########discretization#################

summary(dataset$Age)

datasett = dataset

GAge=cut(datasett$Age, br=c(22, 29, 39,49,59 ,67), labels=c("20's","30's","40's","50's","60's"))
table(GAge)

PL=cut(dataset$Personal.Loan, br=c(-0.01,0.5,1), labels=c("0","1"))
table(PL)

dataset$Personal.Loan=PL
datasett$Age=GAge


############### bining by mean ####################

no_of_bins <- 5
dataset$Experience=round(ave(dataset$Experience, rep(1:length(dataset$Experience), each = no_of_bins, length.out = length(dataset$Experience))))




################ Normalization ##################

summary(dataset)
# I decided what to normalize by observing the ranges

dataWithoutNormalization <- dataset
#create function
normalize  <- function (x) {
  return ((x - min(x)) / (max (x)- min (x))) }

dataset$Income<-normalize(dataset$Income) #assign


########### data after processing ##############
str(dataset)
summary(dataset)


############ phase 2


dataset2 = dataset


#split data
set.seed(1234)
#Training (70%)  Test (30%). 
ind <- sample(2, nrow(dataset2), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataset2[ind==1,]
testData <- dataset2[ind==2,]


library(party)
#myFormula specifies that Species is the target variable and all other variables are independent variables.
myFormula <- Personal.Loan ~ Age + Experience + Income + Family + CCAvg + Education + Securities.Account + CD.Account + Online + CreditCard  
#Build decision tree ctree()
data_ctree <- ctree(myFormula, data=trainData)

#check the prediction
#makes prediction for data.
table(predict(data_ctree), trainData$Personal.Loan)
print(data_ctree)
plot(data_ctree,type="simple")
plot(data_ctree)


# predict on test data
#the built tree is tested with test data
testPred <- predict(data_ctree, newdata = testData)

#Evaluate the model
#Create the confusion matrix
table(testPred, testData$Personal.Loan)



#accuracy and cm

library(caret)
results <- confusionMatrix(testPred, testData$Personal.Loan)
acc <- results$overall["Accuracy"]*100
acc
results
#________________


set.seed(1234)
#Training (90%)  Test (10%). 
ind <- sample(2, nrow(dataset2), replace=TRUE, prob=c(0.9, 0.1))
trainData <- dataset2[ind==1,]
testData <- dataset2[ind==2,]



#myFormula specifies that Species is the target variable and all other variables are independent variables.
myFormula <- Personal.Loan ~ Age + Experience + Income + Family + CCAvg + Education + Securities.Account + CD.Account + Online + CreditCard  
#Build decision tree ctree()
data_ctree <- ctree(myFormula, data=trainData)

#check the prediction
#makes prediction for data.
table(predict(data_ctree), trainData$Personal.Loan)
print(data_ctree)
plot(data_ctree,type="simple")
plot(data_ctree)


# predict on test data
#the built tree is tested with test data
testPred <- predict(data_ctree, newdata = testData)

#Evaluate the model
#Create the confusion matrix
table(testPred, testData$Personal.Loan)




#accuracy and cm

results <- confusionMatrix(testPred, testData$Personal.Loan)
acc <- results$overall["Accuracy"]*100
acc
results

#________________


set.seed(1234)
#Training (60%)  Test (40%). 
ind <- sample(2, nrow(dataset2), replace=TRUE, prob=c(0.60, 0.40))
trainData <- dataset2[ind==1,]
testData <- dataset2[ind==2,]



#myFormula specifies that Species is the target variable and all other variables are independent variables.
myFormula <- Personal.Loan ~ Age + Experience + Income + Family + CCAvg + Education + Securities.Account + CD.Account + Online + CreditCard  
#Build decision tree ctree()
data_ctree <- ctree(myFormula, data=trainData)

#check the prediction
#makes prediction for data.
table(predict(data_ctree), trainData$Personal.Loan)
print(data_ctree)
plot(data_ctree,type="simple")
plot(data_ctree)


# predict on test data
#the built tree is tested with test data
testPred <- predict(data_ctree, newdata = testData)

#Evaluate the model
#Create the confusion matrix
table(testPred, testData$Personal.Loan)

#accuracy and cm

results <- confusionMatrix(testPred, testData$Personal.Loan)
acc <- results$overall["Accuracy"]*100
acc
results


# **** CLUSTERING **** 

str(dataset)

#dropping unnecessary columns ( non-numeric columns )
dataset3 <- dataset[,-c(4,6,7,8,9,10,11)]

str(dataset3)

#scaling data
dataset3 <- scale(dataset3)


#checking appropriate number of clusters
library(factoextra)

fviz_nbclust(dataset3,  kmeans , method = "wss") + 
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")


fviz_nbclust(dataset3, kmeans , method = "silhouette") +
  labs(subtitle="Silhouette method")




#visualizing data with two clusters
km2 = kmeans(dataset3, centers = 2, nstart = 25)
fviz_cluster(km2, dataset3)

#interpreting cluster
km2$centers

#average for each cluster
library(cluster)
avg_sil <- silhouette( km2$cluster , dist(dataset3))
fviz_silhouette(avg_sil)



#visualizing data with 3 clusters
km3 = kmeans(dataset3, centers = 3, nstart = 25)
fviz_cluster(km3,dataset3)

#interpreting cluster
km3$centers

#average for each cluster
avg_sil <- silhouette( km3$cluster , dist(dataset3))
fviz_silhouette(avg_sil)



#visualizing data with 4 clusters
km4 <- kmeans(dataset3, centers = 4, nstart = 25)
fviz_cluster(km4, dataset3)

#interpreting cluster
km4$centers

#average for each cluster
avg_sil <- silhouette( km4$cluster , dist(dataset3))
fviz_silhouette(avg_sil)



#clustering patterns interpretation
library(GGally)
library(plotly)

dataset3$cluster <- as.factor(km4$cluster)
p <- ggparcoord(data = dataset3, columns = c(1:4), groupColumn = "cluster", scale = "std") + labs(x = "Personal Loan", y = "value (in standard-deviation units)", title = "Clustering")
ggplotly(p)


