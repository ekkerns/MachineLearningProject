setwd("U:/General/Data_Scientist_Course/Course8_MachineLearning/Project")
#download and import datasets
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                      "train.csv")
        train<-read.csv("train.csv")
        download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                      "test.csv")
        test<-read.csv("test.csv")

#Remove variables that are not relevant to the analysis
        train<-train[, -c(1:7)] 
        test<-test[, -c(1:7)] 
#Remove variables with 100% missing values
        train<-train[,colSums(is.na(train)) == 0]
        test <-test[,colSums(is.na(test)) == 0]
#To reduce the # of variable to a manageable amount, PCA is conducted
        library(caret)
        PCA95<-preProcess(train[,1:52],method="pca",thresh=.95)
        #Result is that 14 variables are needed to capture 95% of the variance
        #Select those 14
        PCA14 <- preProcess(train[,1:52],method="pca",pcaComp=14)
        PCA14$rotation
        #Reduce to those
        trainPCA <- predict(PCA14,train[,1:52])
        testPCA <- predict(PCA14,test[,1:52])
#Create random forest and decison tree models
#as outome is neither binary nor continuos
#decison tree
        library(rpart)
        m1 <- rpart(train$classe ~ ., data=trainPCA)
        #plot model
        library(rattle)
        fancyRpartPlot(m1$finalModel)
#random forest
        library(randomForest)
        m2 <- randomForest(train$classe ~ ., data=trainPCA,do.trace=F)
        #plot model
        library(rattle)
        fancyRpartPlot(m2$finalModel)
#Compare Models
        confusionMatrix(test$classe,predict(m1,test))
        onfusionMatrix(test$classe,predict(m2,test))