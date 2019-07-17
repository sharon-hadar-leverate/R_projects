#4. In this exercise you will employ the ‘caret’ package wrapper for randomForest with the
#‘2016 World Health Report’ data set, in order to fit and tune a model for predicting the
#Happiness score of a country.

#a) Use caret::createDataPartition to create training and test sets, with the training set
#containing 70% of the data.

world <- read.csv('./2016 World Happiness Report.csv',header=T)
attach(world)
#install.packages("caret")
#install.packages("caret", dependencies = c("Depends", "Suggests"))

library("caret")

train<-createDataPartition(y=Happiness,  p = 0.7, list = FALSE)
world.train <- world[train,]
world.test <- world[-train,]
Happiness.test <- world$Happiness[-train]


#b) Explain what is the meaning of the tuning parameter ‘mtry’ of randomForest. 
#What is the maximum number it can take in our case, assuming we want to fit Happiness using
#all the predictors in the data set (not including the field ‘Country’)?

#mtry is the number of variables randomly sampled as candidates at each split,
#the maximum number it can take is bound by the number of variables in the model, 
#as it specifies the size of the variable subset that is randomly picked for each random forest iteration.
#the maximun number would be all distinct factors in each factor feture pluse the number of numric fetures 

numricValues = table(sapply(world, is.numeric))["TRUE"]
factorValues = sapply(world[,sapply(world, is.factor)], nlevels)
(maxmtry = sum(factorValues,numricValues))

# maximum value for mtry =174


#c) Use caret::train with method=‘rf’ to fit the model to the training set. 
#Set that the method will search for the best possible value for ‘mtry’ out of all possible values,
#using root-mean-squre error (RMSE) as the metric for selecting the best value. 
#Set the resampling method to be used to 10-fold cross-validation. 
#Set the number of trees to be grown to 500. 
#Make sure that the importance of the predictors will be assessed.
#Using the obtained fit, 
#plot the cross-validation RMSE as a function of mtry 
#and the cross-validation Rsquared as a function of mtry. 
#What is the selected value for ‘mtry’
#according to the fit? What would have been the selected value for ‘mtry’ if we used
#Rsquared as the metric for selection?

#install.packages("randomForest")
#library(plyr)

Happiness.rfFit <- train(Happiness~.,world.train,
                      method='rf',
                      metric='RMSE',
                      trControl=trainControl(method='cv',number=10),
                      tuneGrid=data.frame(mtry=3:150),
                      ntree=500,
                      importance=TRUE)
plot(Happiness.rfFit, metric='RMSE',main="RMSE by mtry")
plot(Happiness.rfFit, metric='Rsquared',main="Rsquared by mtry")
getTrainPerf(Happiness.rfFit)

(winning.RMSE = Happiness.rfFit$results[which.min(Happiness.rfFit$results[, "RMSE"]), ])
(winning.Rsquared = Happiness.rfFit$results[which.max(Happiness.rfFit$results[, "Rsquared"]), ])
winning.Rsquared$mtry
# the wining mtry for Rsquared is mtry=108

#d) Using the obtained fit, plot the calculated variable importance. What are the top-3
#predictors effecting Happiness according to this plot?

ImpMeasure<-data.frame(varImp(Happiness.rfFit)$importance)
ImpMeasure$Vars<-row.names(ImpMeasure)
ImpMeasure<-ImpMeasure[order(-ImpMeasure$Overall),][1:20,]

barplot(ImpMeasure$Overall, names.arg=ImpMeasure$Vars ,main="important Measure by variable",las=2)


# the top-3 are : 
head(ImpMeasure,3)$Vars

#e) Use the model fit to estimate the Happiness score for the test set. Calculate the RMSE
#for these predictions. Create a ggplot with the actual Happiness score vs. the predicted
#Happiness score.

Happiness.rf <- predict(Happiness.rfFit, newdata=world.test)
(test.mse.rf <- mean((Happiness.rf - Happiness.test)^2))
range = (min(Happiness.test)-0.25):(max(Happiness.test)+0.75)
ggplot()+
  geom_point(mapping = aes(x=Happiness.test,y = Happiness.rf))+
  ylab("Predicted Happiness")+
  xlab("Actual Happiness")+
  geom_smooth(mapping = aes(x = range, y = range), method='lm')+
  geom_segment(aes(x=Happiness.test, xend=Happiness.test, y=Happiness.rf, yend=Happiness.test, color="error"))

