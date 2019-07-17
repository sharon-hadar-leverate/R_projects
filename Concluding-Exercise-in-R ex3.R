#3. In this exercise you will employ logistic regression with the ‘2016 World Health Report’
#data set, to examine how well can we predict if a country is European or African based
#on the given predictors.

#a) Using the ‘dplyr’ package or any other programming method you want to apply, 
#create a subset of the ‘2016 World Health Report’ data set, 
#that includes only European or African countries 
#(50 European countries and 43 African countries). 
#The new data set will have a new field called ‘Region2’ 
#that will contain either ‘Europe’ or ‘Africa’, 
#as well as a field called ‘IsEuropean’ which is set to true for European countries and false
#otherwise.

world <- read.csv('./2016 World Happiness Report.csv',header=T)
attach(world)
summary(world$Region)
EuroGroup =c("Central and Eastern Europe","Western Europe")
AfroGroup =c("Middle East and Northern Africa","Sub-Saharan Africa")

df2 <- filter(.data = world,Region %in%  c(EuroGroup, AfroGroup) )  %>% 
  mutate(Region2=if_else(Region %in%  EuroGroup, "Europe", "Africa", missing = NULL),
         IsEuropean=if_else(Region %in%  EuroGroup, TRUE, FALSE, missing = NULL))

df2$Region2 <- as.factor(df2$Region2)

summary(df2)
#is more then 43 since it is mixed with middel east countries but i assume it is not the point

#b) Using ‘ggplot2’ and with the help of ‘tidyr’, create a plot similar to Figure1 below.
#Based on this figure, which predictors could be helpful in predicting if a country is from
#Europe or Africa?

world_long = gather(data = df2,key = sector, value = value,... = Economy:Generosity,factor_key = TRUE)

ggplot(data =world_long)+
  geom_boxplot(mapping = aes(x=Region2,y = value,color= Region2))+
  facet_wrap( ~sector, scales = "free")

#best proprties base on this plot are health, economy, and family.


#c) Using glm, fit a simple logistic regression to IsEuropean using each of the six predictors
#shown in Figure1. According to the fits, which predictor by itself is better in
#distinguishing between European and African countries? Create a ggplot showing the
#probability of a country being European according to the value of this best predictor.

library(ggplot2)
attach(df2)

logm.Economy<-glm(formula = IsEuropean ~ Economy, family =binomial , data = df2)
logm.Family<-glm(formula = IsEuropean ~ Family, family =binomial , data = df2)
logm.Health<-glm(formula = IsEuropean ~ Health, family =binomial , data = df2)
logm.Freedom<-glm(formula = IsEuropean ~ Freedom, family =binomial , data = df2)
logm.Trust<-glm(formula = IsEuropean ~ Trust, family =binomial , data = df2)
logm.Generosity<-glm(formula = IsEuropean ~ Generosity, family =binomial , data = df2)
Economy_Fit=fitted(logm.Economy)
Family_Fit=fitted(logm.Family)
Health_Fit=fitted(logm.Health)
Freedom_Fit=fitted(logm.Freedom)
Trust_Fit=fitted(logm.Trust)
Generosity_Fit=fitted(logm.Generosity)
g1 <-ggplot() +  geom_line(mapping=aes(x=Economy,y=Economy_Fit))
g2 <-ggplot() +  geom_line(mapping=aes(x=Family,y=Family_Fit))
g3 <-ggplot() +  geom_line(mapping=aes(x=Health,y=Health_Fit))
g4 <-ggplot() +  geom_line(mapping=aes(x=Freedom,y=Freedom_Fit))
g5 <-ggplot() +  geom_line(mapping=aes(x=Trust,y=Trust_Fit))
g6 <-ggplot() +  geom_line(mapping=aes(x=Generosity,y=Generosity_Fit))
grid.arrange(g1,g2,g3,g4,g5,g6)
#we can see that health hold the best separator between 0 probability to be European and 1 - it is the most similar to zigmond function

#d) Based on the fit using the best predictor from c), create a confusion table of actual region
#vs. predicted region, using each of the following threshold values for p(IsEuropean=Yes):
#  1/4, 1/2, 3/4. Report the sensitivity and specificity of the classifier in each case. Which
#of these classifiers would you use? Specify if it depends on anything.

confusion.glm <- function(data, model,tresh) {
  prediction <- ifelse(predict(model, data, type='response') > tresh, TRUE, FALSE)
  confusion  <- table(prediction, as.logical(model$y))
  confusion  <- cbind(confusion, c(1 - confusion[1,1]/(confusion[1,1]+confusion[2,1]), 1 - confusion[2,2]/(confusion[2,2]+confusion[1,2])))
  confusion  <- as.data.frame(confusion)
  names(confusion) <- c('FALSE', 'TRUE', 'class.error')
  confusion
}
confusion.glm(df2,logm.Health,0.25)
confusion.glm(df2,logm.Health,0.5)
confusion.glm(df2,logm.Health,0.75)
