##Sharon Hadar 
##201023991
##01.09.2017
##Exercise 2
#Open Source Tools for Intelligent Systems
#summer semester 2017

#install.packages("tidyr")
library(tidyr)

#!_______Q1:
#a) In class, we saw an exercise in which the iris data set was converted into long format
#   (in order to plot a boxplot of all the attributes simultaneously). Convert the iris data
#   set into long format (iris_long) as we did in the exercise. Now try to convert it back
#   into wide format (iris_wide). Can you explain what went wrong? What can you do,
#   before converting iris into long format, that will allow you to convert it back into wide
#   format? (hint: you should add something to iris before converting it to long format).

#!_______A1a:
# in order to change iris back from long to wide we need to add a new column 'subject' so we will know to aggregate and spread on distinct key
iris
iris$subject <- NULL
iris$subject <- c(1:nrow(iris))
head(iris)
iris_long = gather(data = iris,key = petalNsepalInfo, value = value,... = Sepal.Length:Petal.Width,factor_key = TRUE)
head(iris_long)
iris_wide = spread(data = iris_long, key = petalNsepalInfo, value = value)
head(iris_wide)
iris_wide

#b) Recreate the 6 plots from ex1 Q3b using ggplot, where this time have all the plots
#appear on the same plot in a separate facet by using facet_wrap() (hint: you should transform the data set into long format before plotting it).

#install.packages("ggplot2")
library("ggplot2")
world <- read.csv('./2016 World Happiness Report.csv',header=T)

world_long = gather(data = world,key = sector, value = value,... = Economy:Generosity,factor_key = TRUE)
detach(world_long)
attach(world_long)
summary(world_long)

ggplot(data =world_long)+
  geom_point(mapping = aes(value,Happiness,color= Region))+
  geom_smooth(mapping = aes(x = world_long$value, y = world_long$Happiness), method='lm')+
facet_wrap( ~sector,scales = "free_x")

#2. Using the 2016 World Happiness report and employing the ‘dplyr’ package:
#a) Create a data frame which includes the variables Country, Economy_Rank (which
#   contains a ranking of the Economies) and Economy (in this order), with only the top-5
#   ranked economies and the bottom-5 ranked economies, ordered from top to bottom.
#   Note 1: Use pipes to chain together dplyr operations.
#   Note 2: To perform or/and operations within the dplyr functions always use the |/&
#   version and not the ||/&& version.

#install.packages("dplyr")
library("dplyr")
summary(world)
orderedWorld <-word[order(word$Happiness,decreasing = TRUE),]
orderedWorld$EconomyRank <- c(1:nrow(orderedWorld))
orderedWorld<-rbind(head(orderedWorld),tail(orderedWorld))
orderedWorld<-select(orderedWorld,Country,EconomyRank,Economy)
orderedWorld

# dplyr piping way

select(
  filter(
    mutate(
      arrange(world,desc(Happiness))
      ,Economy_Rank=c(1:nrow(world)))
    ,Economy_Rank<6|Economy_Rank>nrow(world)-5)
 ,Country,Economy_Rank,Economy)

#b) Create a data frame which includes variables for the number of countries in each region
#   and the mean happiness for each region, ordered from the highest mean happiness to
#   the lowest mean happiness.
(
arrange(
  summarise(
    group_by(world, Region),
            count = n(),
            Happiness = mean(Happiness, na.rm = TRUE))
  ,desc(Happiness)) ->Regionworld
)
#3. Using the 2016 World Happiness report:
#a) Fit a simple linear model to predict Happiness using each of the six numeric predictors.
#   Report the estimate, 95% CI and p-value for the slope coefficient (not the intercept)
#   and the R2
#   statistic for each fit. How are the results obtained by the fits reafirm what
#   you can see in the plots from Q1b?

summary(world)
attach(world)

summary(lmEconomy <- lm(data=world, formula=Happiness~Economy))
(beta_lmEconomy <- confint(lmEconomy, level=0.95))
plot(lmEconomy, which=1)
# Economy is very Signif. 3 astrix, also have all positive small conf.interval , as we also can see high corollation @ Q1b

summary(lmFamily <- lm(data=world, formula=Happiness~Family))
(beta_lmFamily <- confint(lmFamily, level=0.95))
plot(lmFamily, which=1)
# Family is very Signif. 3 astrix, also have all positive small conf.interval , as we also can see high corollation @ Q1b


summary(lmHealth <- lm(data=world, formula=Happiness~Health))
(beta_lmHealth <- confint(lmHealth, level=0.95))
plot(lmHealth, which=1)
# Health is very Signif. 3 astrix, also have all positive small conf.interval , as we also can see high corollation @ Q1b

summary(lmFreedom <- lm(data=world, formula=Happiness~Freedom))
(beta_lmFreedom <- confint(lmFreedom, level=0.95))
plot(lmFreedom, which=1)
# Freedom is Signif. 3 astrix, also have all positive small conf.interval , as we also can see some corollation @ Q1b (something like 0.6)

summary(lmTrust <- lm(data=world, formula=Happiness~Trust))
(beta_lmTrust <- confint(lmTrust, level=0.95))
plot(lmTrust, which=1)
# Trust has some Signif. 3 astrix, also have all positive but not small conf.interval and but small F statics , as we also can see some corollation @ Q1b (something like 0.35)

summary(lmGenerosity <- lm(data=world, formula=Happiness~Generosity))
(beta_lmGenerosity <- confint(lmGenerosity, level=0.95))
plot(lmGenerosity, which=1)
# Generosity has almost no Signif. 1 astrix, also have all positive but not small and close to zero conf.interval and but very small F statics , as we also can see almost no corollation @ Q1b (something like 0.1)

#b) Fit a simple linear model to predict Happiness using Region. 
#   How are the results of the fit relate to the results from Q2b?

lmRegion <- lm(data=world, formula=Happiness~Region)
lmRegioncCoef <- coef(lmRegion)
(as.data.frame(lmRegioncCoef)->lmRegioncCoefDF)
lmRegioncCoefDF$Region = row.names(lmRegioncCoefDF)
arrange(lmRegioncCoefDF,desc(lmRegioncCoef))

# when ordering the model coefficients it is equivalent to the region mean happiness order


#c) Use a model selection algorithm of your choice and find the best model to predict
#   Happiness using a combination of predictor variables (not including Country):
  
#install.packages("leaps")
library (leaps)

rss <- regsubsets(Happiness~.-Country,data=world,method="exhaustive",nvmax=15,nbest=1, really.big=T)
rss.sum <- summary(rss)

plot(rss,scale="adjr2",col=c("blue","green","red"))
plot(rss,scale="Cp",col=c("blue","green","red"))

ggplot() + geom_line(aes(x=1:15,y=rss.sum$adjr2)) +
  geom_point(
    aes(x=which.max(rss.sum$adjr2),y=max(rss.sum$adjr2)),col="red",size=3) +
  labs(x='subset size',y='adjusted R^2')

ggplot() + geom_line(aes(x=1:15,y=rss.sum$cp)) +
  geom_point(
    aes(x=which.min(rss.sum$cp),y=min(rss.sum$cp)),col="red",size=3) +
  labs(x='subset size',y='Cp')

ggplot() + geom_line(aes(x=1:15,y=rss.sum$bic)) +
  geom_point(
    aes(x=which.min(rss.sum$bic),y=min(rss.sum$bic)),col="red",size=3) +
  labs(x='subset size',y='bic')


#• (i) What is the model you obtained and how well does it fit the data?
# ill choose one of these models (6,7,8,12):
print(c(
list("model Coef"=coef(rss,7)),
list("BIC"=rss.sum$bic[[7]]),
list("AdjR"=rss.sum$adjr2[[7]]),
list("CP"=rss.sum$cp[[7]]),
list("RSQ"=rss.sum$rsq[[7]]),
list("RSS"=rss.sum$rss[[7]])
))

#• (ii) Using the model fit - what would be the predicted happiness score, including
#       prediction intervals, for a country with Switzerland stats if it were located in the
#       Middle East?
rss
coef(rss,7)

names(coef(rss,7))
coef(rss,7)
newWorld<-world
newWorld$RegionEasternAsia = 0
newWorld$RegionEasternAsia[newWorld$Region=="Eastern Asia"] <- 1
newWorld$LatinAmericaandCaribbean = 0
newWorld$LatinAmericaandCaribbean[newWorld$Region=="Latin America and Caribbean"] <- 1
summary(newWorld)
coefTryModel = lm(data = newWorld,formula =Happiness~.-Country-Region-Generosity)

(Switzerland=filter(.data = world,Country=="Switzerland"))
Switzerland$Region ="Middle East and Northern Africa"
Switzerland$Happiness=NULL
Switzerland$Generosity=0
Switzerland$RegionEasternAsia=0
Switzerland$LatinAmericaandCaribbean=0
data.frame(Switzerland)
predict.lm(coefTryModel,as.data.frame(Switzerland))

#0• (iii) Defining the optimism level of a country as the difference between its obtained
# happiness score and its predicted happiness score - create a vector of the countries
# ordered according to their optimism level from the most optimistic to the least
# optimistic (you can use the order() function). What are the top-three and bottomthree
# countries in terms of optimism? Create a figure, with a boxplot describing
# the distribution of optimism in each region, ordered according to the mean value.
# Which regions seems to be the most optimistic and least optimistic on average?
newWorld$prediction = predict.lm(coefTryModel,newWorld)
newWorld$score = newWorld$Happiness - newWorld$prediction
summary(newWorld)
detach()
attach(newWorld)
 
  as.data.frame(
  select(
    arrange(group_by(newWorld, Region),desc(score))
    ,Country,score,Region))-> optimismdata


arrange(
  summarise(
    group_by(newWorld, Region),
    count = n(),
    Groupscore = mean(score, na.rm = TRUE))
  ,desc(Groupscore)) ->Regionworld

arrange(Regionworld,Groupscore)->tags
orderOptimisdata = arrange(merge(Regionworld,optimismdata,by ="Region" ),desc(Groupscore))
boxplot(orderOptimisdata$score~orderOptimisdata$Groupscore,names=tags$Region)

# What are the top-three and bottomthree
# countries in terms of optimism?
(
  select(
    filter(
      mutate(
        optimismdata
        ,optim_Rank=c(1:nrow(newWorld)))
      ,optim_Rank<4|optim_Rank>nrow(newWorld)-3)
    ,Country,optim_Rank,score)
)

# Which regions seems to be the most optimistic and least optimistic on average?
Regionworld
# best is  North America , worst is Southeastern Asia

#(iv) Using plot() with the fitted model and the argument which=1, find the countries
#     whose obtained happiness score is an outlier to the model fit. Categorize them to
#     optimistic and pesimistic countries according to the defintion from (iii).

plot(coefTryModel, which=1)
outlierTreshole = 2*sd(optimismdata$score)

  outliers<-select(
    filter(
        optimismdata
      ,score<outlierTreshole*-1|score>outlierTreshole)
    ,Country,score)
  outliers$category[outliers$score>0]="optimistic"
  outliers$category[outliers$score<0]="pesimistic"
  outliers
  
# • (v) Using plot() with the fitted model and the argument which=4, find the countries
#  with unusual influence on the fit.
  
  plot(coefTryModel, which=4)
  modelCD<- as.data.frame(cooks.distance(coefTryModel))
  modelCD$index =1:nrow(modelCD)
  newWorld$index=1:nrow(newWorld)
  cookdistanceDF= merge(modelCD,newWorld,by="index")
  colnames(cookdistanceDF)[which(names(cookdistanceDF) == "cooks.distance(coefTryModel)")] <- "cookDist"
  filter(
  arrange(
    select(cookdistanceDF,Country,cookDist,score),
    desc(cookDist)),
  cookDist>4.266085e-02)
  
# • (vi) Can you explain the differences, if any, between the results you obtained in (iv)
#  and in (v)?
  
  #high leverge point - with high cook distance may also be
  #outlier since high leverage in one point implies that the point is very different then other points and by this changing the model to his favor.
  #the differences between the results exist since 
  #Cook's distance measures the effect of deleting a given observation 
  #while outlier is an observation point that is distant from other observations
  