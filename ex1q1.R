mean_and_sd <- function(x,use.sum=T) 
  {
  if(is.matrix(x)){
   x<- as.vector(x)
  }
  if(use.sum){
    xmean = sum(x)/length(x)
    xsd = sqrt(sum((x-xmean)^2)/(length(x)-1))
      }
  else
    {
    xlength = 0
    xsum = 0
   for(cell in x)
     {
      xlength<-xlength+1
      xsum <- xsum+cell
   }
    xmean = xsum/xlength
    xsqrt=0
    for(cell in x)
      {
      xsqrt <- xsqrt + (cell-xmean)^2
    }
    xsd = sqrt(xsqrt/(xlength-1))
  }
  return (list(mean = xmean, sd = xsd))
  } 

x= c(1:30)
x
x=matrix(1:1000, nrow = 1000, ncol = 1000)
dim(x)
mean_and_sd(x,F)

system.time(mean_and_sd(x,use.sum=F))["elapsed"]
system.time(mean_and_sd(x,use.sum=T))["elapsed"]


x=matrix(rnorm(1000, mean = 5, sd = 10), nrow = 100, ncol = 10)
y=matrix(rnorm(100000, mean = 5, sd = 10), nrow = 10000, ncol = 10)
apply(x,2,mean_and_sd)
apply(y,2,mean_and_sd)


word <- read.csv('./2016 World Happiness Report.csv',header=T)
word
attach(word)
names(word)
summary(word)
nrow(word)

sapply(word, class)

Region
length(unique(Region))

install.packages("tidyverse")

install.packages("tidyr")


?ggplot2
install.packages("ggplot2")


library("ggplot2")
ggplot2(word)
x <- seq(-4*pi,4*pi,length=100)
y <- sin(x) 
# plot x vs. sin(x) using base R graphics 
plot(x,y,type='l',col='red',ylab='sin(x)') 
title(main='base R figure') 
# plot x vs. sin(x) using ggplot 
ggplot() + 
  geom_line(mapping=aes(x=x,y=y),col='red') + 
  labs(y='sin(x)',title='ggplot figure')

attach(word)
word[order(word$Generosity),]
names(word)
ggplot() + 
  geom_point(mapping=aes(x=Generosity,y=Happiness,  color= Region))+
  geom_smooth(mapping = aes(x = Generosity, y = Happiness), method='lm')

ggplot() + 
  geom_point(mapping=aes(x=Trust,y=Happiness,  color= Region))+
  geom_smooth(mapping = aes(x = Trust, y = Happiness), method='lm')

ggplot() + 
  geom_point(mapping=aes(x=Freedom,y=Happiness,  color= Region))+
  geom_smooth(mapping = aes(x = Freedom, y = Happiness), method='lm')


ggplot() + 
  geom_point(mapping=aes(x=Health,y=Happiness,  color= Region))+
  geom_smooth(mapping = aes(x = Health, y = Happiness), method=';m')

ggplot() + 
  geom_point(mapping=aes(x=Family,y=Happiness,  color= Region))+
  geom_smooth(mapping = aes(x = Family, y = Happiness), method='lm')

ggplot() + 
  geom_point(mapping=aes(x=Economy,y=Happiness,  color= Region))+
  geom_smooth(mapping = aes(x = Economy, y = Happiness), method='lm')



?orderTrust

+ 
  labs(y='sin(x)',title='ggplot figure')


word[order(word$Generosity),]


install.packages("GGaly")
GGaly
install.packages("GGally")
library("GGally")
?ggpairs
word
names(word)
subword <- word[c("Happiness","Economy","Family","Health","Freedom","Trust","Generosity")]
ggpairs(subword)


ggplot(word) +
  geom_histogram(mapping=aes(x=Region), binwidth=1000, center=500)

ggplot(word) +
  geom_bar(mapping = aes(x = Region, fill = cut))



regionNcuntry <- word[c("Region","Country")]

regionNcuntryD<-unique(regionNcuntry)

regionNcuntryD <- table(regionNcuntry$Region)
regionNcuntryD<-data.frame(regionNcuntryD)


ggplot(regionNcuntryD) +
  geom_bar(mapping = aes(x = regionNcuntryD$Var1, y = regionNcuntryD$Freq), stat = "identity")+ coord_flip()

aggregate(data$age, by=list(data$group), FUN=mean)[2]
?aggregate()



aggregate(regionNcuntry, by=list(regionNcuntry$Region), FUN=count)

ggplot(quakes.agg, aes(x=Region, y=mag)) + geom_bar(stat="identity")


HappinessByRegion <- word[c("Region","Happiness")]

ggplot(HappinessByRegion) +
  geom_boxplot(mapping=aes(x=reorder(HappinessByRegion$Region,HappinessByRegion$Happiness,FUN=median), y=HappinessByRegion$Happiness), outlier.color='hotpink')

X <- seq(0,10,0.5)
f <- 2*X+5
set.seed(123)
Y <- f+rnorm(length(X),0,5)
(g <- ggplot() +
    geom_line(mapping=aes(x=X,y=f)) +
    geom_point(mapping=aes(x=X,y=Y), color="red") +
    labs(x='x',y='y'))


set.seed(123)
n <- 100
X1 <- runif(n,0,40); X2 <- runif(n,0,20);
Y <- -10 + 2*X1 + 5*X2 + 7*X1*X2 + rnorm(n,0,50)
df5 <- data.frame(x1=X1,x2=X2,y=Y)
scatterplot3d::scatterplot3d(df5,angle=30,color="red",mar=rep(3,4))
install.packages("scatterplot3d", repos="http://R-Forge.R-project.org")



X = seq(0, 3.5 * pi, length = 50)
Y = 4 * sin(X) + rnorm(length(X))
df7 <- data.frame(x=X,y=Y)
poly_1 <- lm(data=df7, y ~ poly(x,1))
poly_2 <- lm(data=df7, y ~ poly(x,2))
poly_3 <- lm(data=df7, y ~ poly(x,3))
poly_4 <- lm(data=df7, y ~ poly(x,4))
poly_5 <- lm(data=df7, y ~ poly(x,5))
ns_1 <- lm(data=df7, y ~ splines::ns(x,1))
ns_2 <- lm(data=df7, y ~ splines::ns(x,2))
ns_3 <- lm(data=df7, y ~ splines::ns(x,3))
ns_4 <- lm(data=df7, y ~ splines::ns(x,4))
ns_5 <- lm(data=df7, y ~ splines::ns(x,5))
ggplot() + geom_point(aes(x=X,y=Y))

df7.fit <- data.frame(x=X, y=Y,
                      fit.poly_1=fitted(poly_1),
                      fit.poly_2=fitted(poly_2),
                      fit.poly_3=fitted(poly_3),
                      fit.poly_4=fitted(poly_4),
                      fit.poly_5=fitted(poly_5),
                      fit.ns_1=fitted(ns_1),
                      fit.ns_2=fitted(ns_2),
                      fit.ns_3=fitted(ns_3),
                      fit.ns_4=fitted(ns_4),
                      fit.ns_5=fitted(ns_5))
df7.fit.long <-df7.fit %>%
  gather(key='model',value='fit',c(-x,-y)) %>%
  separate(col=model,into=c('model','terms_num'),sep='_')

library("tidyr")


ggplot(data=df7.fit.long) +
  geom_point(mapping=aes(x=x,y=y)) +
  geom_line(data=df7.fit.long, mapping=aes(x=x,y=fit,color=model)) +
  facet_wrap(~terms_num,nrow=1)

install.packages('ISLR')
library(ISLR)
attach(Default)
summary(Default)


ggplot(data=Default) +
  geom_point(
    mapping=aes(x=balance,y=income,color=default,shape=default,alpha=default)) +
  scale_color_manual(values = c("blue", "orange")) +
  scale_shape_manual(values = c(1,3)) +
  scale_alpha_manual(values = c(0.3,1)) +
  labs(x="Balace",y="Income")

glm.fit1=glm(default~balance,data=Default,family=binomial); summary(glm.fit1)

Confounding in the Default Data
Default.fit <- data.frame(Default,fit=fitted(glm.fit3))
Default.fit <- Default.fit %>% group_by(student) %>% mutate(mean_fit=mean(fit))
ggplot(data=Default.fit) +
  geom_line(mapping=aes(x=balance,y=fit,color=student)) +
  geom_line(mapping=aes(x=balance,y=mean_fit,color=student),linetype="dashed") +
  scale_color_manual(values = c("blue", "orange")) +
  labs(x="Credit Card Balance",y="Default Rate")
ggplot(data=Default) +
  geom_boxplot(mapping=aes(x=student,y=balance,fill=student),show.legend=F) +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(x="Student Status",y="Credit Card Balance")


