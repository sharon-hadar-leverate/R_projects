#In this exercise you will implement the K-nearest-neighbor (KNN) method for regression.
#Let d(a, b) = qPpi=1 (ai − bi)2 be the Euclidean distance between points a and b. 
#Assume we have a training set of x1, ..., xn points (observations with p ≥ 1 dimensions) and
#outcome values y1, ..., yn, and we are given x0 as a new test point. Define di(x0) =
#  d(x0, xi) as the distance between x0 and each point xi. Let d(i)(x0) be the distance
#between x0 and the i’th nearest point to x0 (i.e., d(1)(x0) is the distance between x0 and
#the nearest point, d(2)(x0) is the distance between x0 and the second nearest point. . . ).
#The set of K-nearest neighbors of x0 is given by Nk(x0) = {xi: di(x0) ≤ d(K)(x0)}. 
#The KNN estimator of y0 is given by yˆ0 =  1KPi:xi∈Nk(x0) yi.

#Create a function in R that calculates the KNN estimator at a point x0. The function
#should get as input the points (matrix of size n × p), the outcomes (n × 1), the new
#point x0 (1 × p) and the number K. The output should be the estimate for y0.

calcDistance <- function(a,b)
{
  if(length(a)!=length(b))
  {
    return (NULL)
  }
  vac = (a-b)^2
  value <- sum(vac)
  return (sqrt(value))
}

GetDistanceMatrix <- function(points,newpoint,outcomes)
{
  numberOfRow = nrow(points)
  returnValue = matrix(0, nrow = numberOfRow, ncol = 1)
  for(index in 1:numberOfRow)
  {
    ans = calcDistance(points[index, ],newpoint)
    returnValue[index,1] =ans
    if(is.null(ans))
    {
      print("error while calc distance")
    }
  }
  return (data.frame(returnValue,outcomes))
}


KNNestimator <- function(points,outcomes,newpoint,knumber)
{
  data <- GetDistanceMatrix(points,newpoint,outcomes)
  head <- head(data[order(data[,1]),], knumber)
  
  if(is.character(outcomes))
  {
    slice(
      arrange(
        count(head, vars = outcomes)
        ,desc(n))
      , 1 )  -> firstk
    return (firstk[1,1]$vars)
  }
  else if(is.double(outcomes))
  {
    return(sum(head$outcomes)/knumber)
  }
  else
  {
    return("Dont support outcome type")
  }
}


sizeFactor = 50
k=50
propvec = c(0.25, 0.25, 0.25, 0.25)
(points=matrix(rexp(sizeFactor*sizeFactor), sizeFactor))
newpoint = rexp(sizeFactor)
characterOutcomes = sample( LETTERS[1:4], sizeFactor, replace=TRUE, prob=propvec)  
doubleOutcomes = rexp(sizeFactor)  

(KNNestimator(points,characterOutcomes,newpoint,k))
(KNNestimator(points,doubleOutcomes,newpoint,k))

#Generate x and y using the setup from question 1 with n = 100. Calculate the KNN
#estimator with K = 1, 5, 9 for the points x0 = x1, ..., xn (i.e., for each point xi, 
#estimate yi using all the points in the training set). 
#For each K, create a ggplot with the true function (yi without the noise term - 
#drawn using a black line), the observations (yi -drawn using black circles) and the KNN estimators (yˆi - drawn using a red line).

Xvector <- function(n) 
{
  return (seq(0, 10, length.out = n)) 
} 

Yvector <- function(x)
{
  set.seed(123)
  epsilon = rnorm(n=length(x), mean = 0, sd = sqrt(3))
  y = 2 + 1*x + epsilon
  return (y)
}
library(tidyr)

n=100

xdata = as.data.frame(Xvector(n))
ydata = Yvector(Xvector(n))
ytrend =  2 + 1*Xvector(n)

returnValue <- data.frame(x= xdata, y= ydata, ytrend,est_y_k1=numeric(n),est_y_k5=numeric(n),est_y_k9=numeric(n))
for(row in 1:nrow(returnValue))
{
  x0<-  returnValue[row,'Xvector.n.']
  returnValue[row,'est_y_k1']<-KNNestimator(xdata,ydata,x0,1)
  returnValue[row,'est_y_k5']<-KNNestimator(xdata,ydata,x0,5)
  returnValue[row,'est_y_k9']<-KNNestimator(xdata,ydata,x0,9)
}
colnames(returnValue) <- c("X", "Y", "Trend", "k1", "k5", "k9")
head(returnValue)

gatheredValues = gather(data = returnValue,key = K, value = estimation,... = k1:k9,factor_key = TRUE)

ggplot(data =gatheredValues)+
  geom_point(mapping = aes(X,estimation))+
  geom_smooth(mapping = aes(x = gatheredValues$X, y = gatheredValues$Trend), method='lm')+
  facet_wrap( ~K)

library("ggplot2")
ggplot(data =gatheredValues)+ 
  geom_point(mapping = aes(X,Y))+ 
  geom_smooth(mapping = aes(x = gatheredValues$X, y = gatheredValues$Trend), method='lm')+ 
  geom_smooth(mapping = aes(x = gatheredValues$X, y = gatheredValues$estimation), method='lm',color='red')+ 
  facet_wrap( ~K)


#c) Explain how would you go about selecting a value for K to use for prediction using the
#training set.

#Select the biggest k possible