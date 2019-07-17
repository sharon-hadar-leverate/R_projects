#In this exercise you will implement the K-nearest-neighbor (KNN) method for regression.
#Let d(a, b) = qPpi=1 (ai ??? bi)2 be the Euclidean distance between points a and b. 
#Assume we have a training set of x1, ..., xn points (observations with p ??? 1 dimensions) and
#outcome values y1, ..., yn, and we are given x0 as a new test point. Define di(x0) =
#  d(x0, xi) as the distance between x0 and each point xi. Let d(i)(x0) be the distance
#between x0 and the i’th nearest point to x0 (i.e., d(1)(x0) is the distance between x0 and
#the nearest point, d(2)(x0) is the distance between x0 and the second nearest point. . . ).
#The set of K-nearest neighbors of x0 is given by Nk(x0) = {xi: di(x0) ??? d(K)(x0)}. 
#The KNN estimator of y0 is given by yˆ0 =  1KPi:xi???Nk(x0) yi.

#Create a function in R that calculates the KNN estimator at a point x0. The function
#should get as input the points (matrix of size n ª p), the outcomes (n ª 1), the new
#point x0 (1 ª p) and the number K. The output should be the estimate for y0.

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
  returnValue = matrix(0, nrow = nrow(points), ncol = 1)
  numberOfRow = nrow(points)
  for(index in 1:numberOfRow)
  {
    returnValue[index,1] =calcDistance(points[index, ],newpoint)
  }
  returnValue<-cbind(returnValue,outcomes)
  return (returnValue)
}

KNNestimator <- function(points,outcomes,newpoint,K)
{
  data <- GetDistanceMatrix(points,newpoint,outcomes)
  firstk <- head(data[order(data[,1]),], k)
  return (firstk)
}




sizeFactor = 10
(points=matrix(1:sizeFactor, nrow = sizeFactor, ncol = sizeFactor))
(newpoint = c(1:sizeFactor)) 
(outcome = c(1:sizeFactor)) 
(k=4)
KNNestimator(points,outcome,newpoint,k)

