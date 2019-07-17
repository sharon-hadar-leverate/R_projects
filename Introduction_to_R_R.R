
x<-matrix(1:5,5,5)
y<-t(x)

print(x)
print(y)

a1=x%*%y
a2=y%*%x

print(a1)
print(a2)

a4 = x[2,]+y[,3]
print(a4)

print(x)
print(y)
b1= cbind(x,y)
b2= rbind(x,y)
print(b1)
print(b2)

c1= apply(x,1,prod)
c2 = apply(y,2,sum)

print(c1)
print(c2)

(myList <- list(c(T,F,T), 'abc', matrix(1:4,2,2)))
print(myList[[1]][1])

addr <- list(Name=c("Dan","Ran"), Street=c("Kaplan","Pinkas"), StreetNum=c(144,33));
print(addr$Street)
addr<- c(addr,LastUpdated=date())
print(addr)

addrV<- unlist(addr)
print(addrV[[1]])

myList <- list(1:10,11:20)
sumList <- lapply(myList,sum)
print(myList)
print(sumList)

a1 = list(Bands=c("U2","RS","Funkadelic"),TVSeries=c("Die For a moment","Breaking bad"),number=pi)
print(a1)
names(a1) = c("NewBands","NewTV","Newpi")
a2=unlist(a1$NewBands)
print(a2)
typeof(a1)
a1$NewBands[3]="Neil Young"
print(a1)
a1$NewTV[3] = "Metomtemet"

a1<-a1[-3]



vec1 = c('John','Jim','Jack','Jil','Jane') 
vec2 = c('M','M','M','F','F') 
vec3 = c(80,100,70,65,77) 
vec4 = c(170, 180, 170, 160, 172) 
df <- data.frame(Name=vec1,Sex=vec2,Weight=vec3,Height=vec4)
print(df)

df$BMI <- round(df$Weight/((df$Height/100)^2),2)
df
split(df,df$Sex)
attach(df)
BMI
mat<-as.matrix(df)
mat
df
summary(df)
summary(mat)
df$Name<-as.character(df$Name)
?iris
iris
summary(iris)
?fix
fix(iris)
is.data.frame(iris)
is.list(iris)
is.matrix(iris)
dim(iris)

names(iris)
for(x in names(iris)){
  print(x)
  print(c("numric = ",is.numeric(unlist(iris[x]))))
  print(c("factor = ",is.factor(unlist(iris[x]))))
}
summary(iris)
is.numeric(unlist(iris["Sepal.Length"]))
is.numeric(iris$Sepal.Length)
as.vector()
typeof(iris$Sepal.Length)
typeof(iris["Sepal.Length"])

newIris = iris
dim(newIris)
newIris[,5]
attach(newIris)
newIris[Species=="serosa",6] = "blue"
summary(newIris[,5])

newIris<-newIris[,-6]
if(Species =="serosa") {newIris[,6]= "blue"}
newIris
summary(newIris$Species)

newIris$Color = "red"
newIris$Color[Species=="setosa"] = "blue"
newIris$Color[Species=="versicolor"] = "Green"


sub = subset(newIris,subset=Sepal.Length>5&Sepal.Width>3)
sub

write.table(sub,'./IrisSub.txt',col.names=T,row.names=F,sep='\t')
IrisSub <- read.table('./IrisSub.txt',header=T,sep='\t')
IrisSub

x <- 'b' 
switch(x, 
       'a' = { y <- 10 }, 
       'b' = { y <- 20 }, 
       'c' = { y <- 30 })
y



