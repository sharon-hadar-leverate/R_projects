#title: "Concluding Exercise in R"
#author: "Sharon Hadar"
#date: "09.13.2017"
library(shiny)
library(gridExtra)
Xvector <- function(n) 
{
  return (seq(0, 10, length.out = n)) 
} 

Yvector <- function(x,B0=2,B1=1,Sigma=3)
{
  set.seed(123)
  epsilon = rnorm(n=length(x), mean = 0, sd = sqrt(Sigma))
  y = B0 + B1*x + epsilon
  return (y)
}

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
    return(sum(head$outcomes, na.rm = TRUE)/knumber)
  }
  else
  {
    return("Dont support outcome type")
  }
}

ui <- fluidPage(
   titlePanel("Bonus"),
   sidebarLayout(
      sidebarPanel(
         sliderInput("inputk",
                     "K:",
                     min = 1,
                     max = 100,
                     value = 5),
         numericInput("inputB0", "B0", 2, min = NA, max = NA),
         numericInput("inputB1", "B1", 1, min = NA, max = NA),
         numericInput("inputSigmaSqr", "SigmaSqr", 1, min = NA, max = NA)
      ),
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     xdata = as.data.frame(Xvector(100))
     ydata = Yvector(Xvector(100),input$inputB0,input$inputB1,input$inputSigmaSqr)
     ytrend =  input$inputB0 + input$inputB1*Xvector(100)

     returnValue <- data.frame(x= xdata, y= ydata, ytrend,k=numeric(100))
     colnames(returnValue) <- c("X", "Y", "Trend", "prediction")
     for(row in 1:nrow(returnValue))
     {
       x0<-  returnValue[row,'X']
       returnValue[row,'prediction']<-KNNestimator(xdata,ydata,x0,input$inputk)
     }
    g1<- ggplot(data =returnValue)+
       geom_point(mapping = aes(X,prediction))+
       geom_smooth(mapping = aes(x = X, y = Trend), method='lm')+
       geom_segment(aes(x=X, xend=X, y=Trend, yend=prediction))
     
     g2<-ggplot(data =returnValue)+ 
       geom_point(mapping = aes(X,Y))+ 
       geom_smooth(mapping = aes(x = X, y = Trend), method='lm')+ 
       geom_smooth(mapping = aes(x = X, y = prediction), method='lm',color='red')
     
     grid.arrange(g1,g2, ncol=2)

   })
}

shinyApp(ui = ui, server = server)

