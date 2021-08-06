#INSTALLING THE REQUIRED PACKAGES
install.packages("neuralnet")
library(neuralnet)

#Loading the dataset
file <- read.csv(file.choose())

#Normalizing the dataset
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
data <- as.data.frame(lapply(file,normalize))
data['TEY'] <- file[8]

#Splitting the dataframe into training nad testing dataset
ind <- sample(2,nrow(file),replace=T,prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]


#Model1 with Rectified Linear Activation function
model1 <- neuralnet(TEY~.,data = train)
#Model Visualization
plot(model1)
#Computing the model result
result <- compute(model1,test)
pred1 <- result$net.result
#Checking the model accuracy
cor(test$TEY,pred1)


##Model2 with Hyperbolic Tangent Activation function and backpropagation algorithm
model2 <- neuralnet(TEY~.,data=train, hidden = c(4,4),algorithm = 'backprop',learningrate = 0.001,stepmax = 1e+08,linear.output = F,act.fct = 'tanh')
#Model Visualization
plot(model2)
#Computing the model result
result2 <- compute(model2,test)
pred2 <- result2$net.result
#Checking the model accuracy
cor(test$TEY,pred2)
