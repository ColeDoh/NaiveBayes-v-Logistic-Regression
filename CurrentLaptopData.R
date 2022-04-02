LaptopData <- read.csv("Laptop.csv", stringsAsFactors=T)


#naivebayes

indexes<-sample(2, nrow(LaptopData), replace=T, prob=c(0.8, 0.2))
train<-LaptopData[indexes==1,]
test<-LaptopData[indexes==2,]
library(naivebayes)
library(dplyr)


model<-naive_bayes(Brand ~ Price + Rating + Inches, data=train, usekernel = T)
plot(model)

p<-predict(model, test)
table(p, test$Brand)


#logisticregression
LaptopData <- read.csv("Laptop.csv", stringsAsFactors=T)

glm.fit<-glm (Brand ~ Price + Rating + Inches, data = train, family = 'binomial')
summary(glm.fit)

glm.probs<- predict(glm.fit, test, type = "response")
glm.probs[1:5]

glm.pred<- ifelse(glm.probs > 0.5, "APPLE", "DELL")
attach(test)
table(glm.pred,Brand)


#logistic regression plot
LaptopData %>%

ggplot(aes(x=Brand, y=Price, fill=Brand))+
  geom_boxplot() +
  ggtitle("Box Plot")


#naiveBayes
LaptopData %>%
  
ggplot(aes(x=Price, fill=Brand))+
  geom_density(alpha = 0.8, color = 'black') +
  ggtitle("Density Plot")



