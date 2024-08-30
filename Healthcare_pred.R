---
  title: "IST687FinalProject"
output: html_document
date: "2023-12-08"
---
  
  #libraries
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)
library(e1071)
library(kernlab)
library(arules)
library(arulesViz)
library(maps)
library(ggmap)
library(mapproj)
library(rworldmap)
library(rpart)
library("zoo")


#saving the data
datafile <- "https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv"

RawhosData <-  read.csv(datafile)
view(RawhosData)

dim(RawhosData)
glimpse(RawhosData)

str(RawhosData)
summary(RawhosData)

sapply(RawhosData,function(x) sum(is.null(x))) 

sapply(RawhosData,function(x) sum(is.na(x)))




#check for NA values in columns

table(is.na(RawhosData$X))
table(is.na(RawhosData$age))
table(is.na(RawhosData$bmi))
table(is.na(RawhosData$children))
table(is.na(RawhosData$smoker))
table(is.na(RawhosData$location))
table(is.na(RawhosData$location_type))
table(is.na(RawhosData$education_level))
table(is.na(RawhosData$yearly_physical))
table(is.na(RawhosData$exercise))
table(is.na(RawhosData$married))
table(is.na(RawhosData$hypertension))
table(is.na(RawhosData$gender))
table(is.na(RawhosData$cost))


#converting na values into 0 for bmi column
which(is.na(RawhosData$bmi), arr.ind=TRUE)

RawhosData$bmi <- na.approx(RawhosData$bmi)
table(is.na(RawhosData$bmi))

#removing the na values from hypertension
RawhosData <- RawhosData[complete.cases(RawhosData), ]



#creating a column named expensive based on the 3rd quartile value from the cost column
qval <- quantile(RawhosData$cost, probs = c(0.75))
qval
RawhosData$expensive <- ifelse(RawhosData$cost > qval, 0, 1)

table(RawhosData$expensive)

#creating a column AgeCategory for visualisation purpose
RawhosData$AgeCategory[RawhosData$age < 18] <- "child"
RawhosData$AgeCategory[18<=RawhosData$age & RawhosData$age<=25] <- "young-adults"
RawhosData$AgeCategory[25<RawhosData$age & RawhosData$age<=40] <- "Adults"
RawhosData$AgeCategory[40<RawhosData$age & RawhosData$age<=59] <- "middle-aged"
RawhosData$AgeCategory[59<RawhosData$age] <- "older-adults"

#creating another column based on bmi
RawhosData$bmi <- round(RawhosData$bmi, digits= 1)
RawhosData$bmicat[RawhosData$bmi < 18.5] <- "under_weight"
RawhosData$bmicat[18.5<=RawhosData$bmi & RawhosData$bmi<=25] <- "normal_weight"
RawhosData$bmicat[25<RawhosData$bmi & RawhosData$bmi<=30] <- "over_weight"
RawhosData$bmicat[30<RawhosData$bmi] <- "obese"

Data <- RawhosData

#maps
#US Map
#load ggplot2 library
library(ggplot2) 
#load maps library
#install.packages("map")
library(maps)
#load ggmap library
#install.packages("ggmap")
library(ggmap)
#load the mapproj library
#install.packages("mapproj")
library(mapproj)
newDF <- Data %>% group_by(location) %>% summarise(cost)
us<- map_data("state")
us$state_name <- tolower(us$region)
newDF$location <- tolower(newDF$location)
str(us)
farewithgeom <- merge(us,newDF,all.x=TRUE,by.y="location",by.x="state_name")
#structure of the popwithgeom
str(farewithgeom)
bb <- c(left = min(us$long), bottom = min(us$lat),right = max(us$long), top = max(us$lat))
map <- get_stamenmap(bbox = bb, zoom=5)
ggmap(map) + geom_polygon(data=farewithgeom,color="black",
                          alpha=0.9,aes(x=long,y=lat,group=group,fill=cost))

#map only for the states in the data
#map for the cost
table(Data$location)
#install.packages("map")
library(maps)
#load ggmap library
#install.packages("ggmap")
library(ggmap)
#load the mapproj library
#install.packages("mapproj")
library(mapproj)
newDF <- Data %>% group_by(location) %>% summarise(cost)
newDF$location <- tolower(newDF$location)
counties <- map_data("county", c("connecticut","maryland","masschusetts","new jersey","new york","pennsylvania","rhode island"))
counties$region <- tolower(counties$region)
farewithgeom <- merge(counties,newDF,all.x=TRUE,by.y="location",by.x="region")
bb <- c(left = min(counties$long), bottom = min(counties$lat),right = max(counties$long), top = max(counties$lat))
map <- get_stamenmap(bbox = bb, zoom=5)
ggmap(map) + geom_polygon(data=farewithgeom,color="black",
                          alpha=0.9,aes(x=long,y=lat,group=group,fill=cost))

#map for the bmi

library(ggplot2) 
library(maps)
library(ggmap)
library(mapproj)
bmidf <- Data %>% group_by(location) %>% summarise(bmi)
bmidf$location <- tolower(bmidf$location)
farewithgeom <- merge(counties,bmidf,all.x=TRUE,by.y="location",by.x="region")
bb <- c(left = min(counties$long), bottom = min(counties$lat),right = max(counties$long), top = max(counties$lat))
map <- get_stamenmap(bbox = bb, zoom=5)
ggmap(map) + geom_polygon(data=farewithgeom,color="black",
                          alpha=0.9,aes(x=long,y=lat,group=group,fill=bmi)) +  scale_fill_viridis_c(option = "plasma", trans = "sqrt")


#box plot
sampdata <- Data
boxplot(bmi~hypertension,
        data=sampdata,
        main="bmi vs hypertension",
        xlab="hypertension",
        ylab="bmi",
        col="blue",
        border="black"
)

sampdata$smoker <- as.factor(sampdata$smoker)
boxplot(cost~smoker,
        data=sampdata,
        main="Cost  Vs Smoker",
        xlab="smoker",
        ylab="cost",
        col="orange",
        border="brown"
)

#pie chart
pidata <- Data %>% group_by(AgeCategory) %>% summarise(mean(cost))
colnames(pidata)[2] <- 'mcost'
library(ggplot2)
bp<- ggplot(pidata, aes(x="", y=mcost, fill=AgeCategory))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie

bmicdata <- Data %>% group_by(bmicat) %>% summarise(mean(cost))
colnames(bmicdata)[2] <- 'mcost'
library(ggplot2)
bp<- ggplot(bmicdata, aes(x="", y=mcost, fill=bmicat))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie +  scale_fill_brewer("Blues")

#scatter plot
sampdata$location_type <- as.factor(sampdata$location_type)
sampdata$AgeCategory <- as.factor(sampdata$AgeCategory)
scaterdata <- Data %>% group_by(age) %>% summarise(mean(cost))
colnames(scaterdata)[2] <- 'mean_cost'

ggplot(scaterdata, aes(x=age, y=mean_cost)) +
  geom_point()  + geom_smooth(method="lm", se=FALSE) 

#barchart
bardata <- Data %>% group_by(AgeCategory) %>% summarise(hypertension)
colnames(bardata)[2] <- 'hypertension'
counts <- table(bardata$hypertension, bardata$AgeCategory)
barplot(counts, main="hypertension vs Agecategory",
        xlab="AgeCategory", col=c("darkblue","red"),
        legend = rownames(counts))

#line chart
age_summary <- Data %>% 
  group_by(age, gender) %>% 
  summarise(count = n(),
            high_cost = sum(cost > qval),
            high_cost_per = high_cost/count)

age_summary %>% ggplot(aes(x=age, y=high_cost_per, color = gender)) + 
  geom_line(size = 2) +
  theme_minimal() + geom_smooth(alpha = 0.05, fill = "black") +
  ylab("Percentage of High Cost Customers") + xlab("Age") + 
  ggtitle("Percentage of High Cost Customers By Age and Gender")
#barchart
qval <- quantile(Data$cost, probs = c(0.75))

smoker_summary <- Data %>% group_by(smoker) %>%
  summarise(
    count = n(),
    high_cost = sum(ifelse(cost > qval, 1, 0)),
    high_cost_per = high_cost/count
  )

smoker_summary %>% ggplot(aes(x=smoker, y=high_cost_per, fill = smoker)) + geom_bar(stat = "identity") + 
  ylim(0,1) +
  ggtitle("Percentage of Smokers and Non-Smokers Who Are High Cost") +
  ylab("Percentage of High Cost Customers") +
  xlab("Smoker")

#Modelling
#cart
tdata <- RawhosData[,c(2,3,4,5,6,7,8,9,10,11,12,13,15)]
set.seed(1)
trainListone=createDataPartition(y=tdata$expensive,p=.40,list=FALSE)
trainSetone=tdata[trainListone,]
testSetone=tdata[-trainListone,]
library(rpart)
library(rpart.plot)
cartTree=rpart(expensive~.,data=trainSetone)
cartTree
prp(cartTree,faclen=0,cex=0.8,extra=1)

varImp(cartTree)

#predicting data
RawhosData$expensive<-as.factor(RawhosData$expensive)
tdata <- data.frame(age = RawhosData$age,
                    smoker = RawhosData$smoker,
                    hyper = RawhosData$hypertension,
                    expensive = RawhosData$expensive,
                    exr = RawhosData$exercise,
                    chld = RawhosData$children,
                    bmi = RawhosData$bmi)

library(caret)
library(kernlab)
trainList <- createDataPartition(y=tdata$expensive,p=.40,list=FALSE)
trainSet <- tdata[trainList,]
str(trainSet)
testSet <- tdata[-trainList,]
str(testSet)

#linear model
lmcost1 <- lm( data = tdata, expensive ~ smoker  + exr + age + bmi + chld + hyper)
lmcost1
summary(lmcost1)

#random forest
library(kernlab)
trctrl <- trainControl(method = "repeatedcv", number =10, repeats = 3)
svmModel <- train(expensive~ ., data = trainSet,trControl=trctrl, preProcess = c("center","scale"))
svmModel
Predout <- predict(svmModel, newdata = testSet)
table(Predout)
confusionMatrix(Predout, testSet$expensive)

#glm
model <- glm(expensive ~.,family=binomial(),data=trainSet)
predict_reg <- predict(model, 
                       testSet, type = "response")
head(predict_reg)
predict_reg <- round(predict_reg, digits = 1)
predict_reg <- ifelse(predict_reg >0.5, 1, 0)
head(predict_reg)
confusionMatrix(as.factor(predict_reg),testSet$expensive)

#svm model
library(kernlab)
svm<- ksvm(expensive~ ., data=trainSet, C=5, cross=3, prob.model=TRUE)
#svmModel<- ksvm(low_cost~trainset$bmi., data=trainset, C=5, cross=3, prob.model=TRUE)
svm
predoutsvm <- predict(svm, newdata= testSet, type= "response")
confusionMatrix(predoutsvm, testSet$expensive)


#association rules
library(arules)
library(arulesViz)

mydata <- Data
summary(mydata)
dataX <- as(mydata,"transactions")

freq <- itemFrequency(dataX)
freq
itemFrequencyPlot(dataX, topN = 10)
inspect(dataX[1:10])

ruleset <- apriori(dataX,
                   parameter=list(supp=0.005, conf=0.3),
                   control=list(verbose=F),
                   appearance=list(default="lhs",rhs=("expensive=1")))
head(inspect(ruleset)
     
     