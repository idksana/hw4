#sana abbasi
#uin: 662260656
#hw4
install.packages("dplyr")
install_github("idksana/dplyr")
library(dplyr)
install.packages("ggplot2")
install_github("idksana/dplyr")
library(ggplot2)
setwd("~/Desktop/hw4")
case = read.csv("Professor Proposes Case Data.csv", fileEncoding="UTF-8-BOM")
str(case)
View(case)

#1a
#checking the price of the diamond 
cor(case$Carat, case$Price)
plot(x = case$Price, y = case$Carat)
case %>% group_by(Colour) %>% summarize( avgCarat = mean(Carat), freq = n())
chisq.test(case$Carat, case$Price, correct = FALSE)

#1b
#outllier
summary(case$Price)
sd(case$Price)
hist(case$Price)
hist(log(case$Price))

boxplot(case$Price ~ case$Carat)
boxplot(case$Carat ~ case$Colour)

outlier = boxplot.stats(case$Price)$out
case1 = case[!(case$Price %in% outlier),]
case2 = case[case$Price <  min(outlier),]

boxplot(case1$Price)
boxplot(case2$Price)

#1c 
#training set
samples = set.seed(123)
indx <- sample(2, nrow(case), replace = T, prob = c(0.7, 0.3))
train <- case[indx == 1, ]
test <- case[indx == 2, ]
summary(train)
summary(test)

#train Min.   :0.0900   Length:316
#test  in.   :0.100   Length:124
# in the training data the minium is lower, with a bigger length than the testing data 

#1d
#train dataset
lmModel <- lm(formula = Price ~ .,data = train)
#1e 
#r^2
summary(lmModel)
# the stars what makes it significant - all the colors, clarity, and cut are signififacnt 
#polish G,IV,X are signifacnt and Wholesaler
#the R-Squared: 0.9825:  adjusted R-squared:  0.9804

#1f
#MAPE training set
predLmModel <- lmModel$fitted.values
error <- predLmModel - train$Price
absError <- abs(error)
percAbsError <- absError / train$Price
mape <- mean(percAbsError) # 0.1007499


#1g
##MAPE testing data
summary(train$Certification) 
summary(test$Certification) 
pred = predict(lmModel, test)
error <- pred - test$Price
absError <- abs(error)
percAbsError <- absError / test$Price
mape <- mean(percAbsError) #1.398526

#1h
install.packages("fastDummies")
library(fastDummies)
install.packages("nnet")
library(nnet)
case2 <- fastDummies::dummy_cols(case)
case2 <- case2 %>% select(Colour,-Clarity,-Cut,-Certification,-Polish,-Symmetry)
case3 = case3 %>% select(Price, Carat:Wholesaler, Colour_D:Symmetry_X)
View(case3)

mins <- apply(case2, 2, min)
maxs <- apply(case2, 2, max)
case3 <- scale(case2, mins, maxs-mins) 
as.numeric(case3)

#in exhibit 2 it shows how they want cut to be very good, color to be J, and clarity to be s12
# i would recommend because its one of the good selections you can get

#2a
# building the neural network(NN) model with R
# first you should get understand the NN by having the histograms. 
#this shows a lot bc visually you can see the data and how you can manipulate the data
#after that you should ample the data which we did in 1c and making a training and testing data
#then NN the data and predict how the model would be. after that compute the training set to see the accuracy of the hidden layers


#2b
install.packages("neuralnet")
library(neuralnet)

train_nn <- case3[samples == 1 ]
test_nn <- case3[samples == 2 ]

nn_model = nnet(Price ~ ., data=train_nn, linout=F, size=10, decay=0.01, maxit=1000)
train_nn_preds <- nn_model$fitted.values
case3 =  as.numeric(case3)
train_nn_preds <- train_nn_preds *   ((maxs[1] - mins[1]) + mins[1])
test_nn_preds = predict(nn_model, test_nn)
test_nn_preds <- test_nn_preds * (maxs[1] - mins[1]) + mins[1]


# MAPE of training data
err <- train_nn_preds - train$charges
abserr <- abs(err)
percabserr <- abserr / train$charges
mape <- mean(percabserr)


# MAPE on testing data
err <- test_nn_preds - test$charges
abserr <- abs(err)
percabserr <- abserr / test$charges
mape <- mean(percabserr)


#2c 
#I would perfer linear regression because you can see see the strength
# like tge R^2, the significants and confidence level
#from q1 you can see the data visually with the histograms and overall its simpler to look at


#2d 
#i would not recommend with the NN model, it does not seem to be worth it if you do purchase that diamond


seq(0,100, 5)
seq(0:100, 5)
seq(0,100, by = 5)

sample(2, case = 1500, prob = c(0.7,0.3))



