setwd("C:/Users/ranbh/OneDrive/Desktop/r project for submission")

library(readxl)
library(tidyverse)
library(dplyr)
library(caTools)
library(MASS)
library(e1071)
library(caret)
install.packages("corrplot")
library(corrplot)
library(GGally)
install.packages("ISLR")
library(ISLR)

hrt_df<- read.csv("heart.csv")
View(hrt_df)

hrt_DF<-hrt_df %>% rename(age= ï..age)
dim(hrt_DF)

head(hrt_DF)
tail(hrt_DF)
colSums(is.na(hrt_DF))
str(hrt_DF)
summary(hrt_DF)
class(hrt_DF[,c(1:14)])
corr.df<-cor(hrt_DF)
corr.df

cor(hrt_DF$target, hrt_DF$oldpeak)
cor(hrt_DF$target, hrt_DF$chol)

corrplot(corr.df)

ggpairs(hrt_DF[, c(1,3,8,10,11,14)],
        upper = list(continuos = 'Cor'),
        lower = list(continuos = 'Points'),
        diag = list(continuos = 'densityDiag'))+theme_bw()

unique(hrt_DF$age)
summary(as.factor(hrt_DF$age))

unique(hrt_DF$sex)
summary(as.factor(hrt_DF$sex))

unique(hrt_DF$cp)
summary(as.factor(hrt_DF$cp))

unique(hrt_DF$fbs)
summary(as.factor(hrt_DF$fbs))

unique(hrt_DF$restecg)
summary(as.factor(hrt_DF$restecg))

unique(hrt_DF$exang)
summary(as.factor(hrt_DF$exang))

unique(hrt_DF$slope)
summary(as.factor(hrt_DF$slope))

unique(hrt_DF$ca)
summary(as.factor(hrt_DF$ca))

unique(hrt_DF$thal)
summary(as.factor(hrt_DF$thal))

unique(hrt_DF$target)
summary(as.factor(hrt_DF$target))


#### spliting the dataset ####

split<- sample.split(hrt_DF$target, SplitRatio = 0.7)
train<- subset(hrt_DF, split == TRUE)
test<- subset(hrt_DF, split == FALSE)


nrow(train)
nrow(test)

#### Biulding a model of logistic regression ####

model<- glm(target~.,family = binomial(link = 'logit'), data = train)
summary(model)


#### test the model ####s

summary(test)

test_pred<- predict(model, type = 'response', newdata = test)
test_pred
head(test_pred)
test$prob<- test_pred
view(test)


test_pred_target<- factor(ifelse(test_pred>= 0.50, 'Yes','No'))
test_actual_target<- factor(ifelse(test$target==1, 'Yes','No'))
table(test_actual_target,test_pred_target)


test_conf<-confusionMatrix(test_pred_target,test_actual_target, positive ='Yes')
test_conf
sssssSconfusion
















