hello everyone

library(odbc)
library(dplyr)
library(tidyverse)
library(corrplot)
library(modeldata)
library(Metrics)
library(ggeffects)
library(leaps) 
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-2F3Q272\\SQLEXPRESS",
                 Database = "HomePredictions",
                 Port = 1433)

dbListTables(con)
dbListFields(con, 'train')

testdata <- tbl(con, 'train')
testdata <-collect(testdata)
View(testdata)

num <- unlist(lapply(testdata, is.numeric), use.names = FALSE) 
corrplot(cor(testdata[num]),method = 'number', tl.cex =0.5)


df<- select_if(testdata, is.numeric)
View(df)

df_clean<- na.omit(df)
View(df_clean)

df_clean <- as.data.frame(scale(df_clean))
View(df_clean)



set.seed(1234) 

indexset<- sample(2, nrow(df_clean), replace = T, prob = c(0.8, 0.2))

train <- df_clean[indexset==1,]
View(train)

test<-df_clean[indexset==2,]
View(test)

lapply(df_clean, class)


mymodel<- glm(train$`SalePrice`~., data = train)
summary(mymodel)


library("MASS")

initial<-stepAIC(mymodel)
summary(initial)



predictions<-mymodel %>% predict(test)

compare<- data.frame(actual=test$SalePrice,
                     predicted= predictions)
View(compare)


error<- rmse(predictions, test$SalePrice)


plot(initial)


plot(error)


plot(compare)


plot(x=predictions, y=test$SalePrice, xlab = 'Predicted Values', ylab = 'Actual Values',
     main= 'Predicted vs. Actual Values')
abline(a=0, b=1)
