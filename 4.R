##Load Training dataset 
blogData_train <- read.csv("C:/Users/vanwu/Desktop/INFO 7390 ADS/Midterm/BlogFeedback/blogData_train.csv", header=FALSE)

##Load testing dataset
temp1 <- list.files(path="C:/Users/vanwu/Desktop/INFO 7390 ADS/Midterm/BlogFeedback/test/", pattern="*.csv")
blogData_test <-lapply(temp1, read.delim)
blogData_test <- read.csv(file = "C:/Users/vanwu/Desktop/INFO 7390 ADS/Midterm/BlogFeedback/test/blogData_test-2012.02.01.00_00.csv", header = FALSE)


#Training LR
lmFit <- lm(V281 ~ ., data = blogData_train)
summary(lmFit)

#Testing LR
rmseLm <- rmse(predict(lmFit,blogData_test),blogData_test$V281)
rmseLm

#Training CART
library(rpart)
cartFit <- rpart(V281 ~ ., method="anova", data=blogData_train)
#Visualize the tree
plot(cartFit, uniform=TRUE, main="Regression Tree")
text(cartFit, use.n=TRUE, all=TRUE, cex=.8)

#Test CART
rmseCart <-rmse(predict(cartFit,blogData_test),blogData_test$V281)
rmseCart

#Training Random forest
install.packages("randomForest")
library(randomForest)
rffit <- randomForest(V281 ~ ., data = blogData_train, ntree = 10, mtry = 10  ,importance=TRUE, na.action = na.omit)
summary(rffit)

#Test Random forest
rmseRF <-rmse(predict(rffit,blogData_test),blogData_test$V281)
rmseRF
