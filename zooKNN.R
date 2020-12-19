Zoo <- read.csv(choose.files())
View(Zoo)

str(Zoo)
Zoo$type <-factor(Zoo$type)
prop.table(table(Zoo$type))*100
summary(Zoo)

normalize <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

zoo_norm <- as.data.frame(lapply(Zoo[2:17], normalize))
summary(zoo_norm)

zoo_norm <- cbind(zoo_norm, Zoo$type)
colnames(zoo_norm)[17] <- "type"

pd <- sample(2, nrow(zoo_norm), replace = TRUE, prob = c(0.8,0.2))
zoo_train <- zoo_norm[pd==1,]
zoo_test <- zoo_norm[pd==2,]

library(class)
pred1 <- knn(train = zoo_train, test = zoo_train, cl = zoo_train$type, k=5)
mean(pred1==zoo_train$type)
pred2 <- knn(train = zoo_train, test = zoo_test, cl = zoo_train$type, k=5)
mean(pred2==zoo_test$type)

library(gmodels)
library(caret)
CrossTable(x=zoo_train$type, y=pred1, prop.chisq = FALSE)
CrossTable(x=zoo_test$type, y=pred2, prop.chisq = FALSE)
confusionMatrix(pred1, zoo_train$type)
confusionMatrix(pred2, zoo_test$type)