glass <- read.csv(choose.files())
View(glass)

str(glass)
glass$Type <- factor(glass$Type)
prop.table(table(glass$Type))*100
summary(glass)

normalize <- function(x){ 
 return((x-min(x))/(max(x)-min(x)))
}

glass_norm <- as.data.frame(lapply(glass[1:9], normalize))

gls_norm_label <- cbind(glass_norm, glass$Type)
colnames(gls_norm_label)[10] <- "Type"

pd <- sample(2, nrow(gls_norm_label), replace = TRUE, prob = c(0.8, 0.2))
gls_train <- gls_norm_label[pd==1,]
gls_test <- gls_norm_label[pd==2,]

library(class)
pred1 <- knn(train = gls_train, test = gls_train, cl=gls_train$Type, k=6)
mean(pred1==gls_train$Type)
pred2 <- knn(train = gls_train, test = gls_test, cl=gls_train$Type, k=6)
mean(pred2==gls_test$Type)

library(gmodels)
library(caret)
CrossTable(x=gls_train$Type, y=pred1, prop.chisq = FALSE)
CrossTable(x=gls_test$Type, y=pred2, prop.chisq = FALSE)
confusionMatrix(pred1, gls_train$Type)
confusionMatrix(pred2, gls_test$Type)
