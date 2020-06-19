forest <- read.csv(file.choose())
View(forest)
summary(forest)
library(ggplot2)
plot <- function(a){
  r <- ggplot(forest,aes(a))+geom_histogram(color="black",fill="green")
  return(r)
}
plot1 <- function(a){
  c <- ggplot(forest,aes(a))+geom_boxplot(fill="red",color="black")+coord_flip()
  return(c)
}
(f1 <- plot(a=forest$FFMC))
(f2 <- plot1(a=forest$FFMC))
(d1 <- plot(a=forest$DMC))
(d2 <- plot1(a=forest$DMC))
(dc1 <- plot(a=forest$DC))
(dc2 <- plot1(a=forest$DC))
(i1 <- plot(a=forest$ISI))
(i2 <- plot1(a=forest$ISI))
(t1 <- plot(a=forest$temp))
(t2 <- plot1(a=forest$temp))
(r1 <- plot(a=forest$RH))
(r2 <- plot1(a=forest$RH))
(w1 <- plot(a=forest$wind))
(w2 <- plot1(a=forest$wind))
library(gridExtra)
grid.arrange(f1,f2,d1,d2,dc1,dc2,i1,i2,t1,t2,r1,r2,w1,w2,nrow=7,ncol=2)
set.seed(123)
tra <- sample(2,nrow(forest),replace = T,prob = c(0.7,0.3))
forest_train <- forest[tra==1,]
forest_test <- forest[tra==2,]
attach(forest)
#Trainig the model using SVM different kernal
#1)simple linear SVM
library(kernlab)
forest_classifier <- ksvm(size_category~.,data=forest_train,kernel="vanilladot")
forest_classifier
predict.linear <- predict(forest_classifier,forest_test)
table(predict.linear,forest_test$size_category)
a <- predict.linear==forest_test$size_category
table(a)
prop.table(table(a))
#2)Gaussian svm
forest_classifier1 <- ksvm(size_category~.,data=forest,kernel="rbfdot")
forest_classifier1
predict.gaussian <- predict(forest_classifier1,forest_test)
table(predict.gaussian,forest_test$size_category)
b <- predict.gaussian==forest_test$size_category
table(b)
prop.table(table(b))
#3)polynomial
forest_classifier2 <- ksvm(size_category~.,data=forest,kernel="anovadot")
predict.polynomial <- predict(forest_classifier2,forest_test)
table(predict.polynomial,forest_test$size_category)
c <- predict.polynomial==forest_test$size_category
table(c)
prop.table(table(c))
#using function to find the best output from the kenernal for the classification 
best <- function(kern) {
  forest_classifier3 <- ksvm(size_category~.,data=forest_train,kernel=kern)
  predict.poly <- predict(forest_classifier3,forest_test)
  table(predict.poly,forest_test$size_category)
  d <- predict.poly==forest_test$size_category
  return(d)
}
res1 <- best(kern = "rbfdot")
prop.table(table(res1))
res2 <- best(kern = "tanhdot")
prop.table(table(res2))
res3 <- best(kern = "laplacedot")
prop.table(table(res3))
res4 <- best(kern="besseldot")
prop.table(table(res4))
res5 <- best(kern="anovadot")
prop.table(table(res5))
res6 <- best(kern = "splinedot")
prop.table(table(res6))
res7 <- best(kern = "vanilladot")
prop.table(table(res7))
#so the linear kernal has the best classification
#with a correct classification percentage of 98%