#https://www.r-bloggers.com/neural-networks-exercises-part-1/
#http://www.r-exercises.com/2017/06/08/neural-networks-solutions-part-1/

Below are the solutions to these exercises on neural network.

####################
#                  #
#    Exercise 1    #
#                  #
####################
set.seed(42)
x<-runif(200, -10, 10)
y<-sin(x)

####################
#                  #
#    Exercise 2    #
#                  #
####################
weight<-runif(10, -1, 1)

####################
#                  #
#    Exercise 3    #
#                  #
####################
set.seed(42)
index<-sample(1:length(x),round(0.75*length(x)),replace=FALSE)
reg.train<-data.frame(X=x[index],Y=y[index])
reg.test<-data.frame(X=x[-index],Y=y[-index])

####################
#                  #
#    Exercise 4    #
#                  #
####################
library(nnet)
set.seed(42)
reg.model.1<-nnet(reg.train$X,reg.train$Y,size=3,maxit=50,Wts=weight,linout=TRUE)
## # weights:  10
## initial  value 103.169943 
## iter  10 value 70.636986
## iter  20 value 69.759785
## iter  30 value 63.215384
## iter  40 value 45.634297
## iter  50 value 39.876476
## final  value 39.876476 
## stopped after 50 iterations
str(reg.model.1)
## List of 15
##  $ n            : num [1:3] 1 3 1
##  $ nunits       : int 6
##  $ nconn        : num [1:7] 0 0 0 2 4 6 10
##  $ conn         : num [1:10] 0 1 0 1 0 1 0 2 3 4
##  $ nsunits      : num 5
##  $ decay        : num 0
##  $ entropy      : logi FALSE
##  $ softmax      : logi FALSE
##  $ censored     : logi FALSE
##  $ value        : num 39.9
##  $ wts          : num [1:10] -7.503 2.202 3.004 -0.806 -4.69 ...
##  $ convergence  : int 1
##  $ fitted.values: num [1:150, 1] -0.196 0.568 -0.353 -0.205 -0.161 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : NULL
##  $ residuals    : num [1:150, 1] 0.692 0.3079 -0.0398 0.4262 -0.7024 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : NULL
##  $ call         : language nnet.default(x = reg.train$X, y = reg.train$Y, size = 3, Wts = weight,      linout = TRUE, maxit = 50)
##  - attr(*, "class")= chr "nnet"
####################
#                  #
#    Exercise 5    #
#                  #
####################
predict.model.1<-predict(reg.model.1,data.frame(X=reg.test$X))
str(predict.model.1)
##  num [1:50, 1] -0.201 0.184 -0.873 -0.981 0.598 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : NULL
rmse.reg<-sqrt(sum((reg.test$Y-predict.model.1)^2))
rmse.reg
## [1] 3.41651
plot(sin, -10, 10)
points(reg.test$X,predict.model.1)
plot of chunk NN_part1
####################
#                  #
#    Exercise 6    #
#                  #
####################
set.seed(42)
reg.model.2<-nnet(reg.train$X,reg.train$Y,size=7,maxit=50,Wts=runif(22, -1, 1),linout=TRUE)
## # weights:  22
## initial  value 353.642846 
## iter  10 value 55.906010
## iter  20 value 42.700328
## iter  30 value 22.757713
## iter  40 value 16.910492
## iter  50 value 12.770497
## final  value 12.770497 
## stopped after 50 iterations
str(reg.model.2)
## List of 15
##  $ n            : num [1:3] 1 7 1
##  $ nunits       : int 10
##  $ nconn        : num [1:11] 0 0 0 2 4 6 8 10 12 14 ...
##  $ conn         : num [1:22] 0 1 0 1 0 1 0 1 0 1 ...
##  $ nsunits      : num 9
##  $ decay        : num 0
##  $ entropy      : logi FALSE
##  $ softmax      : logi FALSE
##  $ censored     : logi FALSE
##  $ value        : num 12.8
##  $ wts          : num [1:22] 0.894 0.992 1.997 1.506 8.113 ...
##  $ convergence  : int 1
##  $ fitted.values: num [1:150, 1] 0.585 0.6145 -0.464 0.0943 -0.8862 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : NULL
##  $ residuals    : num [1:150, 1] -0.0891 0.2612 0.071 0.1269 0.0232 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : NULL
##  $ call         : language nnet.default(x = reg.train$X, y = reg.train$Y, size = 7, Wts = runif(22,      -1, 1), linout = TRUE, maxit = 50)
##  - attr(*, "class")= chr "nnet"
predict.model.2<-predict(reg.model.2,data.frame(X=reg.test$X))
str(predict.model.2)
##  num [1:50, 1] 1.13 0.153 -0.931 -0.962 0.647 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : NULL
rmse.reg<-sqrt(sum((reg.test$Y-predict.model.2)^2))
rmse.reg
## [1] 2.188407
plot(sin, -10, 10)
points(reg.test$X,predict.model.2)
plot of chunk NN_part1
####################
#                  #
#    Exercise 7    #
#                  #
####################
data<-iris

scale.data<-data.frame(lapply(data[,1:4], function(x) scale(x)))
scale.data$Species<-data$Species

index<-sample(1:nrow(scale.data),round(0.75*nrow(scale.data)),replace=FALSE)
clust.train<-scale.data[index,]
clust.test<-scale.data[-index,]

####################
#                  #
#    Exercise 8    #
#                  #
####################
set.seed(42)
clust.model<-nnet(Species~.,size=10,Wts=runif(83, -1, 1),data=clust.train)
## # weights:  83
## initial  value 187.294915 
## iter  10 value 10.386561
## iter  20 value 5.337510
## iter  30 value 2.311922
## iter  40 value 1.426508
## iter  50 value 1.387440
## iter  60 value 1.386324
## final  value 1.386294 
## converged
####################
#                  #
#    Exercise 9    #
#                  #
####################
predict.model.clust<-predict(clust.model,clust.test[,1:4],type="class")

####################
#                  #
#    Exercise 10   #
#                  #
####################
Table<-table(clust.test$Species ,predict.model.clust)
Table
##             predict.model.clust
##              setosa versicolor virginica
##   setosa         16          0         0
##   versicolor      0          9         0
##   virginica       0          0        13
accuracy<-sum(diag(Table))/sum(Table)
accuracy
## [1] 1