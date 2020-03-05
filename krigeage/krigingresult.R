library('DiceKriging')
library('sensitivity')
library('DMwR')
rm(list=ls())
load("antennes_2d_train.Rdata")
design<-C
response<-S
kernels<-c("matern5_2","matern3_2","exp","gauss")

kernels<-c("matern5_2","matern3_2","exp","gauss")

LOOCV <- function(design,response,ker){
  # Attributes:
  #------------------------------
  # fit: Fit of the model
  # dataset: Dataset to be used
  # -----------------------------
  # Returns mean of squared errors for each fold - MSE
  MSEP_=c()
  
  for (idx in seq(from=1,to=nrow(design),by=50)){
    train_design <- design[-c(idx),]
    train_response<-response[-c(idx)]
    test <- design[c(idx),]
    test<-matrix(test,nrow=1,ncol=ncol(design))
    
    
    
    modelkrig1<-km(formula=~1, train_design, train_response, covtype=ker,
                   coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
                   nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
                   penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
                   parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
                   iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)
    
    pred<-predict(modelkrig1, test, type="SK")
    pred<-pred$mean
    
    MSEP_[idx]<-(pred -  response[idx])^2
  }
  RMSE<-sqrt(mean(MSEP_))
  result<-RMSE
  return(result)
}


error<-c()
i<-1
for(ker in kernels)
{
 
  error[i]<-LOOCV(design,response,ker)
  i<-i+1
}

min_index<-which.min(error)
paste("The best kernel for the kriging is :",kernels[min_index],",with an RMSE of :",round(error[min_index],2),sep=' ')

modelkrig<-km(formula=~1, design,response, covtype="gauss",
               coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
               nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
               penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
               parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
               iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)

load("antennes_2d_test.Rdata")
test<-C
pred<-predict(modelkrig, test, type="SK")

pred.mean<-pred$mean
pred.sd<-pred$sd
#pred.variance<-pred.sd*pred.sd
result<-cbind(pred.mean,pred.sd)

names(result)<-c("mean","standard deviation")

LB<-pred.mean -1.96*pred.sd/sqrt(100)
UB<-pred.mean +1.96*pred.sd/sqrt(100)

write.table(cbind(LB,UB),"CI_2d.txt")
write.table(pred.mean,"prediction_2d.txt")


#---------------Cas:6d-----------------
load("antennes_6d_train.Rdata")
design<-C
response<-S
modelkrig<-km(formula=~1, design,response, covtype="gauss",
              coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
              nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
              penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
              parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
              iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)

load("antennes_6d_test.Rdata")
test<-C
pred<-predict(modelkrig, test, type="SK")

pred.mean<-pred$mean
pred.sd<-pred$sd

LB<-pred.mean -1.96*pred.sd/sqrt(300)
UB<-pred.mean +1.96*pred.sd/sqrt(300)

write.table(pred.mean,"prediction_6d.txt")
write.table(cbind(LB,UB),"CI_6d.txt")