
rm(list=ls())

library(DiceDesign)
library(DiceKriging)

library("rgl") # library for plots
source("./test_functions.R")
source("./RSalgorithm.R")
source("./NSalgorithm.R")
source("./lbfgs.R")
source("./cmaes.R")
source("./of_wrapper.R")



##### user data #####
zdim <- 2 # 6 in the case of 3 antennas problem
budget <- 3000
LB = -6
UB = 5

# controls for noisy functions and other dirty global variables
glob_noisy <- FALSE # is the function noisy
glob_tau <- 1 # noise std deviation
# glob_estim_noise <- FALSE # this should go in KNF parameters
glob_xstar <- rep(2.5,zdim)
store_hist <<- FALSE # TRUE only inside lbfgs.R, see file.


load("antennes_2d_train.Rdata") #6 in the case of 3 antennas problem
design<-C
response<-S

# Modèle de krigeage

kmodel<-km(formula=~1, design, response, covtype="gauss",
           coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
           nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
           penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
           parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
           iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)







s_tilde<-function(newdata)
{
  newdata<-matrix(newdata,ncol=zdim)
  S_tilde<-predict(kmodel, newdata, type="UK")
  result<-S_tilde$mean
  return(-result)
}


fun <- s_tilde

# optimize with normal search
paramNS <- list(LB=LB,UB = UB,budget = budget,dim=zdim, xinit=rep(1,zdim),sigma=0.3) # param for normal_search
optresNS <- normal_search(fun, paramNS)
# print out results
cat("xbest=")
optresNS$x_best
cat("fbest=")
-optresNS$f_best
plot(optresNS$fhist,type="l",
     xlab="no. calls to f",ylab="f")
title("ES-(1+1)")

# optimize with CMA-ES
paramCMA <- list(LB=LB,UB = UB,budget = budget, dim=zdim, xinit=rep(-4, zdim),sigma=2.) # param for cmaes  
optresCMA <- cmaes(fun, paramCMA)
# print out results
cat("xbest=")
optresCMA$x_best
cat("fbest=")
-optresCMA$f_best
plot(-optresCMA$ymeanhist,type="l",
     xlab="no. of iterations",ylab="f of xmean")
title("CMA-ES")

#--Save the results
# save the results, put your names in the output file
fname <- "LAMZAOUAK_PERRIN_ELHAKOUNI_cma_2d.Rdata" # In the case of 3 antennas put 6d instead of 2d
x_solution_cma_2d <- matrix(optresCMA$x_best,ncol=zdim)

y_solution_cma_2d <- predict(object =kmodel ,newdata=data.frame(x_solution_cma_2d),type="UK")

save(x_solution_cma_2d,y_solution_cma_2d,file=fname)


fname <- "LAMZAOUAK_PERRIN_ELHAKOUNI_NS_2d.Rdata"  # In the case of 3 antennas put 6d instead of 2d

x_solution_cma_2d <- matrix(optresNS$x_best,ncol=zdim)

y_solution_cma_2d <- predict(object =kmodel ,newdata=data.frame(x_solution_cma_2d),type="UK")

save(x_solution_cma_2d,y_solution_cma_2d,file=fname)

