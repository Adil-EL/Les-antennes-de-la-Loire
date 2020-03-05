# template for one EGO iteration
#
rm(list=ls())

library(DiceDesign)
library(DiceKriging)

source("./test_functions.R")
source("./RSalgorithm.R")
source("./NSalgorithm.R")
source("./lbfgs.R")
source("./cmaes.R")
source("./of_wrapper.R")

##### user data #####
## 2d problem 
load('antennes_6d_train.Rdata')  # or antennes_6d_train.Rdata
nb_antennas <- 3  # make it 3 for the 6d antennas pb
C_init <- C
S_init <- S
fmin <- min(S_init)
LB = rep(c(-6,-6.5),nb_antennas)
UB = rep(c(5,8.7),nb_antennas)
# calculate a GP model
GPmodel <- km(~1, design=C_init, response=S_init, covtype = "matern5_2")
zdim <- GPmodel@d

##### define -EI function
#   GPmodel and fmin are passed as global variables... a bit ugly
mEI <- function(x) {
  x <- matrix(x,ncol=GPmodel@d)
  p <- predict(object = GPmodel,newdata=data.frame(x),type="UK")
  m <- p$mean
  s <- p$sd
  # !!!  here do the calculations for EI
  w<-(fmin-m)/s
  EI <- (fmin-m)*pnorm(w)+s*dnorm(w)
  # !!!  end here do the calculations for EI
  return(as.numeric(-1*EI))
}

# one call to mEI
mEI(x = runif(zdim, min=LB,max=UB))

### optimization related data
budget <- 3000
# 2 other global variables for the optim, sorry and never mind
store_hist <<- FALSE # TRUE only inside lbfgs.R, see file.
glob_noisy <- FALSE

# optimize with normal search
paramNS <- list(LB=LB,UB = UB,budget = budget,dim=zdim, xinit=rep(1,zdim),sigma=0.3) # param for normal_search
optresNS <- normal_search(mEI, paramNS)
# print out results
cat("xbest=")
optresNS$x_best
cat("fbest=")
optresNS$f_best

# optimize with CMA
paramCMA <- list(LB=LB,UB = UB,budget = budget, dim=zdim, xinit=rep(-4, zdim),sigma=2.) # param for cmaes  
optresCMA <- cmaes(mEI, paramCMA)
# print out results
cat("xbest=")
optresCMA$x_best
cat("fbest=")
optresCMA$f_best

#------------save the results-----------------

##  cas 2d
fname <- "LAMZAOUAK_PERRIN_ELHAKOUNI_EGO_2d.Rdata"  # In the case of 3 antennas put 6d instead of 2d

x_solution_EGO_2d <- optresCMA$x_best

y_solution_EGO_2d <- predict(object = GPmodel ,newdata=matrix(x_solution_EGO_2d,ncol=2),type="UK")
y_solution_EGO_2d<-y_solution_EGO_2d$mean
save(x_solution_EGO_2d,y_solution_EGO_2d,file=fname)

##  cas 6d
fname <- "LAMZAOUAK_PERRIN_ELHAKOUNI_EGO_6d.Rdata"  # In the case of 3 antennas put 6d instead of 2d

x_solution_EGO_6d <- optresCMA$x_best

y_solution_EGO_6d <- predict(object = GPmodel ,newdata=matrix(x_solution_EGO_6d,ncol=6),type="UK")
y_solution_EGO_6d<-y_solution_EGO_6d$mean
save(x_solution_EGO_6d,y_solution_EGO_6d,file=fname)


