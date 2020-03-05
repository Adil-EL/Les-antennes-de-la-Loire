------------------------------------------------------
README FILE TO R SUITE OF SIMPLE OPTIMIZER UTILITIES
Author : Rodolphe Le Riche
------------------------------------------------------

This directory provides simple optimizers and accompanying R utilities meant as demonstrations in class.

------------------------------------------------------
MAIN PROGRAMS

The 3 files 3Dplots.R, main4tests.R, postproc_tests_optimizers.R :
* 3Dplots.R : make 3D plots of 2D functions and superpose 1 trajectory of an optimizer.
* main4tests.R : repeat optimizations to study the behavior of a particular optimizer with a 
particular objective function.
* postproc_tests_optimizers.R : do summary plots of one or many repeated optimizations that were 
previously performed with main4test.R .

* In main4tests.R describe the function you want to optimize (with the string nameoffun) 
and the optimizer you want to use (with nameofoptim). 
Also choose the number of repetitions of the optimization (no_tests)
In postproc_tests_optimizers.R compare the results of different optimizations by filling
the vector of strings filenames.

------------------------------------------------------
BASIC USE

* In 3Dplots.R and main4tests.R : 
- fill in the name of the objective function (string nameoffun) and 
	the name of the optimizer (string nameofoptim). Possible names for 
	nameoffun are the functions given in test_functions.R . Possible names 
	for nameofoptim are the optimization algorithms R functions present 
	in the directory. Examples of both are given in comments.
- fill in other characteristics of the objective function: 
	number of dimensions (variable zdim, which must be = 2 in 3Dplots.R), 
	position of the optimum (variable glob_xstar), noise characteristics (glob_noisy, glob_tau)
- fill in parameters of the optimizer: they are all fields of the param list, cf. param specific to 
	each optimizer. Typically, one finds lower and upper bounds on the optimization variables 
	(LB and UB), and maximum number of calls to the objective function (budget), and the rest 
	depends on nameofoptim.
- then execute the file. 
- The results of 3Dplots.R are R plots which are also saved as image files fileofplot.png
and contour.png . You may want to copy these files elsewhere or change their names otherwise they will 
be overwritten by the next call to 3Dplots.R .
- The results of main4tests.R are stored in the files "nameofoptim"_"nameoffun"_{some characteristic params}.RData
with plots in "nameofoptim"_"nameoffun"_{some characteristic params}.pdf . The data in the .RData file is meant 
to be reloaded later (cf below). The names of the result files are weakly unique, it is probably safer to change them 
(or move them to another directory) before using main4tests.R again.

* In postproc_tests_optimizer.R : 
- fill in the names of the .RData files you want to process in order to compare the performance of the 
optimizers (string vector filenames). Source the file and enjoy.
	

------------------------------------------------------
LIST OF FILES

3Dplots.R : plots 2D functions in 3D and superimposes optimizer trajectories
cmaes.R : CMA-ES optimization algorithm
lbfgs.R : memory light BFGS optimization algorithm
main4tests.R : main program to call optimizer. Can perform many optimizations.
NSalgorithm.R : a fixed step ES-(1+1) search algorithm
of_wrapper.R : wraps calls to the objective function in order to store all calls thru global variables when necessary
postproc_tests_optimizers.R : creates plots to compare different optimizers
readme.txt : some explanations
RSalgorithm.R : a random search algorithm
utility_optim_functions.R : set of utility functions for plotting and casting our data structures
test_functions.R : a set of test functions (potentially noisy) with control on the location of the optimum

------------------------------------------------------

PROGRAMMING PRINCIPLES

d : problem dimension
N : number of points sampled during 1 optimization

* Names of objective functions and optimizers handled through characters strings and the eval function: this allows easier printing.

* Optimizers signature : 
    optres <- optimizer(ofwrapper, param)
    where ofwrapper is the wrapper to the objective function (of_wrapper.R) and param the list that contains the optimizer parameters
    optres is the list of outputs. optres should contain the fields list(xhist= , fhist= ) where optres$xhist is N*d , optres$fhist is N*1 . Depending on the optimizer other fields will be output (sigmahist ... ) but are not compulsory.

* glob_xhist and glob_fhist : N*d and N*1 matrices with global scope that store the history of an optimizer. They are updated in the objective function wrapper and output to the main through the output list of the optimizer. Some optimizers do not use them (as they keep track of all their OF calls)

* fun and opt : global variables that contains the objective function and the optimizer respectively.

* Objective function definitions 
  ** glob_xstar : global optimum for most test functions (at least sphere, quadratic, ackley, rastrigin, ... not true with michalewicz)
  ** glob_noisy : global boolean to add noise to the test functions 
  ** glob_tau : std deviation of noise to the test functions
  
