if(!("Rcpp" %in% installed.packages())){install.packages("Rcpp")}
if(!("RcppArmadillo" %in% installed.packages())){install.packages("RcppArmadillo")}


library(Rcpp)
sourceCpp("linmod.cpp")
X = as.matrix(iris[,2:4])
y = iris[,1]
lin_mod_cpp(y, X)

# Check, this is the same as:
lm(y ~ 0 + X)