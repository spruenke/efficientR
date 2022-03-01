rm(list = ls())

if(!("microbenchmark" %in% installed.packages())){install.packages("microbenchmark")}
if(!("ggplot2" %in% installed.packages())){install.packages("ggplot2")}
if(!("Rcpp" %in% installed.packages())){install.packages("Rcpp")}
if(!("RcppArmadillo" %in% installed.packages())){install.packages("RcppArmadillo")}
library(microbenchmark)
library(ggplot2)
library(Rcpp)

# Define Functions ---------------------------------------------------------------

wild_boot = function(x, y, nboot){
   x_new     <- cbind(rep(1, nrow(x)), x) # Add intercept column
   beta_orig <- solve(t(x_new) %*% x_new) %*% t(x_new) %*% y # OLS Estimator
   resid     <- y - (x_new %*% beta_orig) # Residuals
   coefs     <- matrix(NA, ncol = ncol(x) + 1, nrow = nboot) # Spaceholder for bootstrap coefs
   for(i in 1:nboot){
      resid_boot <- sample(c(-1,1), size = length(resid), replace = T) * resid # Rademacher
      y_boot     <- x_new %*% beta_orig + resid_boot # bootstrap y
      coefs[i,]  <- solve(t(x_new) %*% x_new) %*% t(x_new) %*% y_boot # Bootstrap weights
   }
   coef_boot = colMeans(coefs)
   return(coef_boot)
}

sourceCpp("wildboot.cpp")


# Simulate different numbers of nboot --------------------------------------

   # dataset
      x = as.matrix(iris[,2:4])
      y = iris[,1]
      
   # Simulate
      nboot = c(1e2, 5e2, 1e3, 5e3, 1e4)
      res = data.frame(median = rep(0, 10), lq = rep(0, 10), uq = rep(0, 10), method = rep(c("R", "C++"), times = 5))
      ind = list(c(1:2), c(3:4), c(5:6), c(7:8), c(9:10))
      for(i in 1:5){
         nbi = nboot[i]
         temp_time = microbenchmark("R" = wild_boot(x, y, nbi), "C++" = wild_boot_cpp(x, y, nbi))
         res[ind[[i]],1] = tapply(temp_time$time, temp_time$expr, median) / 1e6 # Median in Milliseconds
         res[ind[[i]],2] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.25) / 1e6 # lq in Milliseconds
         res[ind[[i]],3] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.75) / 1e6 # uq in Milliseconds
         
      }
      
      res$nboot = rep(nboot, each = 2)
      
      ### Plot
      pnboot = ggplot(data = res)  +
         geom_line(aes(x = nboot, y = median, colour = as.factor(method)), size = 1) +
         #geom_line(aes(x = n_row, y = median, group = as.factor(method)), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Method") +
         labs(title ="Computation Time of Wild Bootstrap Regression", x = "Number of Bootstrap Draws", y = "Time in ms")+
         geom_ribbon(aes(x = nboot, ymin = lq, ymax = uq, fill = as.factor(method)),
                     alpha = 0.2, linetype = "dashed", show.legend = F) +
         scale_x_continuous(trans = "log10")
      
      
      ggsave("wild1_time.pdf")
      


# Simulate different number of column dimensions for X --------------------

   # fix nboot to 1e4
   # fix number of observations to 200
      data_1 = matrix(rnorm(200 * 4), nrow = 200) # p = 4
      data_2 =cbind(data_1, matrix(rbeta(200 * 4, 4, 7), nrow = 200)) # p = 8
      data_3 = cbind(data_2, matrix(rpois(200 * 4, 15), nrow = 200)) # p = 12
      data_4 = cbind(data_3, matrix(rchisq(200 * 4, 6), nrow = 200)) # p = 16
      data_5 = cbind(data_4, matrix(runif(200 * 4), nrow = 200)) # p = 20
      
      p = c(4, 8, 12, 16, 20)
      dat_list = list(data_1, data_2, data_3, data_4, data_5)
   # Simulate
      res = data.frame(median = rep(0, 10), lq = rep(0, 10), uq = rep(0, 10), method = rep(c("R", "C++"), times = 5))
      ind = list(c(1:2), c(3:4), c(5:6), c(7:8), c(9:10))
      for(i in 1:5){
         beta = runif(p[i], 5, 50)
         x = dat_list[[i]]
         y = rnorm(1, 20, 2) + x%*%beta + rnorm(200)
         temp_time = microbenchmark("R" = wild_boot(x, y, 1e4), "C++" = wild_boot_cpp(x, y, 1e4))
         res[ind[[i]],1] = tapply(temp_time$time, temp_time$expr, median) / 1e6 # Median in Milliseconds
         res[ind[[i]],2] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.25) / 1e6 # lq in Milliseconds
         res[ind[[i]],3] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.75) / 1e6 # uq in Milliseconds
         
      }
      
      res$p = rep(p, each = 2)
      
      pp = ggplot(data = res)  +
         geom_line(aes(x = p, y = median, colour = as.factor(method)), size = 1) +
         #geom_line(aes(x = n_row, y = median, group = as.factor(method)), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Method") +
         labs(title ="Computation Time of Wild Bootstrap Regression", x = "Column dimension of X (without intercept)", y = "Time in ms")+
         geom_ribbon(aes(x = p, ymin = lq, ymax = uq, fill = as.factor(method)),
                     alpha = 0.2, linetype = "dashed", show.legend = F) +
         scale_x_continuous(trans = "log10")
      
      
      ggsave("wild2_time.pdf")
      