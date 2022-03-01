rm(list = ls())

if(!("microbenchmark" %in% installed.packages())){install.packages("microbenchmark")}
if(!("ggplot2" %in% installed.packages())){install.packages("ggplot2")}
if(!("Rcpp" %in% installed.packages())){install.packages("Rcpp")}
library(microbenchmark)
library(ggplot2)
library(Rcpp)


# Define Functions --------------------------------------------------------

bm_test = function(x, y, alpha = 0.05){
   nx = length(x)
   ny = length(y)
   
   BMstats <- rankFD:::BMstat(x, y, nx, ny, "t.app")
   
   pvalues = 2 * min(pt(BMstats[, 5], BMstats[, 9]), 1 - pt(BMstats[, 5], BMstats[, 9]))
   
   crit1 <- qt(1 - alpha/2, BMstats[, 9])
   crit2 <- qt(alpha/2, BMstats[, 9])
   
   Rel.Effects <- data.frame(Estimator = BMstats[,1], Std.Error = BMstats[, 10], T_stat = BMstats[, 5], p.Value = pvalues)
   #Rel.Effects[, 1:4] <- round(Rel.Effects[, 1:4], 5)
   return(Rel.Effects)  
}

sourceCpp("bm_cpp.cpp")


# Simulate different (total) sample sizes ---------------------------------

n_tot = c(30, 50, 100, 200, 500)

res = data.frame(median = rep(0, 10), lq = rep(0, 10), uq = rep(0, 10), method = rep(c("R", "C++"), times = 5))
ind = list(c(1:2), c(3:4), c(5:6), c(7:8), c(9:10))
for(i in 1:5){
   x = rnorm(n_tot[i]/2)
   y = rnorm(n_tot[i]/2)
   temp_time = microbenchmark("R" = bm_test(x, y), "C++" = bm_test_cpp(x, y, 0.05))
   res[ind[[i]],1] = tapply(temp_time$time, temp_time$expr, median) / 1e6 # Median in Milliseconds
   res[ind[[i]],2] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.25) / 1e6 # lq in Milliseconds
   res[ind[[i]],3] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.75) / 1e6 # uq in Milliseconds
   
}

res$n_tot = rep(n_tot, each = 2)

### Plot
pbm = ggplot(data = res)  +
   geom_line(aes(x = n_tot, y = median, colour = as.factor(method)), size = 1) +
   #geom_line(aes(x = n_row, y = median, group = as.factor(method)), size = 1) +
   #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
   scale_colour_discrete(name = "Method") +
   labs(title ="Computation Time of Brunner-Munzel Test", x = "Total Sample Size", y = "Time in ms")+
   geom_ribbon(aes(x = n_tot, ymin = lq, ymax = uq, fill = as.factor(method)),
               alpha = 0.2, linetype = "dashed", show.legend = F) +
   scale_x_continuous(trans = "log10")


ggsave("bm_time.pdf")
