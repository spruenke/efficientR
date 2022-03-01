if(!("microbenchmark" %in% installed.packages())){install.packages("microbenchmark")}
if(!("ggplot2" %in% installed.packages())){install.packages("ggplot2")}
library(microbenchmark)
library(ggplot2)
# Define Functions --------------------------------------------------------

# Function computing rowwise variance by utilizing matrix operations
rowVar = function(x){
   n    <- ncol(x)
   xSq  <- rowSums(x^2) # Sum of Squares
   xBar <- rowMeans(x) # Means
   Var  <- (xSq - n * xBar^2) / (n-1)
   return(Var)
}

# Function using apply to compute variance
var_apply = function(x){
   return(apply(x, 1, var))
}

# Function using for-loop to compute variance
var_for = function(x){
   v = numeric(nrow(x))
   for(i in 1:nrow(x)){
      
      v[i] = var(x[i,])
   }
   return(v)
}




# Simulate different number of rows with 150 observations --------------------------------------------

n_row = c(1e2, 5e2, 1e3, 5e3, 1e4)
res = data.frame(median = rep(0, 15), lq = rep(0, 15), uq = rep(0, 15), method = rep(c("r_mat", "r_apply", "r_for"), times = 5))
ind = list(c(1:3), c(4:6), c(7:9), c(10:12), c(13:15))
for(i in 1:5){
   dat_temp = matrix(rnorm(n_row * 150), nrow = n_row[i], ncol = 150)
   temp_time = microbenchmark("r_mat" = rowVar(dat_temp), "r_apply" = var_apply(dat_temp), "r_for" = var_for(dat_temp))
   res[ind[[i]],1] = tapply(temp_time$time, temp_time$expr, median) / 1e6 # Median in Milliseconds
   res[ind[[i]],2] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.25) / 1e6 # lq in Milliseconds
   res[ind[[i]],3] = tapply(temp_time$time, temp_time$expr, quantile, probs = 0.75) / 1e6 # uq in Milliseconds
   
}

res$n_row = rep(n_row, each = 3)

### Plot

pvar = ggplot(data = res)  +
   geom_line(aes(x = n_row, y = median, colour = as.factor(method)), size = 1) +
   #geom_line(aes(x = n_row, y = median, group = as.factor(method)), size = 1) +
   #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
   scale_colour_discrete(name = "Method") +
   labs(title ="Computation Time of Row-wise Variance", x = "Number of Rows", y = "Time in ms")+
   geom_ribbon(aes(x = n_row, ymin = lq, ymax = uq, fill = as.factor(method)),
               alpha = 0.2, linetype = "dashed", show.legend = F) +
   scale_x_continuous(trans = "log10")


ggsave("var_time.pdf")
