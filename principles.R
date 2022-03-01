
# KISS-Code ---------------------------------------------------------------

   # Linear affine function
   laf = function(x, a, b){
      return(a * x + b)
   }

   

# DRY-Code ----------------------------------------------------------------

   I = c(1, 5, 3, 7, 9)
   a = b = 3
   
   # Bad Example:
   result = c()
   result[1] = laf(I[1], a, b)
   result[2] = result[1] + laf(I[2], a, b)
   result[3] = result[2] + laf(I[3], a, b)
   result[4] = result[3] + laf(I[4], a, b)
   result[5] = result[4] + laf(I[5], a, b)
   
   # Good Example:
   result = numeric(5) # Allocate space beforehand
   result[1] = laf(I[1], a, b)
   for(i in 2:length(I)){
      result[i] = result[i-1] + laf(I[i], a, b)
   }
   
   # Best Example:
   result = cumsum(laf(I, a, b))
   

# YAGNI-Code --------------------------------------------------------------

   # Bad Example:
   laf = function(x, a, b){
      return(a * x + b)
   }
   y = laf(3, 4, 2)
   
   # Good Example:
   y = 3*4 + 2 # Careful with magic numbers!