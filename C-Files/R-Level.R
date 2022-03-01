#### DON'T FORGET TO COMPILE THE .C-FILES!

# Simple .C Version -------------------------------------------------------

   dyn.load("doubler.so")
   times_two_r = function(x){
      .C("times_two_c", a = as.integer(x))$a
   }

   

# Better .Call Version ----------------------------------------------------

   dyn.load("rowVar.so")
   row_var = function(x){
      .Call("rowV", x, ncol(x), nrow(x))
   }
   