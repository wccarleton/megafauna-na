nimbleIndVector <- nimbleFunction(
   run = function(Time = double(0), Delta = double(0)){
      indvector <- c(rep(0,Delta),rep(1,Time-Delta))
      returnType(double(1))
      return(indvector)
   }
)
