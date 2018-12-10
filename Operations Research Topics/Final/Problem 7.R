# Use the R script optimizeMe2.R and add R code to find the minimum value of the
# function called fitness on the domain described in the script. You’ll need to
# experiment and be sure that you locate the coordinates of the minimum to the
# nearest hundredth. There are many local minima, but you need to locate the
# global minimum using the R optimization tools we saw in class.

library(nloptr)
library(GA)
library(GenSA)

R = qr.Q(qr(matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=T)))
dimension <- 2
lower = rep(-5,dimension)
upper = rep(5,dimension) 

fitness = function(x){
  # x should be a vector with two elements e.g. fitness(c(1,2))
  z = R%*%x+c(.5,.5);
  s = 100*(z[1]^2-z[2])^2 + (z[1]-1)^2;
  return(10*(s/4000-cos(s))+10)
}

fitnessGA = function(x){
  # Since GA maximizes, have function return -f(x) to minimize
  z = R%*%x+c(.5,.5);
  s = 100*(z[1]^2-z[2])^2 + (z[1]-1)^2;
  return( (10*(s/4000-cos(s))+10) * -1 )
}

###########################################
## Naive Multistart
###########################################
bestmin <- 100000
numlocalSearch = 10000 
numevals = 0
set.seed(444)
for (j in 1:numlocalSearch){
  x0 <- as.vector(runif(dimension,min=-5,max=5));
  result <- optim(x0,fitness,method="L-BFGS-B",lower=lower,upper=upper);
  numevals = numevals + result$counts[1]
  if (result$value<bestmin){
    bestmin = result$value
    bestx = result$par
    }
}
sprintf("Answer using Naive Multistart is %.2f at (%.2f, %.2f)", 
        fitness(bestx), bestx[1], bestx[2])

###########################################
# Multi-level single linkage
###########################################
set.seed(412)
x0 <- as.vector(runif(dimension,min=-5,max=5));
result <- mlsl(x0,fitness,lower=lower,upper=upper,nl.info=TRUE,
               control=list(maxeval = 100000))

sprintf("Answer using Multi-level Single-linkage is %.2f at (%.2f, %.2f)", 
        fitness(result$par), result$par[1], result$par[2])

###########################################
## Genetic Algorithm with local search
###########################################
set.seed(423)
result = ga(type="real-valued",fitness=fitnessGA,
            popSize = 70,
            min=lower,max=upper,maxiter=2500,optim=TRUE);
round(result@solution,5)
result@fitnessValue
fitness(as.vector(result@solution))

sprintf("Answer using GA with local search is %.2f at (%.2f, %.2f)", 
        fitness(as.vector(result@solution)), result@solution[1], result@solution[2])

###########################################
# Simulated Annealing
###########################################
set.seed(1234) # The user can use any seed.
out <- GenSA(lower = lower, upper = upper, fn = fitness,
             control=list(verbose=TRUE))

sprintf("Answer using Simulated Annealing is %.2f at (%.2f, %.2f)", 
        fitness(out$par), out$par[1], out$par[2])

# All methods agree on minimum (0) and variable values at it. (-.63, -.32)
