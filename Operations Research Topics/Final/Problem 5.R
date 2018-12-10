## Problem 5, DS775 Final - Chris Austin
library(GenSA)

coord = read.csv('xycoordinates.csv')
numcities = length(coord[,1])
distmat = as.matrix( dist(coord) )

distance <- function(sq) {
  sq2 <- embed(sq, 2)
  sum(distmat[cbind(sq2[,2], sq2[,1])])
}

genseq <- function(sq) {  
  idx <- seq(2, nrow(distmat))
  changepoints <- sample(idx, size = 2, replace = FALSE)
  tmp <- sq[changepoints[1]]
  sq[changepoints[1]] <- sq[changepoints[2]]
  sq[changepoints[2]] <- tmp
  sq
}

sq2 <- c(1:nrow(distmat), 1) 

#####################################################################
# 31015.65 Optimal 
set.seed(42497) 
res <- optim(sq2, distance, genseq, method = "SANN",
             control = list(maxit = 35000, temp = 2000, trace = TRUE,
                            REPORT = 500))
res$par
res$value
#####################################################################