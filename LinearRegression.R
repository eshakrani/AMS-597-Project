# File Containing Linear Regression Code


# Assumes X is a matrix of nxp,
# Assumes y is a vector of nx1
# Lambda is a positive value for the strength of penalty on the L1 and L2 regularization
# Alpha is a continuous parameter on [0,1] determining the weighted average of the penalties
solveLR <- function(X, y, lamdba = c(), alpha, max_iter = 1000, ...) {
  
  solved = FALSE
  data <- checkData(X,y)
  n <- length(data$y)
  
  models <- list(lambda = c(), betas = c())

  for (i in 1:length(lambda)) {
    B_hats <-  c(0,rep(0, times = ncol(x)))
    
    while (!solved) {
      
      for (j in 1:length(B_hats)) {
        eta_j <- B_hats[1] + t(B_hats[-1]) %*% X[j,]
        mu_i <- eta_j
        z_i <- eta_j + (y - mu_i) 
      }
      
    }
     
  }
      
}