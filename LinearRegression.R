# File Containing Linear Regression Code

# For information on the algorithm used: https://arxiv.org/pdf/2103.03475.pdf

# Assumes X is a matrix of nxp,
# Assumes y is a vector of nx1
# L1,L2 are positive values for the strength of penalty on the L1 and L2 regularization
# Alpha is a continuous parameter on [0,1] determining the weighted average of the penalties


elasticLR <- function(X, y, l, alpha = 0.5, tolerance = 1e-5, path.length = 100, epsilon = 1e-3) {
  
  data <- checkData(X,y)
  converge = FALSE 
  r <- dim(data)[1]
  c <- dim(data)[2] - 1
  X <- as.matrix(data[,-1], byrow = T)
  y <- as.matrix(data[,1], byrow = T)
  beta_hats <- as.matrix(rep(0,c + 1), nrow = c + 1)

  
  if (alpha > 1 | alpha < 0) {
    message("Invalid Alpha, defaulting to 0.5")
    alpha = 0.5
  }
  
  if (alpha == 0)
    alpha = alpha + epsilon
  l_max <- max(abs(t(X) %*% y))/(r*alpha) 

  if (l >= l_max) {
    beta_hats[1] <- mean(y)
    warning("Penalty is too large, returning naive model.")
    return(beta_hats)
  }
  l_path <- seq(l, l_max, path.length)
  
  
  for(i in 1:path.length) {
    converge = FALSE
    while(!converge) {
      B_s <- beta_hats
      for(j in 1:c) {
        k <- which(B_s != 0)
        
        x_r.ij <- (X[,j] %*% y) - (X[,j] %*% X[,k]) %*% B_s[k]
        B.j.star <- 1/r * x_r.ij + B_s[j]
        s <- soft_threshold(B.j.star, l)
        B.j <- sign(B.j.star) * s/(1 + l*(1-alpha))
        beta_hats[j] <- B.j
        
      }
      if (tolerance_check(B_s,beta_hats,tolerance)) 
        converge = TRUE
      
    }
  }
  
  return(beta_hats)
}

soft_threshold <- function(beta, lambda) {
  if (lambda >= abs(beta))
    return(0)
  else if (beta > 0 & lambda < abs(beta))
    return(beta - lambda)
  else 
    return(beta + lambda)
}

tolerance_check <- function(vector, updated_vector, tolerance) {
  maximum_change <- max(abs(vector - updated_vector))
  return(maximum_change < tolerance)
}