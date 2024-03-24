# File Containing Linear Regression Code

# For information on the algorithm used: https://arxiv.org/pdf/2103.03475.pdf

# Assumes X is a matrix of nxp,
# Assumes y is a vector of nx1
# L1,L2 are positive values for the strength of penalty on the L1 and L2 regularization
# Alpha is a continuous parameter on [0,1] determining the weighted average of the penalties


elasticLR <- function(X, y, l1, l2, alpha = 0.5, path_length = 50, tolerance = 1e-5, learning_rate = 1e-4, max_iter = 1000,...) {
  
  data <- checkData(X,y)
  converge = FALSE
  r <- dim(data)[1]
  c <- dim(data)[2] - 1
  X <- as.matrix(cbind(rep(1,c),data[,-1]), byrow = T)
  y <- as.matrix(data[,1], byrow = T)
  N <- length(data$y)
  beta_hats <- as.matrix(rep(0,c + 1), nrow = c + 1)
  

  if (alpha > 1 | alpha < 0) {
    message("Invalid Alpha, defaulting to 0.5")
    alpha = 0.5
  }
  
  j = 0
  while (j < max_iter & !converge) {
    beta_hat_previous <- beta_hats
    j = j + 1
    
    y_pred <- X %*% beta_hats 
    dB <- rep(0,c)
    for (i in 2:(c+1)) {
      
      if (beta_hats[i] > 0) 
        dB[i-1] = (-2 * t(X[,i]) %*% (y - y_pred) + l1 + 2 * l2 * beta_hats[i])/r
      
      else 
        dB[i-1] = (-2 * t(X[,i]) %*% (y - y_pred) - l1 + 2 * l2 * beta_hats[i])/r
      
    }
    
    dB0 <- -2 * t(y) %*% y_pred / r
    step_b0 <- learning_rate * dB0
    step_b <- learning_rate * dB
    
    beta_hats <- c(beta_hats[1] - step_b0, beta_hats[-1] - step_b)
    
    tolerance_vector <- (abs(beta_hats - beta_hat_previous) < tolerance)
    if (sum(tolerance_vector) == (c + 1)) {
      converge = TRUE
      print("Converged")
    }
  }
  
  return(beta_hats)
  
  }