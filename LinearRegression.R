# File Containing Linear Regression Code

# For information on the algorithm used: https://arxiv.org/pdf/2103.03475.pdf

# Assumes X is a matrix of nxp,
# Assumes y is a vector of nx1
# L1,L2 are positive values for the strength of penalty on the L1 and L2 regularization
# Alpha is a continuous parameter on [0,1] determining the weighted average of the penalties


elasticLR <- function(X, y, l1, l2, alpha = 0.5, tolerance = 1e-7, learning_rate = 1e-2, max_iter = 50000, clip_threshold = 1e3,...) {
  
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
  
  j = 0
  while (j < max_iter & !converge) {
    beta_hat_previous <- beta_hats
    j = j + 1
    dB0 <- 0
    y_pred <- X %*% beta_hats[-1] + beta_hats[1]
    dB <- rep(0,c)
    for (i in 1:c) {
      
      if (beta_hats[i] > 0) 
        dB[i] = ((-2 * t(X[,i]) %*% (y - y_pred)) + alpha * l1 + 2 * (1-alpha) * l2 * beta_hats[i])/r
      
      else 
        dB[i] = ((-2 * t(X[,i]) %*% (y - y_pred)) - alpha * l1 + 2 * (1-alpha) * l2 * beta_hats[i])/r
      
    }

    dB0 <- -2 * sum(y - y_pred) / r
    step_b0 <- learning_rate * dB0
    step_b <- learning_rate * dB
    
    if (max(abs(step_b0)) > clip_threshold) {
      step_b0 <- clip_threshold * step_b0 / max(abs(step_b0))
    }
    if (max(abs(step_b)) > clip_threshold) {
      step_b <- clip_threshold * step_b / max(abs(step_b))
    }
    
    beta_hats <- beta_hats - c(step_b0, step_b)
    
    tolerance_vector <- (abs(beta_hats - beta_hat_previous) < tolerance)
    if (sum(tolerance_vector) == (c + 1)) {
      converge = TRUE
      message("Converged")
    }
  }
  if (!converge) {
    warning("Warning, failed to converge with set iterations")
  }
  
  return(beta_hats)
  
}
