library(glmnet)

# Not sure what measures to put, maybe leave it null.

modelFit <- function(X,y, family = c("gaussian","binomial"), measure = c("mse","auc"), 
                     lambda = c(), alpha = c(), bagging = FALSE, topP = FALSE, 
                     K = 0, ensemble = FALSE) {
  
  data <- checkData(X,y)
  X <- data[,-1]
  y <- data[,1]
  
  family = c(family)[1]
  
  lambda <- c(lambda)
  alpha <- c(alpha)
  
  if (topP) {
    # Select top K predictors to be the X matrix
  }
  
  if (!bagging) {
    if (ensemble & family == 'gaussian') {
      # Not sure what to do about this yet.
      
    }
    
    else if (ensemble & family == 'binomial') {
      # Not sure what to do about this yet.

    }
    
    
    else if (family == 'gaussian') {
      
      results <- fitLinearRegressor(...)
    }
    
    else if (family == 'binomial') {
      
      results <- fitLinearClassification(...)
      
    }
  }
  
  if (bagging) {
    
    
    
  }
}


#' Fit Linear Regressor with Elastic Net Regularization
#' 
#' This function searches for the best fit linear regression model with 
#' elastic net regularization. Returns a list of best models per corresponding 
#' alpha value.
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' 
#' @return A list of models, each corresponding to a different value of alpha. 
#'         Each model is trained with the best lambda value selected through cross-validation.
#' 
#' @import glmnet
#' 
#' @export

fitLinearRegressor <- function(X, y, loss, lambda = 0, alphas, nfolds = 5) {
  require(glmnet)
  
  models <- list() # Stores all finalized models after cross validation
  counter <- 0 # Tracks model number
  
  for (alpha in alphas) {
    counter <- counter + 1
    
    cvfit <- cv.glmnet(X, y, alpha = alpha, lambda = lambda, nfolds = nfolds) # Cross validation
    
    # Takes the best models corresponding lambda and re-fits it, then stores that model.
    lambda_best <- cvfit$lambda.min 
    model <- glmnet(X, y, alpha = alpha, lambda = lambda_best)
    models[[counter]] <- model
  }
  
  return(models)
}


#' Fit Logistic Regressor with Elastic Net Regularization
#' 
#' This function searches for the best fit logistic regression model with 
#' elastic net regularization. Returns a list of best models per corresponding 
#' alpha value.
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable (binary outcome).
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' 
#' @return A list of models, each corresponding to a different value of alpha. 
#'         Each model is trained with the best lambda value selected through cross-validation.
#' 
#' @import glmnet
#' 
#' @export

fitLogisticRegressor <- function(X, y, loss, lambda = 0, alphas, nfolds = 5, loss = NULL) {
  require(glmnet)
  
  
  models <- list() # Stores all finalized models after cross validation
  counter <- 0 # Tracks model number
  
  for (alpha in alphas) {
    counter <- counter + 1
    
    cvfit <- cv.glmnet(X, y, alpha = alpha, lambda = lambda, nfolds = nfolds) # Cross validation
    
    # Takes the best models corresponding lambda and re-fits it, then stores that model.
    lambda_best <- cvfit$lambda.min 
    model <- glmnet(X, y, alpha = alpha, lambda = lambda_best, family = "binomial")
    models[[counter]] <- model
  }
  
  return(models)
}