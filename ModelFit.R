library(glmnet)

# Not sure what measures to put, maybe leave it null.

modelFit <- function(X,y, family = c("gaussian","binomial"), measure = c("mse","auc"), 
                     lambda = c(), alpha = c(), bagging = FALSE, topP = FALSE, 
                     K = 10, ensemble = FALSE) {
  
  data <- checkData(X,y)
  X <- data[,-1]
  y <- data[,1]
  
  family = c(family)[1]
  
  lambda <- c(lambda)
  alpha <- c(alpha)
  
  if (topP) {
    # Select top K predictors to be the X matrix
    # Create lambda max * epsilon
    # fit lasso regression with bagging
    # calculate the proportion of times feature i appears
    # pick the top K
  }
  
  if (!bagging) {
    if (ensemble & family == 'gaussian') {
      
      # Going to perform Stacking
      # will take the predicted values from linear regression
      # fitted on X and having SVM fit onto [X,y^]
      # return that SVM model
      
    }
    
    else if (ensemble & family == 'binomial') {
      # Going to perform Stacking
      # will take the predicted values from logistic regression
      # fitted on X and having SVM fit onto [X,y^]
      # return that SVM model
    }
    
    
    else if (family == 'gaussian') {
      
      # If cross validation use the below Regression function
      results <- fitLinearRegressor(...)
      
      # If not cross validation uses tony's functions to return a specific
      # 
    }
    
    else if (family == 'binomial') {
      
      results <- fitLinearClassification(...)
      
    }
  }
  
  if (bagging) {
    
    #' The bagging procedure is simple, only implement for the regression models
    #' For the weight on the bagging procedure to get the final prediction
    #' one possibility is to a uniform weighting. Another is to have an option
    #' when the data is highly skewed/imbalanced on the classification side 
    #' to weight the models that predict the in balanced category better more heavily. 
    #'
    
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