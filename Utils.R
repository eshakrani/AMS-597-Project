# Basic utility functions for general use

# Does basic null checks and dimension checks.
# If there is an NA located in X or y, the corresponding row is dropped.
checkData <- function(X,y) {
  
    if (is.null(X) || is.null(y)) 
      stop("X or y is null.")
    
    if (length(y) != nrow(X))
      stop("Dimensions of X and y do not match.")
    
    na_indices <- which(is.na(X) | is.na(y))
    if (length(na_indices) > 0) {
      X <- X[-na_indices, ]
      y <- y[-na_indices]
    }
  
    return(cbind(y, X))
  }


checkAssumptions <- function(family, lambda, alpha, bagging, topP, K, ensemble,
                             nfolds, test_size, R, meta_learner) {
  # Check if family is either 'gaussian' or 'binomial'
  if (!identical(family, 'gaussian') & !identical(family,'binomial')) 
    stop("Family parameter must be either 'gaussian' or 'binomial'")

  
  # Check if bagging, topP, ensemble, and cv are logical
  if (!is.logical(bagging) || !is.logical(topP) || !is.logical(ensemble)) 
    stop("bagging, topP, and ensemble parameters must be logical.")
  
  
  # Check if K is numeric and greater than 1
  if (K %% 1 != 0 | K < 1) {
    stop("K parameter must be numeric and greater than 0.")
  }
  
  if (R %% 1 != 0 | R < 1) 
    stop("(R) Number of boostraps must be a positive integer.")
  
  #if (bagging & length(lambda) < 2)
  #  stop("Bagging is done with cv.glmnet to search for best hyperparameters. Enter more than 1 lambda value.")
  
  if (!is.null(alpha))
    if (!all(alpha >= 0 & alpha <= 1)) 
      stop("All alpha values must be between 0 and 1 inclusive.")
  
  if (!is.null(lambda)) 
    if (is.list(lambda)) 
      if (!all(lambda >= 0)) 
        stop("All lambda values must be greater than or equal to 0.")
  
  
  if (length(alpha) == 1 && lambda != 0 && is.null(alpha)) {
    stop("Alpha can only be null if lambda is 0.")
  }
  
  if (ensemble && (!(length(meta_learner) == 1 ) || !(meta_learner %in% c("svm", "randomForest", "glm"))))
    stop("If ensemble is true, meta_learner must be one of: svm, randomForest, glm")

  
}