# Basic utility functions for general use

# Does basic null checks and dimension checks.
# If there is an NA located in X or y, the corresponding row is dropped.
checkData <- function(X,y) {
  
  if (is.null(X) | is.null(y)) 
    stop("X or y is null.")
  
  if (length(y) != dim(X)[1])
    stop("Dimensions of X and y do not match.")
  
  if (sum(is.na(X)) != 0) {
    
    row_drop_index_X <- which(is.na(X), arr.ind = T)[,1] # Row index's with NA's in the X matrix
    
    y <- y[-row_drop_index_X]
    X <- X[-row_drop_index_X,]
  }
  
  if (sum(is.na(y)) != 0) {
    row_drop_index_y <- which(is.na(y), arr.ind = T) # Row index with NA's in the y vector
    
    y <- y[-row_drop_index_y]
    X <- X[-row_drop_index_y,]
  }
  return(cbind(y,X))
}

checkAssumptions <- function(family, lambda, alpha, bagging, topP, K, ensemble,
                             nfolds, test_size, R) {
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
}