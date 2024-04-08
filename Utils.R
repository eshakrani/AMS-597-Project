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

checkAssumptions <- function(family, measure, lambda, alpha, bagging, topP, K, ensemble, cv) {
  # Check if family is either 'gaussian' or 'binomial'
  if (!(family %in% c("gaussian", "binomial"))) {
    stop("Family parameter must be either 'gaussian' or 'binomial'")
  }
  
  # Check if measure is either 'mse' or 'auc'
  #if (!(measure %in% c("mse", "auc"))) {
  #  stop("Measure parameter must be either 'mse' or 'auc'")
  #}
  
  # Check lambda and alpha
  if (!cv) {
    # If cv is false, lambda and alpha should be single values
    if (length(lambda) != 1 || length(alpha) != 1) {
      stop("When cv is false, lambda and alpha should be single values.")
    }
  } else {
    # If cv is true, lambda and alpha should be lists
    if (!is.list(lambda) || !is.list(alpha)) {
      stop("When cv is true, lambda and alpha should be lists.")
    }
  }
  
  # Check if bagging, topP, ensemble, and cv are logical
  if (!is.logical(bagging) || !is.logical(topP) || !is.logical(ensemble) || !is.logical(cv)) {
    stop("bagging, topP, ensemble, and cv parameters must be logical.")
  }
  
  # Check if K is numeric and greater than 1
  if (!is.numeric(K) || K < 1) {
    stop("K parameter must be numeric and greater than 0.")
  }
}