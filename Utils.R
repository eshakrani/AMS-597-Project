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