#' @name Check Data
#'
#' @description
#' This function performs basic null checks and dimension checks on input data.
#' If there is an NA located in either the features (X) or the target variable (y),
#' the corresponding row is dropped from both X and y.
#'
#' @param X A matrix or data frame representing the features.
#' @param y A vector representing the target variable.
#' @return A data frame or matrix where rows with NAs in either X or y have been removed.
#' @details This function checks for null values in X and y. If either X or y is NULL,
#' it raises an error. It also checks if the number of rows in X matches the length of y.
#' If they do not match, an error is raised. If NAs are found in X or y, the corresponding
#' rows are removed from both X and y.
#' @seealso Other functions related to data preprocessing.
#' @examples
#' X <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2)
#' y <- c(1, 2, 3)
#' cleaned_data <- checkData(X, y)
#' @export
checkData <- function(X,y) {
  
    if (is.null(X) || is.null(y)) 
      stop("X or y is null.")
    
    if (dim(y)[1] != dim(X)[1])
      stop("Dimensions of X and y do not match.")
    
    df <- data.frame(y = y,X)
    df <- na.omit(df)
  
    return(df)
  }


#' @name checkAssumptions
#' @description This function checks the assumptions for a machine learning model including parameters such as family type, lambda, alpha, bagging, topP, K, ensemble, nfolds, test_size, and R.
#'
#' @param family Type of family for the model. It must be either 'gaussian' or 'binomial'.
#' @param lambda Regularization parameter for the model.
#' @param alpha Elastic net mixing parameter.
#' @param bagging Logical value indicating whether bagging is used in the model.
#' @param topP Logical value indicating whether topP sampling is used in the model.
#' @param K Number of nearest neighbors in the model.
#' @param ensemble Logical value indicating whether ensemble learning is used in the model.
#' @param nfolds Number of folds for cross-validation.
#' @param test_size Size of the test dataset.
#' @param R Number of bootstraps for ensemble learning.
#' @return No return value; the function is used for validation and raises errors if assumptions are violated.
#'
#' @details This function validates the parameters used in a machine learning model to ensure they meet certain assumptions. It checks if the family parameter is either 'gaussian' or 'binomial', if bagging, topP, and ensemble parameters are logical, if K is numeric and greater than 0, and if R is a positive integer.
#'
#' @examples
#' checkAssumptions(family = "gaussian", lambda = 0.1, alpha = 0.5, bagging = TRUE, topP = FALSE, K = 5, ensemble = TRUE, nfolds = 10, test_size = 0.2, R = 100)
#'
#' @export


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
  
  
  if (is.null(alpha) && lambda != 0) {
    stop("Alpha can only be null if lambda is 0.")
  }
  
  if (ensemble && (!(length(meta_learner) == 1 ) || !(meta_learner %in% c("svm", "randomForest", "glm"))))
    stop("If ensemble is true, meta_learner must be one of: svm, randomForest, glm")

  
}