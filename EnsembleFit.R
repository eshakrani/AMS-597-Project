#' Train Meta Learner for Binary Classification and Regression
#'
#' This function trains a meta learner for regression using specified base models.
#' It performs hyperparameter tuning for each base model and combines their predictions to train the meta learner.
#' Training and tuning is done with glmnet, e1071 and the randomForest package.
#'
#' @param model_names A character vector specifying the base models to use. Currently supported models are "svm" and "randomForest".
#' @param meta_model_name A character string specifying the type of meta learner to use. Currently supported options are "glm", "svm", or "randomForest".
#' @param X A matrix or data frame containing the predictor variables.
#' @param y A numeric vector or factor containing the target variable.
#' @param alphas A numeric scalar/vector specifying the alpha values for elastic net regularization (only applicable when meta_model_name = "glm").
#' @param lambda A numeric scalar/vector specifying the lambda value for regularization (only applicable when meta_model_name = "glm").
#'
#' @return An object representing the trained meta learner.

train_meta_learner <- function(model_names, meta_model_name, X, y, alphas, lambda = NULL) {
  library(randomForest)
  library(e1071)
  library(mlr)
  print(y)
  # Check if model_names is a character vector
  if (!is.character(model_names)) {
    stop("'model_names' argument must be a character vector of model names.")
  }
  
  # Initialize variables to store best models and their predictions
  best_svm_model <- NULL
  best_rf_model <- NULL
  best_svm_predictions <- NULL
  best_rf_predictions <- NULL
  
  # Function for SVM hyperparameter tuning
  tune_svm <- function(train_x, train_y) {
    
    svm_tuned <- tune(svm,train.x = train_x, train.y = train_y, ranges = list(cost = 10^(-2:2), gamma = 2^(-5:5)))
    best_svm <- svm_tuned$best.model
    
    return(best_svm)
  }
  
  # Function for random forest hyper parameter tuning and fitting the best model
  tune_rf <- function(train_data, train_target) {
    rf_model <- randomForest(x = train_data, y = train_target)
  
    
    return(rf_model)
  }
  
  # Iterate through model_names
  for (i in 1:length(model_names)) {
    model_name <- model_names[i]
    
    # Check if model is SVM or random forest
    if (model_name == "svm") {
      # Hyperparameter tuning for SVM
      best_svm_model <- tune_svm(train_x = X, train_y = y)
      # Predict using the best SVM model
      best_svm_predictions <- predict(best_svm_model, X)
      
    } else if (model_name == "randomForest") {
      # Hyperparameter tuning for random forest
      best_rf_model <- tune_rf(train_data = X, train_target = y)
      
      # Predict using the best Random Forest model
      best_rf_predictions <- predict(best_rf_model, X)
      
    } else {
      warning("Model type '", model_name, "' not recognized. Skipping model ", i)
      next
    }
  }
  if (length(unique(y)) == 2) {
    best_best_svm_predictions <- as.factor(best_svm_predictions[[2]])
    best_rf_predictions <- as.factor(best_rf_predictions[[2]])
  }
  
  newX <- NULL
  
  if ("svm" %in% model_names & "randomForest" %in% model_names) {
    # To avoid recursive stack issues concatenate the average of outputs between the two predictors.
    newX <- cbind(X, (as.integer(best_svm_predictions) +  as.integer(best_rf_predictions)) / 2)
  } else if ("svm" %in% model_names ) {
    # Only SVM model was used
    newX <- cbind(X, as.integer(best_svm_predictions))
  } else if ("randomForest" %in% model_names) {
    # Only Random Forest model was used
    newX <- cbind(X, as.integer(best_rf_predictions))
  } else {
    stop("At least one of 'svm' or 'randomForest' must be included in 'model_names'.")
  }
  
  # Train the meta learner using the concatenated data
  if (meta_model_name == "glm" & !all(sapply(y, is.factor))) {
    meta_trained <- fitLinearRegressor(as.matrix(newX), y, alphas = alphas, lambda = lambda, family = 'gaussian')
  } else if (meta_model_name == "glm" & all(sapply(y, is.factor))) {
    meta_trained <- fitLogisticRegressor(as.matrix(newX), y, alphas = alphas, lambda = lambda, family = 'gaussian')
  } else if (meta_model_name == "svm") {
    meta_trained <- tune_svm(train_x = newX, train_y = y)
  } else if (meta_model_name == "randomForest") {
    meta_trained <- tune_rf(train_data = newX, train_target = y)
  } else {
    stop("Invalid 'meta_model_name'. Choose from 'glm', 'svm', or 'randomForest'.")
  }
  
  return(meta_trained)
}

