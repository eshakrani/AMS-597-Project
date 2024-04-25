train_meta_learner_regression <- function(model_names, meta_model_name, X, y, alphas, lambda = NULL, ...) {
  # Load necessary libraries
  library(randomForest)
  library(e1071)
  
  # Check if model_names is a character vector
  if (!is.character(model_names)) {
    stop("'model_names' argument must be a character vector of model names.")
  }
  
  # Combine X and y into a data frame
  data <- data.frame(X, y)
  
  # Initialize variables to store best models and their predictions
  best_svm_model <- NULL
  best_rf_model <- NULL
  best_svm_predictions <- NULL
  best_rf_predictions <- NULL
  
  # Function for SVM hyperparameter tuning
  tune_svm <- function(train_x, train_y, ...) {
    # Perform grid search for SVM
    svm_tuned <- tune(svm, train.x = train_x, train.y = train_y, ...)
    # Select best model
    best_svm <- svm_tuned$best.model
    return(best_svm)
  }
  
  # Function for random forest hyper parameter tuning and fitting the best model
  tune_rf <- function(train_data, train_target, ...) {
    # Perform grid search for random forest
    
    rf_tuned <- tuneRF(x = train_data, y = train_target, plot = F, trace = F, doBest = T)
    
    return(rf_tuned)
  }
  
  # Iterate through model_names
  for (i in 1:length(model_names)) {
    model_name <- model_names[i]
    
    # Check if model is SVM or random forest
    if (model_name == "svm") {
      # Hyperparameter tuning for SVM
      best_svm_model <- tune_svm(train_x = X, train_y = y, ...)
      
      # Predict using the best SVM model
      best_svm_predictions <- predict(best_svm_model, X)
      
    } else if (model_name == "randomForest") {
      # Hyperparameter tuning for random forest
      best_rf_model <- tune_rf(train_data = X, train_target = y, ...)
      
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
  if (meta_model_name == "glm" & !is.factor(y)) {
    meta_trained <- fitLinearRegressor(as.matrix(newX), y, alphas = alphas, lambda = lambda, family = 'gaussian')
  } else if (meta_model_name == "glm" & is.factor(y)) {
    meta_trained <- fitLogisticRegressor(as.matrix(newX), y, alphas = alphas, lambda = lambda, family = 'gaussian')
  } else if (meta_model_name == "svm") {
    meta_trained <- tune_svm(train_x = newX, train_y = y, ...)
  } else if (meta_model_name == "randomForest") {
    meta_trained <- tune_rf(train_data = newX, train_target = y, ...)
  } else {
    stop("Invalid 'meta_model_name'. Choose from 'glm', 'svm', or 'randomForest'.")
  }
  
  return(meta_trained)
}




# Set seed for reproducibility
set.seed(123)

head(iris)
# Generate predictor variables
X <- iris[, -c(5)]  # Excluding the third and fourth columns
y <- iris[, 5]  # Creating a binary target variable
# Display the first few rows of the dataset

y_mapped <- ifelse(y == levels(y)[1], 0, 1)

# Convert to factor
y_mapped <- factor(y_mapped)
print(y_mapped)

str(X)
str(y)


set.seed(123)
n <- 100  # Number of samples

# Generate predictors
X1 <- rnorm(n)
X2 <- rnorm(n)

# Generate binary target variable
y <- X1 - 2 + X2 * 0.3

# Combine predictors and target into a data frame
binary_data <- data.frame(X1, X2, y)


# Train the meta learner using model names and custom X, y
meta_trained <- train_meta_learner_regression(model_names = c("svm","randomForest"), 
                                   meta_model_name = "glm", 
                                   X = binary_data[,-3], 
                                   y = binary_data[,3], 
                                   cv_folds = 5,
                                   lambda = NULL,
                                   alphas = c(0.5))

meta_trained

