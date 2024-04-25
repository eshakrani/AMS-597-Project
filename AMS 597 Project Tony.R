#AMS 597 Project Tony
simple_linear_regression <- function(X, y) {
  # Add intercept term to X
  X <- cbind(1, X)
  
  # Compute beta_hat (coefficients) using matrix operations
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  # Return coefficients
  return(beta_hat)
}
###########################################################
#Logistic Regression 
#Theory from: https://thelaziestprogrammer.com/sharrington/math-of-machine-learning/solving-logreg-newtons-method
logistic <- function(x) {
  return(1 / (1 + exp(-x)))
}
logistic_regression_newton <- function(X, y, max_iter = 100, tol = 1e-6) {
  X <- cbind(1, X)
  beta <- rep(0, ncol(X))
  for (i in 1:max_iter) {
    p <- logistic(X %*% beta)
    
    gradient <- colSums(X * (y - p))
    
    W <- diag(p * (1 - p))
    hessian <- t(X) %*% W %*% X
    
    beta_new <- beta + solve(hessian) %*% gradient
    if (max(abs(beta_new - beta)) < tol) {
      break
    }
    beta <- beta_new
  }
  
  return(beta)
}
#########################################################
#Ridge Regression: https://online.stat.psu.edu/stat857/node/155/
ridge_regression <- function(X, y, lambda) {
  X <- cbind(1, X)
  
  beta <- solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y
  
  return(beta)
}

#######################################################
#https://arxiv.org/pdf/2303.03576.pdf#:~:text=In%20this%20section%2C%20we%20presents,path%20following%20algorithm%20(PFA).
#Algorithm 3: Coordinate Gradient Descent Algorithm (CGDA) 
lasso_regression <- function(X, y, lambda, max_iter = 100, tol = 1e-6) {
  X <- cbind(1, X)
  
  beta <- rep(0, ncol(X))
  
  XtX <- t(X) %*% X
  
  for (iter in 1:max_iter) {
    beta_old <- beta
    for (j in 1:length(beta)) {
      r <- y - X %*% beta + X[, j] * beta[j]
      beta[j] <- sign(sum(X[, j] * r)) * max(0, abs(sum(X[, j] * r)) - lambda) / sum(X[, j]^2)
    }
    if (max(abs(beta - beta_old)) < tol) {
      break
    }
  }
  return(beta)
}
########################################################################
#E Net 
elastic_net_regression <- function(X, y, lambda1, lambda2, alpha, max_iter = 100, tol = 1e-6) {
  data <- checkD
  
  beta <- rep(0, ncol(X))
  
  XtX <- t(X) %*% X
  
  for (iter in 1:max_iter) {
    beta_old <- beta
    
    for (j in 1:length(beta)) {
      r <- y - X %*% beta + X[, j] * beta[j]
      if (j == 1) {
        beta[j] <- sum(X[, j] * r) / (sum(X[, j]^2) + lambda2)
      } else {
        beta[j] <- sign(sum(X[, j] * r)) * max(0, abs(sum(X[, j] * r)) - lambda1 * alpha) / (sum(X[, j]^2) + lambda2 * (1 - alpha))
      }
    }
    
    if (max(abs(beta - beta_old)) < tol) {
      break
    }
  }
  return(beta)
}

####################################
#Binary Check of Data 
is_binary <- function(vector) {
  unique_values <- unique(vector)
  if (length(unique_values) != 2) {
    return(FALSE)
  }
  if (!(is.character(unique_values))) {
    return(FALSE)
  }
  if (sum(unique_values %in% c("0", "1")) == 2) {
    return(TRUE)
  }
  return(FALSE)
}
##################################
#Using all glmnet 
linear_regression_glmnet <- function(X, y) {
  model <- glmnet(X, y, alpha = 0, lambda = 0)
  return(coef(model))
}

ridge_regression_glmnet <- function(X, y, lambda) {
  model <- glmnet(X, y, alpha = 0, lambda = lambda)
  return(coef(model))
}

lasso_regression_glmnet <- function(X, y, lambda) {
  model <- glmnet(X, y, alpha = 1, lambda = lambda)
  return(coef(model))
}

elastic_net_glmnet <- function(X, y, lambda, alpha) {
  model <- glmnet(X, y, alpha = alpha, lambda = lambda)
  return(coef(model))
}
####################################
#SVM 
#From: https://www.youtube.com/watch?v=QkAmOb1AMrY&t=20s&ab_channel=Simplilearn
#For Continuous continuous response variable: type = "eps-regression"
#For binary response variable: type = "C" or "nu"

#Q: What is the difference between nu-SVC and C-SVC? ( https://stats.stackexchange.com/questions/237382/difference-between-the-types-of-svm ) 
# A: Basically they are the same thing but with different parameters. 
# The range of C is from zero to infinity but nu is always between [0,1]. 
# A nice property of nu is that it is related to the ratio of support vectors 
# and the ratio of the training error.
svm_model <- function(X, y, type = "C", kernel = "radial", cost = 1) {
  require(e1071)
  
  # Train SVM model
  if (type == "C") {
    model <- svm(y ~ ., data = as.data.frame(cbind(y, X)), type = type, kernel = kernel, cost = cost)
  } else if (type == "nu") {
    model <- svm(y ~ ., data = as.data.frame(cbind(y, X)), type = type, kernel = kernel)
  }
  
  return(model)
}

##################################### 
# Top K with Bagging and Lasso Regression
# Use bagging to fit multiple lasso regression models on bootstrap samples of the data. 
# Then, we combine the coefficients from these models and select the top K predictors based on their average coefficient magnitudes
# We rank the predictors based on their average coefficient magnitudes and select the top K predictors
bagged_lasso <- function(X, y, num_models = 100) {
  n <- nrow(X)
  p <- ncol(X)
  coefficients <- matrix(0, nrow = p, ncol = num_models)
  
  for (i in 1:num_models) {
    indices <- sample(1:n, replace = TRUE) # Sample with replacement
    X_boot <- X[indices, ]
    y_boot <- y[indices]
    
    # Lasso
    model <- glmnet(X_boot, y_boot, alpha = 1)
    coefficients[, i] <- coef(model)[-1]  # Don't need intercept
  }
  
  return(coefficients)
}

# Function to select top K predictors based on coefficient magnitudes
top_K_predictors <- function(coefficients, K) {
  avg_coefficients <- apply(coefficients, 1, mean)
  top_K_indices <- order(abs(avg_coefficients), decreasing = TRUE)[1:K]
  return(top_K_indices)
}
#####################################################
##########################################
#Bagging for linear, logistic,  ridge, lasso , and elastic net
#Can change for each regression 
# final_predictions <- bagging_and_variable_importance(X, y, model_func = lm)
# final_predictions <- bagging_and_variable_importance(X, y, model_func = glm)
# final_predictions <- bagging_and_variable_importance(X, y, model_func = ridge_regression_glmnet)
# NOTE: Might need to adjust for the main function because of layout 


# Steps: 
# 1. Sample with replacement from the dataset to create multiple bootstrap samples.
# 2. Fit the chosen regression model to each bootstrap sample.
# 3. Predict the outcomes for each model.
# 4. Average the predictions across all models to obtain the final prediction. (Soft Voting)
# 5. Count the number of times each variable is selected in the bagging process. Variables that are selected more frequently are considered more important.

# Function to perform bagging for regression models and calculate variable importance score
#NOTE: Might need to adjust for the main function because of layout 
bagging_and_variable_importance <- function(X, y, model_func, num_models = 100) {
  n <- nrow(X)
  predictions <- matrix(0, nrow = n, ncol = num_models)
  
  for (i in 1:num_models) {
    indices <- sample(1:n, replace = TRUE) #Sample with replacement
    X_boot <- X[indices, , drop = FALSE]  # Ensure X_boot is a data frame
    y_boot <- y[indices]
    
    # Combine X_boot and y_boot into a data frame in order for model_func to work 
    boot_data <- cbind(X_boot, y_boot)
    
    model <- model_func(y_boot ~ ., data = boot_data)  # Fit the model
    
    predictions[, i] <- predict(model, newdata = X)
    
    if (i == 1) {
      naive <- as.numeric(coef(model) != 0)
    } else {
      naive <- naive + as.numeric(coef(model) != 0)
    }
  }
  
  # Average predictions across all models (Soft Voting)
  final_predictions <- rowMeans(predictions)
  
  return(list(predictions = final_predictions, naive = naive))
}
#Example of Bagging 
data("ethanol")
ethanol
str(ethanol)
bagging_and_variable_importance(X = ethanol[, -3], y = ethanol$E, model_func = lm, num_models = 100)

############################
#Ensemble - Stacking 
#https://medium.com/@brijesh_soni/stacking-to-improve-model-performance-a-comprehensive-guide-on-ensemble-learning-in-python-9ed53c93ce28
#Steps: 
# 1. Split the training dataset into two parts: training and test
# 2. Train several base models on the training data
# 3. Make predictions using the base models on the hold-out validation data
# 4. Train the meta-model on the hold-out validation data using the predictions from the base models as input features
# 5. To make a prediction for new data

library(randomForest)
final_predictions <- c()
split_data <- function(X, y, test_size = 0.2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(X)
  n_test <- as.integer(n * test_size)
  test_indices <- sample(1:n, n_test)
  train_indices <- setdiff(1:n, test_indices)

  X_train <- X[train_indices, , drop = FALSE]
  y_train <- y[train_indices]
  X_test <- X[test_indices, , drop = FALSE]
  y_test <- y[test_indices]

  return(list(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test))
}
# Ensemble_stacking function
ensemble_stacking <- function(X_train, y_train, X_test, models) {
  # Fit base models
  base_model_predictions <- lapply(models, function(model) {
    if (inherits(model, "lm") || inherits(model, "glm")) {
      predict(model, newdata = as.data.frame(X_train))
    } else if (inherits(model, "randomForest")) {
      predict(model, newdata = X_train)
    } else {
      stop("Unsupported model type.")
    }
  })

  # Combine predictions into a data frame
  base_model_predictions <- as.data.frame(do.call(cbind, base_model_predictions))

  # Train meta-learner (e.g., linear regression) on base model predictions
  meta_learner <- lm(y_train ~ ., data = base_model_predictions)

  # Generate predictions from base models for test data
  base_model_predictions_test <- lapply(models, function(model) {
    if (inherits(model, "lm") || inherits(model, "glm")) {
      predict(model, newdata = as.data.frame(X_test))
    } else if (inherits(model, "randomForest")) {
      predict(model, newdata = X_test)
    } else {
      stop("Unsupported model type.")
    }
  })

  # Combine predictions into a data frame
  base_model_predictions_test <- as.data.frame(do.call(cbind, base_model_predictions_test))

  # Make final predictions using meta-learner
  final_predictions <- predict(meta_learner, newdata = base_model_predictions_test)

  return(final_predictions)
}
#######################
# Example usage - EMSABLE STACKING 
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- rnorm(n)

# Split data into training and testing sets
data_split <- split_data(X, y, test_size = 0.2, seed = 123)
X_train <- data_split$X_train
y_train <- data_split$y_train
X_test <- data_split$X_test

# Fit base models
model_lm <- lm(y_train ~ ., data = as.data.frame(X_train))
model_rf <- randomForest(x = X_train, y = y_train)
model_glm <- glm(y_train ~ ., data = as.data.frame(X_train), family = gaussian)

# Use ensemble stacking
models <- list(model_lm, model_rf, model_glm)
ensemble_stacking(X_train, y_train, X_test, models)



######################################
#Esamble  
# Define X and y
X <- ethanol[, -1]  # Excluding the first column (NOx) as it's the target variable
y <- ethanol$NOx

# Split the data into training and testing sets
data_split <- split_data(X, y, test_size = 0.2, seed = 123)
X_train <- data_split$X_train
y_train <- data_split$y_train
X_test <- data_split$X_test
y_test <- data_split$y_test

# Fit base models
model_lm <- lm(y_train ~ ., data = as.data.frame(X_train))
model_rf <- randomForest(x = X_train, y = y_train)

# Use ensemble stacking
models <- list(model_lm, model_rf)
ensemble_stacking(X_train, y_train, X_test, models)

######
# Fit base models
model_lm <- lm(y ~ ., data = as.data.frame(X))
model_rf <- randomForest(x = X, y = y)

# Use ensemble stacking
models <- list(model_lm, model_rf)
ensemble_stacking(X, y, X, models)


##############################################################################
# Ensemble - Using Stacking from https://medium.com/@brijesh_soni/stacking-to-improve-model-performance-a-comprehensive-guide-on-ensemble-learning-in-python-9ed53c93ce28

stacked_ensemble_and_aggregate <- function(data, folds, base_models) {
  stacked_predictions_list <- list()
  
  for (i in 1:length(folds)) {
    valid_data <- data[folds[[i]]$test, ]
    train_data <- data[unlist(folds[-i], use.names = FALSE), ]
    
    # Train base models
    base_model_predictions <- lapply(base_models, function(model) {
      predict(model, newdata = valid_data)
    })
    
    # Combine predictions into meta-model input
    input_meta_model <- data.frame(do.call(cbind, base_model_predictions), Rings = valid_data$Rings)
    
    # Train the meta-model based on stacked outputs of base models 
    stacked_model <- lm(Rings ~ ., data = input_meta_model)
    
    # Make predictions on test data
    test_base_model_predictions <- lapply(base_models, function(model) {
      predict(model, newdata = data)
    })
    stacked_model_input <- data.frame(do.call(cbind, test_base_model_predictions), Rings = data$Rings)
    stacked_predictions <- predict(stacked_model, newdata = stacked_model_input)
    
    # Store stacked predictions in the list
    stacked_predictions_list[[i]] <- stacked_predictions
  }
  
  # Initialize a list to store aggregated predictions
  aggregated_predictions <- list()
  
  # Average predictions across folds
  for (i in 1:length(stacked_predictions_list[[1]])) {
    instance_predictions <- sapply(stacked_predictions_list, function(predictions) predictions[i])
    avg_prediction <- mean(instance_predictions)
    aggregated_predictions[[i]] <- avg_prediction
  }
  
  # Convert aggregated predictions to a vector
  aggregated_predictions_vector <- unlist(aggregated_predictions)
  
  return(aggregated_predictions_vector)
}


data(abalone)

abalone <- head(abalone, 100)
library(caret)
library(randomForest)
# Create folds for cross-validation
folds <- createFolds(abalone$Rings, k = 10, list = TRUE)

lm_model <- lm(Rings ~ ., data = abalone)
svm_model <- svm(Rings ~ ., data = abalone, method = "Regression", kernel = "radial", gamma = 0.1, cost = 10)
tree_model <- rpart(formula = as.formula("Rings ~ ."), data = abalone)
randomf_model <- randomForest( x = abalone[, -1 ], y = abalone$Rings  )




base_models <- list(lm_model, svm_model, tree_model)

stacked_ensemble_and_aggregate(abalone, folds, base_models)

