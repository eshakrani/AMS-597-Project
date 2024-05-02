library(glmnet)


#' @param X Input DataFrame of co-variates, n by p.
#' @param y Response DataFrame, n by 1.
#' @param family Output distribution of the y vector (gaussian or binomial)
#' @param lambda Regularization parameter(s).
#' @param alpha ElasticNet mixing parameter(s).
#' @param bagging Logical
#' @param topP Indicating whether to perform feature pre screening
#' @param K Number of top predictors to use.
#' @param ensemble Logical indicating whether to use ensemble learning.
#' @param nfolds controls the number of folds for cross validated models
#' @param test_size controls the test set size for model selection in cross validated models
#' @param R Number of iterations for bootstrap
#' @param models_list List of models to use during ensemble to train the meta learner.
#' Either pick a singular model ('svm' or 'randomForest') or both c('svm','randomForest')
#'
#' @details
#' \bold{Input Warningss}
#' Bagging and ensemble should not be activated at the same time. This function will
#' does not support bagging ensemble models.
#'
#' \bold{Regular Functionality without Bagging and Ensemble}
#' This function fits a model based on specified input parameters. To get an OLS
#' fit or regular logistic regression fit, input lambda = 0 and alpha as any value
#' between 0 and 1. If lambda is a singular value that is not 0, and alpha is a singular value
#' the model will be fitted with glmnet and returned.
#'
#' If lambdas and alphas are both an appropriate list the model will be cross validated
#' and hyper parameter searched, then returned. Logistic Regression models are compared
#' by ROC and linear regression models are compared by MSE on the test set.
#'
#' \bold{Bagging Functionality}
#' If Bagging is enabled the same logic as previous is applied. So for each iteration
#' of bootstrap cross validation and hyper parameter search may be performed which can be
#' costly. A naive score of what percent of coefficients appeared as non 0 will
#' be returned along with the bagged predictions.
#'
#' For regression a average of the output is taken, and for classification a
#' average of the predicted probability is taken.
#'
#' \bold{Ensemble Functionality}
#' Stacking is utilized to create an ensemble learned. One or two learners can
#' be created (limited to SVM and RF) then used to help train a meta learner
#' which can be a: GLM, SVM or RF. The final meta learner is returned.
#'
#' \bold{Top Predictors (TopP)}
#' In cases of needing to pre screen variables (p >> n) the TopP utilizes
#' bootstrapped cross validated (over lambda) lasso fits to determine how often
#' coefficients are retained during fits. These are ranked and printed. Only the
#' top K covariates will be used in the model. If there are more than K covariates
#' that appear 100% of the time a warning will be issued.
#'
#'
#'
modelFit <- function(X, y, family = c("gaussian","binomial"), 
                     lambda = c(), alpha = c(), bagging = FALSE, topP = FALSE, 
                     K = 10, ensemble = FALSE, models_list = c("svm","randomForest"),
                     meta_learner = c("glm","svm","randomForest"), nfolds = 5, 
                     test_size = 0.2, R = 100) {

  
  data <- checkData(X, y)
  X <- data[,-1]
  y <- data[,1]

  checkAssumptions(family = family, lambda = lambda, alpha = alpha
                   , bagging = bagging, topP = topP, K = K, ensemble = ensemble
                   , R = R, meta_learner = meta_learner)
  # Check assumptions was having issues, will fix later.
  
  if (identical(family, c("gaussian","binomial"))) {
    family <- 'gaussian'
  }
  
  
  if (topP) {

    covariates_list <- topPredictors(data.frame(X), y, family = family, alpha = 1, lambda = NULL
                                     , test_size = test_size, R = 100, K = K)
    cat("The top covariates found are: ", covariates_list$top_covariates)

    X <- X[,covariates_list$top_covariates]
  }
  
  if (!bagging) {
    if (ensemble) {
      if(length(unique(y)) == 2)
        y <- factor(y)
      
      model <- train_meta_learner(models_list, meta_learner, X = X, y = y, alpha, lambda)
      return(model)
    }
    
    
    else if (family == 'gaussian') {
      if (!is.null(lambda) && length(lambda) == 1 && lambda == 0) {
        model <- lm('y ~ .', data = cbind(y,X))
        return(model)
      }
      else if (!is.null(alpha) && !is.null(lambda) && length(alpha) == 1) {
        model <- glmnet(X, y, alpha = alpha, lambda = lambda, family = family)
        return(model)
      }
      
      else {
        results <- fitLinearRegressor(X, y, alpha = alpha, lambda = lambda,
                                    nfolds = nfolds, test_size = test_size)
        return(results)
      }
      
    }
    
    else if (family == 'binomial') {
      y <- factor(y)
      X <- as.matrix(X)
      if (is.null(alpha) && !is.null(lambda) && lambda == 0) {
        
        model <- glm(y ~ X, family = family)
        return(model)
      }
      
      else if (!is.null(alpha) && length(alpha) == 1) {
        model <- glmnet(X, y, alpha = alpha, lambda = lambda, family = family)
        return(model)
      }
      
      else {
        results <- fitLinearClassification(X, y, alpha = alpha, lambda = lambda,
                                         nfolds = nfolds, test_size = test_size)
        return(results)    
      }
    }
  
  }
  
  if (bagging) {
    if (family == 'gaussian') {
      y_pred_avg <- rep(0, length(y))
      naive_score <- matrix(rep(0,ncol(X) + 1),nrow = 1, ncol = ncol(X) + 1)
      colnames(naive_score) <- c("Intercept",colnames(X))
      
      for (i in 1:R) {
        id <- sample(1:nrow(X), nrow(X), replace = TRUE)
        X_bootstrap <- X[id,]
        y_bootstrap <- y[id]
        model <- NULL
        
          if (lambda == 0 && is.null(alpha)) {
            model <- lm(y_bootstrap ~ ., data = cbind(y_bootstrap, X_bootstrap))
            y_pred <- predict(model, newdata = data.frame(X))
            y_pred_avg <- y_pred_avg + 1/R * y_pred
            non_zero_coeffs <- which(coef(model) != 0)
            naive_score[1,non_zero_coeffs] <- naive_score[1,non_zero_coeffs] + 1/R
            
            next
          }
          else if(!is.null(alpha) && length(alpha) == 1){
            model <- glmnet(X_bootstrap, y_bootstrap, alpha = alpha, lambda = lambda, family = family)
            y_pred <- predict(model, newx = as.matrix(X), type = 'response',s = lambda)
            y_pred_avg <- y_pred_avg + 1/R * y_pred
            
            non_zero_coeffs <- which(coef(model, s = lambda) != 0)
            naive_score[1,non_zero_coeffs] <- naive_score[1,non_zero_coeffs] + 1/R
            }
        else {
          results <- fitLinearRegressor(X_bootstrap, y_bootstrap, alpha = alpha,
                                        lambda = lambda, family = family, test_size = test_size)
          
          y_pred <- predict(results$model, newx = as.matrix(X), s = results$lambda)
          y_pred_avg <- y_pred_avg + 1/R * y_pred
          
          non_zero_coeffs <- which(coef(results$model, s = results$lambda) != 0)
          naive_score[1,non_zero_coeffs] <- naive_score[1,non_zero_coeffs] + 1/R
        }

      }
      return(list(y_pred_avg = y_pred_avg, naive_score = naive_score))
    }
    
    if (family == 'binomial') {
      y <- factor(y)
      y_pred_avg <- rep(0, length(y))
      naive_score <- matrix(rep(0,ncol(X) + 1),nrow = 1, ncol = ncol(X) + 1)
      colnames(naive_score) <- c("Intercept",colnames(X))
      
      for (i in 1:R) {
        id <- sample(1:nrow(X), nrow(X), replace = TRUE)
        X_bootstrap <- X[id,]
        y_bootstrap <- y[id]
        model <- NULL
        
        if (!is.null(alpha) && !is.null(lambda) && length(lambda) == 1 && length(alpha)) {
          if (lambda == 0) {
            model <- glm(y_bootstrap ~ X_bootstrap, family = family)
            y_pred <- predict(model, newdata = data.frame(X), type = 'response')
            y_pred_avg <- y_pred_avg + 1/R * y_pred
            non_zero_coeffs <- which(coef(model) != 0)
            naive_score[1,non_zero_coeffs] <- naive_score[1,non_zero_coeffs] + 1/R
          
            next
          }
          else {
            model <- glmnet(X_bootstrap, y_bootstrap, alpha = alpha, lambda = lambda, family = family)
            y_pred <- predict(model, newx = X, type = 'response',s = lambda)
            y_pred_avg <- y_pred_avg + 1/R * y_pred
            
            non_zero_coeffs <- which(coef(model, s = lambda) != 0)
            naive_score[1,non_zero_coeffs] <- naive_score[1,non_zero_coeffs] + 1/R
          }
        }
        else {
          results <- fitLogisticRegressor(X_bootstrap, y_bootstrap, alpha = alpha, lambda = lambda,
                                          family = family, test_size = test_size)
          y_pred <- predict(results$model, newx = as.matrix(X), type = 'response',s = results$lambda)
          y_pred_avg <- y_pred_avg + 1/R * y_pred
          
          non_zero_coeffs <- which(coef(results$model, s = results$lambda) != 0)
          naive_score[1,non_zero_coeffs] <- naive_score[1,non_zero_coeffs] + 1/R
        }
        
      }
      
      return(list(y_pred_avg = y_pred_avg, naive_score = naive_score))
      }
    }
    
}

#' @name 
#' Fit Linear Regression
#' 
#' @description 
#' This function searches for the best fit linear regression model using:
#' elastic net penalty, cross validation, train-test split, and grid search.
#' All model fitting is done using glmnet.  
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' @param test_size Size of the test set
#' 
#' @return Best fit model over a grid search of lambdas and alphas using cv.glmnet
#' 
#' @import glmnet
#' 
#' @export

fitLinearRegressor <- function(X, y, lambda = NULL, alphas, nfolds = 5, test_size = 0.2, family = 'gaussian') {
  require(glmnet)
  X <- as.matrix(X)
  
  n <- nrow(X)
  n_train <- floor((1 - test_size) * n)
  train_indices <- sample(1:n, n_train)
  
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]
  
  best_model <- NULL
  best_cv_error <- Inf
  final_model <- NULL
  best_alpha <- NULL
  
  for (alpha in alphas) {

    cvfit <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambda, nfolds = nfolds, family = family) # Cross validation
    
    lambda_best <- cvfit$lambda.min
    
    # Fit model using best lambda
    model <- glmnet(X_train, y_train, alpha = alpha, lambda = lambda_best, family = family)
    
    # Predict on test set
    y_pred <- predict(model, newx = X_test)
    
    # Calculate mean squared error on test set
    cv_error <- mean((y_test - y_pred)^2)
    
    # Update best model if current model has lower error
    if (cv_error < best_cv_error) {
      best_model <- model
      best_cv_error <- cv_error
      best_alpha <- alpha
    }
    
    final_model <- glmnet(X, y, alpha = best_alpha, lambda = best_model$lambda)
    
  }
  
  return(list(model = final_model, lambda = best_model$lambda, alpha = best_alpha))
}




#' @name 
#' Fit Logistic Regression
#' 
#' @description 
#' This function searches for the best fit logistic regression model using:
#' elastic net penalty, cross validation, train-test split, and grid search.
#' All model fitting is done using glmnet.  
#' 
#' @param X The matrix of predictor variables.
#' @param y The vector of response variable.
#' @param loss The loss function to be used. Currently not used in the function, but included for potential future extensions.
#' @param lambda The penalty parameter controlling the amount of regularization. Default is 0 (no regularization).
#' @param alphas A numeric vector specifying the mixing parameter for elastic net regularization. 
#'        Alpha = 1 corresponds to Lasso regression, Alpha = 0 corresponds to Ridge regression, 
#'        and values in between represent a combination of Lasso and Ridge. 
#' @param nfolds Number of folds for cross-validation. Default is 5.
#' @param test_size Size of the test set
#' 
#' @return Best fit model over a grid search of lambdas and alphas using cv.glmnet
#' 
#' @import glmnet
#' 
#' @export

fitLogisticRegressor <- function(X, y, loss, lambda = NULL, alphas, nfolds = 5,  test_size = 0.2,family = 'binomial') {
  require(glmnet)
  require(pROC)
  X <- as.matrix(X)
  
  n <- nrow(X)
  n_train <- floor((1 - test_size) * n)
  train_indices <- sample(1:n, n_train)
  
  X_train <- X[train_indices, ]
  y_train <- y[train_indices]
  
  X_test <- X[-train_indices, ]
  y_test <- y[-train_indices]
  
  best_model <- NULL
  best_cv_error <- Inf
  best_auc = 0
  final_model <- NULL
  best_alpha <- NULL
  
  for (alpha in alphas) {
    
    cvfit <- cv.glmnet(X_train, y_train, alpha = alpha, lambda = lambda, 
                       nfolds = nfolds, family = 'binomial') # Cross validation
    
    lambda_best <- cvfit$lambda.min
    
    # Fit model using best lambda
    model <- glmnet(X_train, y_train, alpha = alpha, lambda = lambda_best,
                    family = 'binomial')
    
    
    # NEEDS TO BE FIXED, has to calculate non MSE loss metric.
    # Predict on test set
    #y_pred <- predict(model, newx = X_test)
    
    # More info: https://www.geeksforgeeks.org/how-to-calculate-auc-area-under-curve-in-r/
    # AUC is between 0 and 1, higher is better.
    
    y_pred = predict(model, newx = X_test)
    roc_object = roc(y_test, y_pred, quiet = T)
    auc = auc(roc_object, quiet = T)
    
    if (auc > best_auc) {
      best_model <- model
      best_auc = auc
      best_alpha <- alpha
    }
    
    final_model <- glmnet(X, y, alpha = best_alpha, 
                          lambda = best_model$lambda, family = 'binomial')
    
  }
  
  return(list(model = final_model, lambda = best_model$lambda, alpha = best_alpha))
}


topPredictors <- function(X, y, family, alpha, lambda, R, K, test_size) {

  naive_score <- matrix(rep(0,ncol(X) + 1),nrow = 1, ncol = ncol(X) + 1)
  colnames(naive_score) <- c("Intercept",colnames(X))
  
  X <- as.matrix(X)
  for (i in 1:R) {
    id <- sample(1:nrow(X), nrow(X), replace = TRUE)
    X_bootstrap <- X[id, ]
    y_bootstrap <- y[id]
    
    results_bootstrap <- NULL
    if (family == "gaussian")
      results_bootstrap <- fitLinearRegressor(X_bootstrap, y_bootstrap, alpha = alpha,
                                              lambda = lambda, family = family, test_size = test_size)
                              
    else
      results_bootstrap <- fitLogisticRegressor(X_bootstrap, y_bootstrap, alpha = alpha,
                                              slambda = lambda, family = family, test_size = test_size)

    non_zero_coeffs <- which(coef(results_bootstrap$model, s= results_bootstrap$lambda) != 0)
    naive_score[non_zero_coeffs] <- naive_score[non_zero_coeffs] + 1/R
  }
  
  sorted_indices <- order(naive_score, decreasing = TRUE)
  top_covariates <- colnames(naive_score)[-1][sorted_indices][-1][1:K]  # Excluding the intercept 
  

  if (sum(naive_score[sorted_indices] != 0) > K) {
    warning("Model has more than K covariates that are not 0.")
  }
  
  return(list(naive_score = naive_score, top_covariates = top_covariates))
}