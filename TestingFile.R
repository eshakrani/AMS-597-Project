# Generate synthetic data
n <- 100
p <- 10
X <- matrix(rnorm(n * p), nrow = n)
beta <- matrix(rep(0, p))
beta[sample(1:p, 5)] <- 1  # True coefficients
y <- X %*% beta + rnorm(n)

# Test the topP function
K <- 3

#top_covariates <- topP(data.frame(X), y, family = "gaussian", alpha = c(0,1), lambda = NULL, test_size = 0.2, R = 100, K = K)

modelFit(data.frame(X), data.frame(y = y), family = "gaussian", alpha = 0.5, lambda = 0, nfolds = 5, test_size = 0.2, R = 100, K = K, bagging = F,
         topP = T)





################################
#Categorical Data testing

#X <- iris[iris$Species == 'setosa' | iris$Species == 'versicolor',c(1,2,3)]
#y <- iris[iris$Species == 'setosa' | iris$Species == 'versicolor',c(5)]
#y <- factor(y, levels = c('0', '1'))
#y <- iris[iris$Species == 'setosa' | iris$Species == 'versicolor',c(4)]



# Number of observations
n <- 1000

# Generate predictor variables
x1 <- rnorm(n, 10, 2)
x2 <- rnorm(n, 5, 1)
x3 <- rnorm(n, 8, 3)

# Generate outcome variable
logistic_function <- function(x1, x2, x3) {
  p <- 1 / (1 + exp(-(-2 + 0.5 * x1 + 0.3 * x2 - 0.2 * x3)))  # Log odds
  response <- rbinom(length(x1), 1, p)  # Generate binary response
  return(response)
}

y <- factor(logistic_function(x1, x2, x3))

# Create dataframe
data <- data.frame(x1 = x1, x2 = x2, x3 = x3, y = as.character(y))

# Display the first few rows of the dataframe
head(data)


y.pred <- modelFit(data[,c(1,2,3)],data.frame(y),ensemble = F, bagging = F, lambda = c(0.0), alpha = 0, R  = 20, family = 'binomial',
                   meta_learner = 'randomForest', models_list = "randomForest")

y.pred

