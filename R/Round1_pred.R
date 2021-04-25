#' @include helper_functions.R

# Predicting the outcome of tournaments - Round 1
path = 'data/Data_2002_19.rda'
load(path)

year <- "2017"

# define the data
# x defines the season-long stats only
y_train <- Data_2002_19$rd_1[which(Data_2002_19[, 2] <= as.character(as.numeric(year) - 1))]
x_train <- Data_2002_19[which(Data_2002_19[, 2] <= as.character(as.numeric(year) - 1)),
                        c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_19$rd_1[which(Data_2002_19[, 2] == "2017")]
x_test <- Data_2002_19[which(Data_2002_19[, 2] == "2017"), c(4, seq(12, 38, by = 2))]

x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
tau <- 0.1
C = 2 # number of classification groups
classes = c(0, 1) # classification groups

# descretize y_train to be applicable to the cqs function
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
out <- quantdr::cqs(x_train, y_train_dis, tau)
dtau_hat <- out$dtau
# the BBQ.grplasso algorithm requires at least two predictor variables
if (dtau_hat < 2) {
  dtau_hat = 2
}
beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

# define the new sufficient predictors
new_data_train <- x_train %*% beta_hat
new_data_test <- x_test %*% beta_hat

# run the classification
fit <- BBQ.grplasso(y_train ~ new_data_train, tau, c(1, 2), method = 'Binary', Run = 1500,
                    burn = 500, Ce = 0, scale = TRUE)
prob_fit <- BBQ.prob(fit, new_data_test)$p1x



