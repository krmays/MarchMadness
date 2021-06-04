## Round 6 using Rd3 probabilities (method 1) ##

Rd6 <- Rd5_winners[, c(1, 3:4)]
Rd6$prob <- subset(Rd5$prob, Rd5$win == 1)
Rd6 <- Rd6[, c("region", "seed", "team", "prob")]
library(dplyr)
Rd6 <- dplyr::arrange(Rd6, -prob)
Rd6$win <- rep(c(1:0))

Rd6_winner <- subset(Rd6, Rd6$win == 1)
Rd6_winner

## Round 6 independently (method 2) ##

source('R/helper_functions.R')

# Predicting the outcome of tournaments - Round 4
path = 'data/Data_2002_19.rda'
load(path)

season <- "2016"

# define the data
# x defines the season-long stats only
y_train <- Data_2002_19$rd_6[which(Data_2002_19[, 10] == 1 & Data_2002_19[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_19[which(Data_2002_19[, 10] == 1 & Data_2002_19[, 2] <= as.character(as.numeric(season) - 1)),
                        c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_19$rd_6[which(Data_2002_19[, 2] == as.character(season) & Data_2002_19[, 1] %in% Rd5_winners$team)]
x_test <- Data_2002_19[which(Data_2002_19[, 2] == as.character(season) & Data_2002_19[, 1] %in% Rd5_winners$team),
                       c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
taus = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
prob_matrix <- matrix(0, length(taus), n_test)
C <- 2 # number of classification groups
classes <- c(0, 1) # classification groups

# discretize y_train to be applicable to the cqs function
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  # the BBQ.grplasso algorithm requires at least two predictor variables
  if (dtau_hat < 2) {
    dtau_hat <- 2
  }
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # run the classification
  fit <- BBQ.grplasso(y_train ~ new_data_train, taus[j], c(1, 2), method = 'Binary', Run = 1500,
                      burn = 500, Ce = 0, scale = TRUE)

  # In model.matrix.default(mt, mf, contrasts) : non-list contrasts argument ignored
  prob_fit <- BBQ.prob(fit, new_data_test)$p1x
  prob_matrix[j,] <- prob_fit
}

# find average across quantiles
avg_prob6 <- apply(prob_matrix, 2, mean)

# arrange games by matchup
Rd6 <- Rd5_winners[, c(1, 3:4)]
Rd6$prob <- avg_prob6
Rd6 <- Rd6[, c("region", "seed", "team", "prob")]
library(dplyr)
Rd6 <- dplyr::arrange(Rd6, -prob)
Rd6$win <- rep(c(1:0))

Rd6_winner <- subset(Rd6, Rd6$win == 1)
Rd6_winner
