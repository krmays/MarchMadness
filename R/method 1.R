source('R/helper_functions.R')

### Predicting the outcome of tournaments - Round 1
path = 'data/Data_2002_21.rda'
load(path)

season <- "2012"

# define the data
# x defines the last 3 game stats only
y_train <- Data_2002_21$rd_1[which(Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)),
                        c(4, seq(13, 39, by = 2))]
y_test <- Data_2002_21$rd_1[which(Data_2002_21[, 2] == as.character(season))]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season)), c(4, seq(13, 39, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
taus = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
prob_matrix <- matrix(0, length(taus), n_test)
C = 2 # number of classification groups
classes = c(0, 1) # classification groups

# discretize y_train to be applicable to the cqs function
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
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
  fit <- BBQ.grplasso(y_train ~ new_data_train, taus[j], c(1, 2), method = 'Binary', Run = 1500,
                      burn = 500, Ce = 0, scale = TRUE)

  # In model.matrix.default(mt, mf, contrasts) : non-list contrasts argument ignored
  prob_fit <- BBQ.prob(fit, new_data_test)$p1x
  prob_matrix[j, ] <- prob_fit
}

# find average across quantiles
avg_prob <- apply(prob_matrix, 2, mean)

# arrange data by game matchup
colnames(Data_2002_21)[1] <- "team" #rename the first column
Rd1 <- subset(Data_2002_21, year == season, c("region", "team", "seed")) #pull out only the columns we need to set up game matrix
Rd1 <- dplyr::arrange(Rd1, region, seed) #make sure that teams really are arranged by seed
game.order <- rep(c(1:8, 8:1), 4)
Rd1$game = game.order
Rd1$prob = avg_prob
Rd1 <- Rd1[, c("region", "game", "seed", "team", "prob")]
Rd1 <- dplyr::arrange(Rd1, region, game, -prob)
win <- rep(c(1:0), 32)
Rd1$win <- rep(c(1:0), 32)

# pull out winners only
Rd1_winners <- subset(Rd1, Rd1$win == 1)
Rd1_winners

### Round 2

# define the data
# x defines the season-long stats only
y_train <- Data_2002_21$rd_2[which(Data_2002_21[, 6] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 6] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)),
                        c(4, seq(13, 39, by = 2))]
y_test <- Data_2002_21$rd_2[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd1_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd1_winners$team),
                       c(4, seq(13, 39, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
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
  fit <- BBQ.grplasso(y_train ~ new_data_train, taus[j], c(1, 2), method = 'Binary', Run = 1500,
                      burn = 500, Ce = 0, scale = TRUE)

  # In model.matrix.default(mt, mf, contrasts) : non-list contrasts argument ignored
  prob_fit <- BBQ.prob(fit, new_data_test)$p1x
  prob_matrix[j, ] <- prob_fit
}

# find average across quantiles
avg_prob2 <- apply(prob_matrix, 2, mean)

# arrange games by matchup
Rd2 <- Rd1_winners[, c(1, 3:4)]
game.order <- rep(c(1:4, 4:1), 4)
Rd2$game = game.order
Rd2$prob = avg_prob2
Rd2 <- Rd2[, c("region", "game", "seed", "team", "prob")]
Rd2 <- dplyr::arrange(Rd2, region, game, -prob)
win2 <- rep(c(1:0), 16)
Rd2$win <- rep(c(1:0), 16)

#Rd2 win predictions
Rd2_winners <- subset(Rd2, Rd2$win == 1)
Rd2_winners

### Round 3

# define the data
# x defines the season-long stats only
y_train <- Data_2002_21$rd_3[which(Data_2002_21[, 7] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 7] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)),
                        c(4, seq(13, 39, by = 2))]
y_test <- Data_2002_21$rd_3[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd2_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd2_winners$team),
                       c(4, seq(13, 39, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

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
avg_prob3 <- apply(prob_matrix, 2, mean)

# arrange games by matchup
Rd3 <- Rd2_winners[, c(1, 3:4)]
game.order <- rep(c(1:2, 2:1), 4)
Rd3$game <- game.order
Rd3$prob <- avg_prob3
Rd3 <- Rd3[, c("region", "game", "seed", "team", "prob")]
Rd3 <- dplyr::arrange(Rd3, region, game, -prob)
win3 <- rep(c(1:0), 8)
Rd3$win <- rep(c(1:0), 8)

# Rd3 win predictions
Rd3_winners <- subset(Rd3, Rd3$win == 1)
Rd3_winners

### Rounds 4-6

# Round 4 winners using Rd3 probabilities
Rd4 <- Rd3_winners[, c(1, 3:4)]
game.order <- rep(c(1:4), each = 2)
Rd4$game <- game.order
Rd4$prob <- subset(Rd3$prob, Rd3$win == 1)
Rd4 <- Rd4[, c("region", "game", "seed", "team", "prob")]
Rd4 <- dplyr::arrange(Rd4, region, game, -prob)
Rd4$win <- rep(c(1:0), 4)

Rd4_winners <- subset(Rd4, Rd4$win == 1)
Rd4_winners

# Round 5 using Rd3 probabilities
Rd5 <- Rd4_winners[, c(1, 3:4)]
game.order <- rep(c(1:2, 2:1))
Rd5$game <- game.order
Rd5$prob <- subset(Rd4$prob, Rd4$win == 1)
Rd5 <- Rd5[, c("region", "game", "seed", "team", "prob")]
Rd5 <- dplyr::arrange(Rd5, game, -prob)
Rd5$win <- rep(c(1:0), 2)

Rd5_winners <- subset(Rd5, Rd5$win == 1)
Rd5_winners

# Round 6 using Rd3 probabilities
Rd6 <- Rd5_winners[, c(1, 3:4)]
Rd6$prob <- subset(Rd5$prob, Rd5$win == 1)
Rd6 <- Rd6[, c("region", "seed", "team", "prob")]
Rd6 <- dplyr::arrange(Rd6, -prob)
Rd6$win <- rep(c(1:0))

Rd6_winner <- subset(Rd6, Rd6$win == 1)
Rd6_winner
