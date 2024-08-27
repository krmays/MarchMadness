### Predicting the outcome of March Madness tournament by round
path = 'data/Data_2002_23.rda'
load(path)

#declare the season for round 5 matchups between regions
season <- "2022"
#matchup options for Rd. 5: 1 (East v. Midwest), 2 (East v. South), 3 (East v. West)
#not needed since we won't be pairing by matchup
#matchup <- 1

## 2023 option option 2, 2022 option 3, 2019 option 3, 2018 option 1, 2017 option 3, 2016 option 1, 2015 option 2, 2021 option 3

#define the data, full season, no window
y_train <- Data_2002_23$rd_1[which(Data_2002_23[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_23[which(Data_2002_23[, 2] <= as.character(as.numeric(season) - 1)),
                        c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_23$rd_1[which(Data_2002_23[, 2] == as.character(season))]
x_test <- Data_2002_23[which(Data_2002_23[, 2] == as.character(season)), c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
incr <- 0.05
taus = seq(0.05, 0.95, by = incr)
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- 2 * sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
    if (ghat[i] < 0) ghat[i] = 0
    if (ghat[i] > 1) ghat[i] = 1
  }

  prob_matrix[j, ] <- ghat
}

# find average across quantiles
avg_prob <- apply(prob_matrix, 2, sum) * incr

# arrange data by game matchup
# not sure we need this colnames(Data_2002_23)[1] <- "team" #rename the first column
# commented out because we do not need to pair teams
# Rd1 <- subset(Data_2002_23, year == season, c("region", "team", "seed"))
# Rd1 <- dplyr::arrange(Rd1, region, seed) #make sure that teams really are arranged by seed
# game.order <- rep(c(1:8, 8:1), 4)
# Rd1$game = game.order
# Rd1$prob = avg_prob
# Rd1 <- Rd1[, c("region", "game", "seed", "team", "prob")]
# Rd1 <- dplyr::arrange(Rd1, region, game, -prob, seed)
# win <- rep(c(1:0), 32)
# Rd1$win <- rep(c(1:0), 32)
# Rd1$win <- win

# pull out winners only
# Rd1_winners <- subset(Rd1, Rd1$win == 1)
#Rd1_winners

# Add Rd1 results
win_prob <- subset(Data_2002_23, year == season, c("region", "team", "seed"))
win_prob <- dplyr::arrange(win_prob, region, seed)
win_prob$Rd1 = avg_prob

### Round 2

# define the data
y_train <- Data_2002_23$rd_2[which(Data_2002_23[, 6] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_23[which(Data_2002_23[, 6] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_23$rd_2[which(Data_2002_23[, 2] == as.character(season))]
x_test <- Data_2002_23[which(Data_2002_23[, 2] == as.character(season)), c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- 2 * sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
    if (ghat[i] < 0) ghat[i] = 0
    if (ghat[i] > 1) ghat[i] = 1
  }

  prob_matrix[j, ] <- ghat
}

# find average across quantiles
avg_prob2 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
# Rd2 <- Rd1_winners[, c(1, 3:4)]
# game.order <- rep(c(1:4, 4:1), 4)
# Rd2$game = game.order
# Rd2$prob = avg_prob2
# Rd2 <- Rd2[, c("region", "game", "seed", "team", "prob")]
# Rd2 <- dplyr::arrange(Rd2, region, game, -prob, seed)
# win2 <- rep(c(1:0), 16)
# Rd2$win <- rep(c(1:0), 16)

#Rd2 win predictions
# Rd2_winners <- subset(Rd2, Rd2$win == 1)
# Rd2_winners

# Add Rd2 results
win_prob$Rd2 = avg_prob2

### Round 3

# define the data
y_train <- Data_2002_23$rd_3[which(Data_2002_23[, 7] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_23[which(Data_2002_23[, 7] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_23$rd_3[which(Data_2002_23[, 2] == as.character(season))]
x_test <- Data_2002_23[which(Data_2002_23[, 2] == as.character(season)),
                       c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- 2 * sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
    if (ghat[i] < 0) ghat[i] = 0
    if (ghat[i] > 1) ghat[i] = 1
  }

  prob_matrix[j, ] <- ghat
}

# find average across quantiles
avg_prob3 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
# Rd3 <- Rd2_winners[, c(1, 3:4)]
# game.order <- rep(c(1:2, 2:1), 4)
# Rd3$game <- game.order
# Rd3$prob <- avg_prob3
# Rd3 <- Rd3[, c("region", "game", "seed", "team", "prob")]
# Rd3 <- dplyr::arrange(Rd3, region, game, -prob, seed)
# win3 <- rep(c(1:0), 8)
# Rd3$win <- rep(c(1:0), 8)

# Rd3 win predictions
# Rd3_winners <- subset(Rd3, Rd3$win == 1)
# Rd3_winners

# Add Rd3 results
win_prob$Rd3 = avg_prob3


### Round 4

# define the data
y_train <- Data_2002_23$rd_4[which(Data_2002_23[, 8] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_23[which(Data_2002_23[, 8] ==1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_23$rd_4[which(Data_2002_23[, 2] == as.character(season))]
x_test <- Data_2002_23[which(Data_2002_23[, 2] == as.character(season)),
                       c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- 2 * sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
    if (ghat[i] < 0) ghat[i] = 0
    if (ghat[i] > 1) ghat[i] = 1
  }

  prob_matrix[j, ] <- ghat
}

# find average across quantiles
avg_prob4 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
# Rd4 <- Rd3_winners[, c(1, 3:4)]
# game.order <- rep(c(1:4), each = 2)
# Rd4$game <- game.order
# Rd4$prob <- avg_prob4
# Rd4 <- Rd4[, c("region", "game", "seed", "team", "prob")]
# Rd4 <- dplyr::arrange(Rd4, region, game, -prob, seed)
# Rd4$win <- rep(c(1:0), 4)

# Rd4_winners <- subset(Rd4, Rd4$win == 1)
# Rd4_winners

# Add Rd4 results
win_prob$Rd4 = avg_prob4


### Round 5

# define the data
y_train <- Data_2002_23$rd_5[which(Data_2002_23[, 9] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_23[which(Data_2002_23[, 9] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_23$rd_5[which(Data_2002_23[, 2] == as.character(season))]
x_test <- Data_2002_23[which(Data_2002_23[, 2] == as.character(season)),
                       c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- 2 * sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
    if (ghat[i] < 0) ghat[i] = 0
    if (ghat[i] > 1) ghat[i] = 1
  }

  prob_matrix[j, ] <- ghat
}

# find average across quantiles
avg_prob5 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
# Rd5 <- Rd4_winners[, c(1, 3:4)]
# game.order <- if(matchup == 1){c(1, 1, 2, 2)
#} else {
#  if(matchup == 2){rep(c(1:2), 2)
#  } else {rep(c(1:2, 2:1))}
#}
# Rd5$game <- game.order
# Rd5$prob <- avg_prob5
# Rd5 <- Rd5[, c("region", "game", "seed", "team", "prob")]
# Rd5 <- dplyr::arrange(Rd5, game, -prob, seed)
# Rd5$win <- rep(c(1:0), 2)

# Rd5_winners <- subset(Rd5, Rd5$win == 1)
# Rd5_winners

# Add Rd5 results
win_prob$Rd5 = avg_prob5


### Round 6

# define the data
y_train <- Data_2002_23$rd_6[which(Data_2002_23[, 10] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_23[which(Data_2002_23[, 10] == 1 & Data_2002_23[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_23$rd_6[which(Data_2002_23[, 2] == as.character(season))]
x_test <- Data_2002_23[which(Data_2002_23[, 2] == as.character(season)),
                       c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- 2 * sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
    if (ghat[i] < 0) ghat[i] = 0
    if (ghat[i] > 1) ghat[i] = 1
  }

  prob_matrix[j, ] <- ghat
}

# find average across quantiles
avg_prob6 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
# Rd6 <- Rd5_winners[, c(1, 3:4)]
# Rd6$prob <- avg_prob6
# Rd6 <- Rd6[, c("region", "seed", "team", "prob")]
# Rd6 <- dplyr::arrange(Rd6, -prob)
# Rd6$win <- rep(c(1:0))

# Rd6_winner <- subset(Rd6, Rd6$win == 1)
# Rd6_winner

# Add Rd6 results and print results
win_prob$Rd6 = avg_prob6
win_prob

# Sort by Rd4 to calculate highest probability of advancing to the Final 4
finalfour <- dplyr::arrange(win_prob, -Rd4)
finalfour

# Sort by Rd6 to calculate highest probability of winning the championship
champ <- dplyr::arrange(win_prob, -Rd6)
champ

# Sort by seed to compare the paths by seed
seeding <- dplyr::arrange(win_prob, seed)
seeding
