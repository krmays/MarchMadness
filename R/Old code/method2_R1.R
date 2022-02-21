### Predicting the outcome of tournaments - Round 1
path = 'data/Data_2002_21.rda'
load(path)

season <- "2021"
#matchup options for Rd. 5: 1 (East v. Midwest), 2 (East v. South), 3 (East v. West)
matchup <- 3

# define the data (ratio, no moving window)
#num <- as.matrix(Data_2002_21[c(seq(13, 39, by = 2))])  # index for numerator
#den <- as.matrix(Data_2002_21[c(seq(12, 38, by = 2))])  # index for denominator
#ratio <- num/den
#newdata <- cbind(Data_2002_21[1:11], ratio)

# ratio
#y_train <- newdata$rd_1[which(newdata[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- newdata[which(newdata[, 2] <= as.character(as.numeric(season) - 1)), c(4, 12:25)]
#y_test <- newdata$rd_1[which(newdata[, 2] == as.character(season))]
#x_test <- newdata[which(newdata[, 2] == as.character(season)), c(4, 12:25)]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

#define the data, full season, no window
y_train <- Data_2002_21$rd_1[which(Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)),
                        c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_21$rd_1[which(Data_2002_21[, 2] == as.character(season))]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season)), c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

#define the data, last 3, no window
#y_train <- Data_2002_21$rd_1[which(Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- Data_2002_21[which(Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(13, 39, by = 2))]
#y_test <- Data_2002_21$rd_1[which(Data_2002_21[, 2] == as.character(season))]
#x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season)), c(4, seq(13, 39, by = 2))]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

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
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
  ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
  }

  prob_matrix[j, ] <- ghat >= 0
}

# find average across quantiles
avg_prob <- apply(prob_matrix, 2, sum) * incr

# arrange data by game matchup
#ratio:
#colnames(newdata)[1] <- "team" #rename the first column
#Rd1 <- subset(newdata, year == season, c("region", "team", "seed")) #pull out only the columns we need to set up game matrix
#non-ratio:
colnames(Data_2002_21)[1] <- "team" #rename the first column
Rd1 <- subset(Data_2002_21, year == season, c("region", "team", "seed"))
Rd1 <- dplyr::arrange(Rd1, region, seed) #make sure that teams really are arranged by seed
game.order <- rep(c(1:8, 8:1), 4)
Rd1$game = game.order
Rd1$prob = avg_prob
Rd1 <- Rd1[, c("region", "game", "seed", "team", "prob")]
Rd1 <- dplyr::arrange(Rd1, region, game, -prob, seed)
win <- rep(c(1:0), 32)
Rd1$win <- rep(c(1:0), 32)

# pull out winners only
Rd1_winners <- subset(Rd1, Rd1$win == 1)
Rd1_winners

### Round 2

# define the data
# x defines the ratio
#y_train <- newdata$rd_2[which(newdata[, 6] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- newdata[which(newdata[, 6] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1)), c(4, 12:25)]
#y_test <- newdata$rd_2[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd1_winners$team)]
#x_test <- newdata[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd1_winners$team),
#                  c(4, 12:25)]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# x defines full season data
y_train <- Data_2002_21$rd_2[which(Data_2002_21[, 6] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 6] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_21$rd_2[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd1_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd1_winners$team), c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# last 3 games
#y_train <- Data_2002_21$rd_2[which(Data_2002_21[, 6] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- Data_2002_21[which(Data_2002_21[, 6] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(13, 39, by = 2))]
#y_test <- Data_2002_21$rd_2[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd1_winners$team)]
#x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd1_winners$team),
#                       c(4, seq(13, 39, by = 2))]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
  }

  prob_matrix[j, ] <- ghat >= 0
}

# find average across quantiles
avg_prob2 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
Rd2 <- Rd1_winners[, c(1, 3:4)]
game.order <- rep(c(1:4, 4:1), 4)
Rd2$game = game.order
Rd2$prob = avg_prob2
Rd2 <- Rd2[, c("region", "game", "seed", "team", "prob")]
Rd2 <- dplyr::arrange(Rd2, region, game, -prob, seed)
win2 <- rep(c(1:0), 16)
Rd2$win <- rep(c(1:0), 16)

#Rd2 win predictions
Rd2_winners <- subset(Rd2, Rd2$win == 1)
Rd2_winners

### Round 3

# define the data
# ratio
#y_train <- newdata$rd_3[which(newdata[, 7] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- newdata[which(newdata[, 7] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1)),
#                  c(4, 12:25)]
#y_test <- newdata$rd_3[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd2_winners$team)]
#x_test <- newdata[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd2_winners$team),
#                c(4, 12:25)]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# full season
y_train <- Data_2002_21$rd_3[which(Data_2002_21[, 7] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 7] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_21$rd_3[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd2_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd2_winners$team),
                      c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# last 3
#y_train <- Data_2002_21$rd_3[which(Data_2002_21[, 7] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- Data_2002_21[which(Data_2002_21[, 7] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(13, 39, by = 2))]
#y_test <- Data_2002_21$rd_3[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd2_winners$team)]
#x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd2_winners$team),
#                       c(4, seq(13, 39, by = 2))]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
  }

  prob_matrix[j, ] <- ghat >= 0
}

# find average across quantiles
avg_prob3 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
Rd3 <- Rd2_winners[, c(1, 3:4)]
game.order <- rep(c(1:2, 2:1), 4)
Rd3$game <- game.order
Rd3$prob <- avg_prob3
Rd3 <- Rd3[, c("region", "game", "seed", "team", "prob")]
Rd3 <- dplyr::arrange(Rd3, region, game, -prob, seed)
win3 <- rep(c(1:0), 8)
Rd3$win <- rep(c(1:0), 8)

# Rd3 win predictions
Rd3_winners <- subset(Rd3, Rd3$win == 1)
Rd3_winners

### Round 4

# define the data
# ratio
#y_train <- newdata$rd_4[which(newdata[, 8] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- newdata[which(newdata[, 8] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1)),
#                   c(4, 12:25)]
#y_test <- newdata$rd_4[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd3_winners$team)]
#x_test <- newdata[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd3_winners$team),
#                 c(4, 12:25)]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# full season
y_train <- Data_2002_21$rd_4[which(Data_2002_21[, 8] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 8] ==1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_21$rd_4[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd3_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd3_winners$team),
                      c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# last 3
#y_train <- Data_2002_21$rd_4[which(Data_2002_21[, 8] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- Data_2002_21[which(Data_2002_21[, 8] ==1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(13, 39, by = 2))]
#y_test <- Data_2002_21$rd_4[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd3_winners$team)]
#x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd3_winners$team),
#                       c(4, seq(13, 39, by = 2))]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
  }

  prob_matrix[j, ] <- ghat >= 0
}

# find average across quantiles
avg_prob4 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
Rd4 <- Rd3_winners[, c(1, 3:4)]
game.order <- rep(c(1:4), each = 2)
Rd4$game <- game.order
Rd4$prob <- avg_prob4
Rd4 <- Rd4[, c("region", "game", "seed", "team", "prob")]
Rd4 <- dplyr::arrange(Rd4, region, game, -prob, seed)
Rd4$win <- rep(c(1:0), 4)

Rd4_winners <- subset(Rd4, Rd4$win == 1)
Rd4_winners

### Round 5

# define the data
# ratio
#y_train <- newdata$rd_5[which(newdata[, 9] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- newdata[which(newdata[, 9] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1)),
#                  c(4, 12:25)]
#y_test <- newdata$rd_5[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd4_winners$team)]
#x_test <- newdata[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd4_winners$team),
#                 c(4, 12:25)]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# full season
y_train <- Data_2002_21$rd_5[which(Data_2002_21[, 9] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 9] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_21$rd_5[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd4_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd4_winners$team),
                      c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# last 3
#y_train <- Data_2002_21$rd_5[which(Data_2002_21[, 9] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- Data_2002_21[which(Data_2002_21[, 9] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(13, 39, by = 2))]
#y_test <- Data_2002_21$rd_5[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd4_winners$team)]
#x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd4_winners$team),
#                       c(4, seq(13, 39, by = 2))]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
  }

  prob_matrix[j, ] <- ghat >= 0
}

# find average across quantiles
avg_prob5 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
Rd5 <- Rd4_winners[, c(1, 3:4)]
#game.order <- rep(c(1:2, 2:1))
#if matchup == 3, game order should be 1221
game.order <- if(matchup == 1){c(1, 1, 2, 2)
} else {
  if(matchup == 2){rep(c(1:2), 2)
  } else {rep(c(1:2, 2:1))}
}
Rd5$game <- game.order
Rd5$prob <- avg_prob5
Rd5 <- Rd5[, c("region", "game", "seed", "team", "prob")]
Rd5 <- dplyr::arrange(Rd5, game, -prob, seed)
Rd5$win <- rep(c(1:0), 2)

Rd5_winners <- subset(Rd5, Rd5$win == 1)
Rd5_winners

### Round 6

# define the data
# ratio
#y_train <- newdata$rd_6[which(newdata[, 10] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- newdata[which(newdata[, 10] == 1 & newdata[, 2] <= as.character(as.numeric(season) - 1)),
#                  c(4, 12:25)]
#y_test <- newdata$rd_6[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd5_winners$team)]
#x_test <- newdata[which(newdata[, 2] == as.character(season) & newdata[, 1] %in% Rd5_winners$team),
#                  c(4, 12:25)]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# full season
y_train <- Data_2002_21$rd_6[which(Data_2002_21[, 10] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
x_train <- Data_2002_21[which(Data_2002_21[, 10] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_21$rd_6[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd5_winners$team)]
x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd5_winners$team),
                     c(4, seq(12, 38, by = 2))]
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

# last 3
#y_train <- Data_2002_21$rd_6[which(Data_2002_21[, 10] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1))]
#x_train <- Data_2002_21[which(Data_2002_21[, 10] == 1 & Data_2002_21[, 2] <= as.character(as.numeric(season) - 1)), c(4, seq(13, 39, by = 2))]
#y_test <- Data_2002_21$rd_6[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd5_winners$team)]
#x_test <- Data_2002_21[which(Data_2002_21[, 2] == as.character(season) & Data_2002_21[, 1] %in% Rd5_winners$team),
#                       c(4, seq(13, 39, by = 2))]
#x_train <- as.matrix(x_train)
#x_test <- as.matrix(x_test)

# define the parameters
n_train <- length(y_train)
n_test <- length(y_test)
p <- dim(x_train)[2]
prob_matrix <- matrix(0, length(taus), n_test)

# discretize y_train to be applicable to the cqs function
set.seed(1234)
y_train_dis <- y_train + .00001 * mean(y_train) * rnorm(n_train)

# apply the cqs function and perform dimension reduction
for (j in 1:length(taus)){ #new
  out <- quantdr::cqs(x_train, y_train_dis, taus[j])
  dtau_hat <- out$dtau
  beta_hat <- cbind(out$qvectors[, 1:dtau_hat])

  # define the new sufficient predictors
  new_data_train <- x_train %*% beta_hat
  new_data_test <- x_test %*% beta_hat

  # estimate the nonparametric quantile function
  ghat <- as.null(length(y_test))
  #h <- 1.25 * max(length(y_train_dis)^(-1 / (dtau_hat + 4)), min(2, sd(y_train_dis)) * length(y_train_dis)^(- 1 / (dtau_hat + 4)))
  h <- sd(y_train_dis) * length(y_train_dis)^(- 1 / (dtau_hat + 4))
  for (i in 1:length(y_test)){
    ghat[i] <- quantdr::llqr(new_data_train, y_train_dis, tau = taus[j], h = h, x0 = new_data_test[i, ])$ll_est
  }

  prob_matrix[j, ] <- ghat >= 0
}

# find average across quantiles
avg_prob6 <- apply(prob_matrix, 2, sum) * incr

# arrange games by matchup
Rd6 <- Rd5_winners[, c(1, 3:4)]
Rd6$prob <- avg_prob6
Rd6 <- Rd6[, c("region", "seed", "team", "prob")]
Rd6 <- dplyr::arrange(Rd6, -prob)
Rd6$win <- rep(c(1:0))

Rd6_winner <- subset(Rd6, Rd6$win == 1)
Rd6_winner
