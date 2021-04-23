# Predicting the outcome of tournaments - Round 1
path = 'data/Data_2002_19.rda'
load(path)
Data_2002_19

# set variables (season-long stats only)
y <- Data_2002_19$rd_1[which(Data_2002_19[, 2]<="2016")]
length(y)
x1 <- Data_2002_19[which(Data_2002_19[, 2]<="2016"), c(4, seq(12, 38, by = 2))] # season stats
dim(x1)
summary(x1)

# define parameters
n <- length(y)
p1 <- dim(x1)[2]
tau <- 0.1

# discretize y1
y1 = y + .00001 * mean(y) * rnorm(n)
summary(y1)

# cqs at tau=0.1
library(quantdr)
out1 <- quantdr::cqs(x1, y1, tau = 0.1)
out1
plot(out1$qvalues)
beta_hat1 <- cbind(out1$qvectors[, 1:out1$dtau])

# define new sufficient predictors and test parameters
new_data1 <- as.matrix(x1) %*% beta_hat1
new_data1
x_train <- x1
y_train <- y1
x_test <- Data_2002_19[which(Data_2002_19[, 2]=="2017"), c(4, seq(12, 38, by = 2))]
y_test <- Data_2002_19$rd_1[which(Data_2002_19[, 2]=="2017")]
nt = length(y_train); nte = length(y_test)
C=2; classes=c(0,1)
prob_results_CQS <- matrix(0,length(tau),nte)
prob_results_reg <- matrix(0,length(tau),nte)

# standardize predictors (should we change to use package MTS?)
for (i in 1:p1){
  x_train[,i]=(x_train[,i]-apply(x_train, 2, mean)[i])/apply(x_train, 2, sd)[i]
  x_test[,i]=(x_test[,i]-apply(x_test, 2, mean)[i])/apply(x_test, 2, sd)[i]
}

new_data_train_cqs = new_data1
new_data_test_cqs = as.matrix(x_test)%*%beta_hat1
# waiting on helping R file here (go ahead and test)
fit_cqs <- BBQ.grplasso(y_train~new_data_train_cqs, tau, c(1,2), method='Binary', Run=1500, burn=500,Ce=0,scale=TRUE)
cqs_prob<-BBQ.prob(fit_cqs,new_data_test_cqs)$p1x
prob_results_cqs[j,]=cqs_prob



