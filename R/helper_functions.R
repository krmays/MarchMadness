#' Used in the Round1_pred.R function
#' Predicted probabilities of y given x from the binary quantile regression model with group lasso.

BBQ.prob <- function(bbq.out, x.test) {
  beta <- bbq.out$beta
  tau <- bbq.out$tau
  M <- nrow(beta)
  xx.test <- cbind(1, x.test)
  p1x <- 0
  for(im in 1:M) {
    loc = - xx.test %*% beta[im, ]
    prob = 1 - VGAM::palap(q = loc, tau = tau)
    p1x <- p1x + prob
  }
  p1x <- p1x / M
  cl <- ifelse(p1x > (1 - tau), 1, 0)
  return(list(cl = cl, p1x = p1x))
}






#' Used in the Round1_pred.R function
#' Main function for binary quantile regression with group lasso
BBQ.grplasso <- function(formula, tau, group, method = c("Continuous", "Binary", "Tobit"),
                         Run = 15000, burn = 5000, Ce = 0,  scale = TRUE) {
  X= x = formula[3][[1]]
  y = formula[2][[1]]
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  x <- model.matrix(mt, mf, contrasts)
  x <- as.matrix(x)
  if (ncol(x) == 1) {
    x = x
  }
  else {
    x = x
    if (all(x[, 2] == 1))
      x = x[, -2]
  }
  if(scale)
  {
    x.mean <- c(0, (apply(x[, -1], 2, mean)))
    x.sd <- c(1, (apply(x[, -1], 2, sd)))
    x[, -1] <- scale(x[, -1], center = TRUE, scale = TRUE)
  }
  N = n <- nrow(x)
  p <- ncol(x)
  if (tau <= 0 || tau >= 1)
    stop("invalid tau:  tau should be >= 0 and <= 1. \nPlease respecify tau and call again.\n")
  if (!(all(is.finite(y)) || all(is.finite(x))))
    stop("All values must be finite and non-missing")
  if (n != length(y))
    stop("length(y) and nrow(x) must be the same. \nPlease respecify the length of y and call again.\n")
  if (!(all(is.finite(y)) || all(is.finite(x))))
    stop("All values must be finite and non-missing")
  if (n != length(y))
    stop("length(y) not equal to nrow(x)")
  if (n == 0)
    return(list(coefficients = numeric(0), fitted.values = numeric(0),
                deviance = numeric(0)))
  if (!(all(is.finite(y)) || all(is.finite(x))))
    stop("All values must be finite and non-missing")

  # Draw from Inverse gaussian distribution.
  rInvgauss <- function(n, mu, lambda = 1) {
    un <- runif(n)
    Xi <- rchisq(n, 1)
    f <- mu / (2 * lambda) * (2 * lambda + mu * Xi + sqrt(4 *
                                                          lambda * mu * Xi + mu^2 * Xi^2))
    s <- mu^2 / f
    ifelse(un < mu / (mu + s), s, f)
  }

  betadraw  <- matrix(0, nrow = Run, ncol = p)
  sigmadraw <- matrix(nrow = Run, ncol = 1)

  n = nrow(x)
  p = ncol(x)
  ym = y
  if (method == "Tobit") ym <- pmax(Ce, y)

  G1 = 0
  a1 = a2 = 0.1
  b1 = b2 = 0.1
  v = rep(1,n)
  beta = rep(1,p)
  sigma = 1
  theta = 1
  xi  =  (1 - 2 * tau)
  zeta = tau * (1 - tau)
  no.groups = length(group)
  s = rep(1, no.groups)
  for (g in 1:no.groups) {
    G1 = c(G1, length(group[[g]]))
  }
  K = list()
  for (g in 1:no.groups) {
    K[[g]] = diag(rep(1, length(group[[g]]))) * length(group[[g]])
  }

  ## Run Gibbs Sampler on the Data  ##
  for(Iteration in 1:Run) {

    #if (Iteration %% 1 == 0) {
    #  	cat('Iteration:', Iteration, "\r")
    #      }
    # Sample y
    if(method == "Binary") {
      yi = y
      XB = as.matrix(x) %*% beta
      Mean = XB + xi * v
      Sd = sqrt(2 * sigma * v)
      ind1 = which(ym == 1)
      ind0  = which(ym == 0)
      y[ind1] = truncnorm::rtruncnorm(length(ind1), a = 0, b = Inf, mean = Mean[ind1], sd = Sd[ind1])
      y[ind0] = truncnorm::rtruncnorm(length(ind0), a = -Inf, b = 0, mean = Mean[ind0], sd = Sd[ind0])
      y[which(is.infinite(y))] = yi[which(is.infinite(y))]
    }

    if(method == "Tobit") {
      yi = y
      XB = as.matrix(x) %*% beta
      mean.y = XB + xi * v
      sd.y = sqrt(2 * v)
      ind0  = which(ym == Ce)
      y[ind0] = rTruncnorm(length(ind0), m = mean.y[ind0], sig = sd.y[ind0],  a = -Inf, b = Ce)
      y[which(is.infinite(y))] = yi[which(is.infinite(y))]
    }

    # Sample v
    temp.lambda = 1 / (2 * sigma)
    temp.nu = sqrt(1 / (y - x %*% beta)^2)
    v = 1 / rInvgauss(n, mu = temp.nu, lambda = temp.lambda)
    v= v =as.vector(v)
    V = diag(1 / (2 * sigma * v))

    # Sample s
    lambda = theta
    for (g in 1:no.groups) {
      mu = sqrt(lambda / (t(beta[group[[g]]]) %*% K[[g]] %*% beta[group[[g]]]) )
      mu = as.vector(mu)
      s[g] = 1 / rInvgauss(1, mu, lambda)
    }

    # Sample beta
    for (g in 1:no.groups) {
      varcov = solve(t(x[,group[[g]]]) %*% V %*% x[, group[[g]]] + K[[g]] * s[g]^(-1))
      xtVy = t(x[ ,group[[g]]]) %*% V %*% (y - xi * v - x[, -group[[g]]] %*% beta[-group[[g]]])
      beta[group[[g]]] = varcov %*% as.numeric(xtVy)
      beta[group[[g]]] = beta[group[[g]]] + t(chol(varcov)) %*% rnorm(length(beta[group[[g]]]))
    }
    if(method == "Binary")
      beta[-1] = beta[-1] / sqrt(sum(beta[-1]^2)) #Scaling beta to have norm equal to 1.

    # Sample sigma
    temp.shape =  3 * n / 2 + a1
    temp.rate = sum( (y - x %*% beta - xi * v)^2 / (4 * v) + zeta * v ) + a2
    sigma = 1 / rgamma(1, shape = temp.shape, rate = temp.rate)
    if (method == "Binary") (sigma = 1)

    # Sample theta
    shape2 = 0.5 * (p + no.groups) + b1
    rate2  = sum(s / 2) + b2
    theta  = rgamma(1, shape2, rate2)

    betadraw[Iteration, ] = beta
    sigmadraw[Iteration, ] = sigma
  }
  if(scale){
    for(i in 1:Run){
      betadraw[i, ]=betadraw[i, ] / x.sd
      betadraw[i, 1]<-betadraw[i, 1] - t(x.mean) %*% betadraw[i, ]
    }
  }
  #sparsity
  #betadraw<-round(betadraw,2)
  result = list(beta = betadraw[-(1:burn), ],  sigma=sigmadraw[-(1:burn)], tau = tau)
  return(result)
}
