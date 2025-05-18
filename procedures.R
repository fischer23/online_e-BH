# This file contains the procedures analyzed in the paper
# "An online generalization of the (e-)Benjamini-Hochberg procedure"

### online e-BH

# Input:

# alpha:   overall significance level, real number between 0 and 1.
# gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
# e:       e-values, n-dim. vector of nonnegative real numbers.
# n:       number of hypotheses, natural number.

# Output:

# rejects: n-dim. vector of rejection indicators

online_e_bh <- function(alpha, gamma, e, n) {
  min_rej <- n - rowSums((e >= (1 / ((alpha * gamma) %*% t((1:n)))))) + 1
  k_star <- max(c(which(colSums(outer(min_rej, (1:n), "<=")) >= (1:n)), 1))
  rejects <- (e >= (1 / (alpha * gamma * k_star)))
  return(rejects)
}

### boosted online e-BH

# Input:

# alpha:   overall significance level, real number between 0 and 1.
# gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
# e:       e-values, n-dim. vector of nonnegative real numbers.
# n:       number of hypotheses, natural number.
# s:       maximum number of rejections, natural number.
# delta:   mean difference between alternative and null hypothesis, nonnegative real number.

# Output:

# rejects: n-dim. vector of rejection indicators

online_e_bh_boosted <- function(alpha, gamma, e, n, s, delta) {
  for (i in 1:n) {
    e[i] <- e[i] * e_boosted_minus(s, alpha, gamma[i], delta)
  }
  min_rej <- n - rowSums((e >= (1 / ((alpha * gamma) %*% t((1:n)))))) + 1
  k_star <- max(c(which(colSums(outer(min_rej, (1:n), "<=")) >= (1:n)), 1))
  rejects <- (e >= (1 / (alpha * gamma * k_star)))
  return(rejects)
}

### boosted online e-BH under local dependence

# Input:

# alpha:   overall significance level, real number between 0 and 1.
# gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
# e:       e-values, n-dim. vector of nonnegative real numbers.
# n:       number of hypotheses, natural number.
# s:       maximum number of rejections, natural number.
# delta:   mean difference between alternative and null hypothesis, nonnegative real number.
# lags:    lags that define the local dependence structure, n-dim. vector of natural numbers (including 0).

# Output:

# rejects: n-dim. vector of rejection indicators

online_e_bh_boosted_local <- function(alpha, gamma, e, n, s, delta, lags) {
  rejects <- c()
  for (i in 1:n) {
    e[i] <- e[i] * e_boosted_minus_local(s, alpha, gamma[i], delta, sum(rejects[1:(i - lags[i] - 1)]))
    min_rej <- i - rowSums((e[1:i] >= (1 / ((alpha * gamma[1:i]) %*% t((1:i)))))) + 1
    k_star <- max(c(which(colSums(outer(min_rej, (1:i), "<=")) >= (1:i)), 1))
    rejects <- (e[1:i] >= (1 / (alpha * gamma[1:i] * k_star)))
  }
  return(rejects)
}


### e-lond

# Input:

# alpha:   overall significance level, real number between 0 and 1.
# gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
# e:       e-values, n-dim. vector of nonnegative real numbers.
# n:       number of hypotheses, natural number.

# Output:

# rejects: n-dim. vector of rejection indicators

e_lond <- function(alpha, gamma, e, n) {
  rejects <- rep(FALSE, n)
  for (i in 1:n) {
    rejects[i] <- (e[i] >= 1 / (alpha * gamma[i] * (sum(rejects) + 1)))
  }
  return(rejects)
}


### online BH

# Input:

# alpha:   overall significance level, real number between 0 and 1.
# gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
# p:       p-values, n-dim. vector of real numbers between 0 and 1.
# n:       number of hypotheses, natural number.

# Output:

# rejects: n-dim. vector of rejection indicators

online_bh <- function(alpha, gamma, p, n) {
  min_rej <- n - rowSums((p <= ((alpha * gamma) %*% t(1:n)))) + 1
  k_star <- max(c(which(colSums(outer(min_rej, (1:n), "<=")) >= (1:n)), 1))
  rejects <- (p <= (alpha * gamma * k_star))
  return(rejects)
}


### online Storey-BH

# Input:

# alpha:   overall significance level, real number between 0 and 1.
# gamma:   weighting sequence, n-dim. vector of real numbers between 0 and 1 with sum less than or equal to 1.
# lambda:  Adaptivity parameter for Storey-BH, real number between alpha and 1.
# p:       p-values, n-dim. vector of real numbers between 0 and 1.
# n:       number of hypotheses, natural number.

# Output:

# rejects: n-dim. vector of rejection indicators

online_storey_bh <- function(alpha, gamma, lambda, p, n) {
  pi_hat <- (max(gamma) + sum(gamma * (p > lambda)) + (1 - sum(gamma))) / (1 - lambda)
  min_rej <- n - rowSums((p <= ((alpha * gamma / pi_hat) %*% t(1:n)))) + 1
  k_star <- max(c(which(colSums(outer(min_rej, (1:n), "<=")) >= (1:n)), 1))
  rejects <- (p <= (alpha * gamma * k_star / pi_hat))
  return(rejects)
}
