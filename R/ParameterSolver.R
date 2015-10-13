#' Function to find parameters for a gamma distn
#' @param qu location (x) of p-th quantile. Vector of length 2.
#' @param p quantile (i.e. 100*p% of the observed data is less than qu). Vector of length 2.
#' @param init starting values for optimization. Recommendation: (x, 1), where x is close to the expected value (or median) of your distribution.
#' @param ... Other options passed to optim
#' @return shape and scale parameters for the gamma distribution, (a, s), where EX = a*s and Var(X) = a*s^2
quantiles_to_gamma_pars = function(qu, p, init=c(1,1), quiet = F, ...) {
  # I think that the p-th quantile occurs at qu
  qu <- qu
  p <- p
  gammaoptim = function(param) {
    q1 <- qu[1]
    q2 <- qu[2]
    p1 <- p[1]
    p2 <- p[2]
    (pgamma(q1, param[1], param[2]) - p1) ^ 2 +
      (pgamma(q2, param[1], param[2]) - p2) ^ 2
  }

  r <- optim(init, gammaoptim, ...)
  v <- unlist(r)
  t <- c(shape = v[1], scale = v[2])
  if (!quiet) print(t)
  return(t)
}

#' Function to find parameters for a gamma distn
#' @param mean expected value of the distribution
#' @param qu location (x) of p-th quantile.
#' @param p quantile (i.e. 100*p% of the observed data is less than qu).
#' @param init starting values for optimization. Recommendation: (mean, 1), where x is close to the expected value (or median) of your distribution.
#' @param ... Other options passed to optim
#' @return shape and scale parameters for the gamma distribution, (a, s), where EX = a*s and Var(X) = a*s^2
mean_quantile_to_gamma_pars = function(mean, qu, p, init=c(1,1), quiet = F, ...) {
  # I think that the p-th quantile occurs at qu
  qu <- qu
  p <- p
  gammaoptim = function(param) {
    q1 <- qu
    p1 <- p
    q2 <- qu[2]
    p2 <- p[2]
    (pgamma(q1, param[1], param[2]) - p1) ^ 2 +
      (param[1]*param[2] - mean) ^ 2
  }

  r <- optim(init, gammaoptim, ...)
  v <- unlist(r)
  t <- c(shape = v[1], scale = v[2])
  if (!quiet) print(t)
  return(t)
}