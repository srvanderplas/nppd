# This file contains functions for bayesian analysis

#' Function to calculate beta prior parameters with data from a more general population
#' @param Nk Number of trials/observations (must be positive if supplied)
#' @param Sk Number of successful trials (must be positive if supplied)
#' @param Fk Number of failed trials (must be positive if supplied)
#' @param equiv.obs Number of equivalent observations supplied by the prior distribution
#' @return data frame with columns alpha and beta, representing parameters shape1 and shape2 of a beta distribution
#' @details Two of Nk, Sk, Fk must be supplied (non-null); the third will be calculated.
equivBetaPars <- function(Nk = NULL, Sk = NULL, Fk = NULL, equiv.obs = 5){
  # Check to ensure Nk, Sk, Fk are reasonable
  if ((is.null(Nk) + is.null(Sk) + is.null(Fk)) > 1) {
    stop("Two of Nk, Sk, Fk must be supplied")
  } else if ((is.null(Nk) + is.null(Sk) + is.null(Fk)) == 0) {
    # If all 3 are supplied, ensure they are consistent
    stopifnot(Nk == (Sk + Fk))
  }

  # Check for NA values which are not null but still not useful
  stopifnot(sum(is.na(Nk) | is.na(Sk) | is.na(Fk)) == 0)

  # Fill in missing Nk, Sk, Fk
  if (is.null(Nk)) {
    Nk <- Sk + Fk
  } else if (is.null(Sk)) {
    Sk <- Nk - Fk
  } else if (is.null(Fk)) {
    Fk <- Nk - Sk
  }

  # Ensure equiv.obs exists and is not NA
  stopifnot(!(is.null(equiv.obs) | is.na(equiv.obs)))

  # Ensure equivalent number of observations is positive
  stopifnot(sum(equiv.obs <= 0) == 0)

  # Ensure Nk, Sk, Fk are positive
  stopifnot(Nk > 0, Sk > 0, Fk > 0)

  return(
    data.frame(
      alpha = equiv.obs * Sk / Nk,
      beta = equiv.obs * Fk / Nk
    ))
}

#' Function to calculate beta-binomial posterior parameters
#' @param Ni Number of tests/observations in the data (must be positive if supplied)
#' @param Si Number of successful tests/observations in the data (must be positive if supplied)
#' @param Fi Number of failed tests/observations in the data (must be positive if supplied)
#' @param Nk Number of tests/observations contributing to the prior distribution (must be positive if supplied)
#' @param Sk Number of successful tests/observations contributing to the prior distribution (must be positive if supplied)
#' @param Fk Number of failed tests/observations contributing to the prior distribution (must be positive if supplied)
#' @param equiv.obs Number of equivalent observations supplied by the prior distribution (must be positive if supplied)
#' @param pars prior distribution parameters shape1, shape2 which can be supplied instead of Nk, Sk, Fk, and eff.obs
#' @return data frame with columns alpha and beta, representing parameters shape1 and shape2 of a beta distribution
#' @details Two of Ni, Si, Fi must be supplied (non-null); the third will be calculated. Either equiv.obs and two of Nk, Sk, Fk must be supplied, or pars must be a vector of length two or a matrix of at least two columns.
#' Similarly, two of Nk, Sk, Fk must be supplied along with equiv.obs (the prior parameters will be calculated using this information)
betaBinomPosteriorPars <- function(Ni = NULL, Si = NULL, Fi = NULL, Nk = NULL, Sk = NULL, Fk = NULL, equiv.obs = NULL, pars = NULL){
  if (is.null(pars)) {
    pars <- as.matrix(
      equivBetaPars(Nk = Nk, Sk = Sk, Fk = Fk, equiv.obs = equiv.obs)
    )
  } else {
    if (!(is.null(Nk) & is.null(Sk) & is.null(Fk) & is.null(equiv.obs))) {
      warning("Using pars for prior parameters and ignoring Nk, Sk, Fk, and equiv.obs.")
    }
    # Format as a two-col matrix if currently a vector
    if (is.null(dim(pars))) {
      pars <- as.matrix(t(pars), ncol = 2)
    } else if (dim(pars)[2] != 2) {
      stop("pars must have two columns")
    }

    # Ensure all pars are positive
    stopifnot(
      sum(pars[,1] <= 0) == 0,
      sum(pars[,2] <= 0) == 0
    )
  }

  # Check to ensure Ni, Si, Fi are reasonable
  if ((is.null(Ni) + is.null(Si) + is.null(Fi)) > 1) {
    stop("Two of Ni, Si, Fi must be supplied")
  } else if ((is.null(Ni) + is.null(Si) + is.null(Fi)) == 0) {
    # If all 3 are supplied, ensure they are consistent
    stopifnot(Ni == (Si + Fi))
  }

  # Check for NA values which are not null but still not useful
  stopifnot(sum(is.na(Ni) | is.na(Si) | is.na(Fi)) == 0)

  # Fill in missing Ni, Si, Fi
  if (is.null(Ni)) {
    Ni <- Si + Fi
  } else if (is.null(Si)) {
    Si <- Ni - Fi
  } else if (is.null(Fi)) {
    Fi <- Ni - Si
  }

  # Calculate beta pars:
  res <- data.frame(
    alpha = pars[,1] + Si,
    beta = pars[,2] + Fi
  )

  # Ensure calculated parameters are positive
  stopifnot(sum(res$alpha <= 0) == 0,
            sum(res$beta <= 0) == 0)

  return( res )
}