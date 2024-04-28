#' Calculate the Maximum Likelihood Estimator for Parameter p of Bernoulli Distribution
#'
#' This function performs a grid-based search to find the value of p (probability of success)
#' that maximizes the log-likelihood of observing the given binary data assuming a Bernoulli
#' distribution. The search considers p values from 0 to 1 in increments of 0.001.
#'
#' @param data A numeric vector of binary data (0s and 1s) where 1 represents success and 0 represents failure.
#'
#' @return The value of p that maximizes the log-likelihood of the data.
#'
#' @examples
#' test_data = c(1,0,0,0,1,1,1)
#' logLikBernoulli(test_data)
#'
#' @export
logLikBernoulli = function(data) {
  p_values = seq(0, 1, by = 0.001)
  log_likelihoods = sapply(p_values, function(p) {
    sum(data * log(ifelse(p == 0, 1e-10, p)) + (1 - data) * log(ifelse(p == 1, 1e-10, 1 - p)))
  })
  max_likelihood_p = p_values[which.max(log_likelihoods)]
  return(max_likelihood_p)
}
