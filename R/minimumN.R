#' Calculate Minimum Sample Size for t-test
#'
#' This function calculates the minimum sample size required to achieve 80% power at a 5% significance level
#' for a one-sample or two-sample t-test. It wraps the `pwr::pwr.t2n.test` function.
#'
#' @param x1 A numeric vector representing sample data. If x2 is NULL, a one-sample t-test is performed against a mean of 0.
#' @param x2 An optional numeric vector representing a second sample for a two-sample t-test.
#'
#' @return The minimum sample size required to achieve the desired power level.
#'
#' @examples
#' x1 = rnorm(10, mean = 3)
#' x2 = rnorm(10, mean = 3.5)
#' minimumN(x1, x2) # Two-sample t-test
#' minimumN(x1) # One-sample t-test against a mean of 0
#'
#' @importFrom pwr pwr.t2n.test
#' @export
minimumN = function(x1, x2 = NULL) {
  require(pwr)
  if (!is.null(x2)) {
    # Two-sample t-test
    d = abs(mean(x1) - mean(x2)) / sqrt((var(x1)/length(x1)) + (var(x2)/length(x2)))
    result = pwr::pwr.t2n.test(d = d, sig.level = 0.05, power = 0.80)
    return(ceiling(max(result$n1, result$n2)))
  } else {
    # One-sample t-test
    d = abs(mean(x1)) / sqrt(var(x1) / length(x1))
    result = pwr::pwr.t2n.test(d = d, sig.level = 0.05, power = 0.80, n2 = 0)
    return(ceiling(result$n1))
  }
}
