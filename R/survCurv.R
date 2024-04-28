#' Calculate and Plot Survival Curve
#'
#' This function calculates the survival curve S(t) using the Kaplan-Meier estimator from
#' survival analysis, and plots the curve using ggplot2. The function requires two vectors:
#' one for the status (1 if the event of interest has occurred, 0 otherwise), and one for
#' the time until the event occurs or censorship.
#'
#' @param status Numeric vector indicating the event occurrence (1 = event, 0 = censored).
#' @param time Numeric vector of times at which the events or censorship occur.
#'
#' @return A ggplot object representing the survival curve.
#'
#' @examples
#' # Assume data is loaded where 'status' and 'time' are defined
#' survCurv(status, time)
#'
#' @import ggplot2
#' @import survival
#' @export
survCurv = function(status, time) {
  require(survival)
  require(ggplot2)
  surv_obj = Surv(time, status)
  surv_fit = survfit(surv_obj ~ 1)
  g = ggplot(data = as.data.frame(surv_fit$time, surv_fit$surv),
              aes(x = time, y = surv)) +
    geom_step() +
    labs(title = "Survival Curve", x = "Time", y = "Survival Probability") +
    theme_minimal()
  return(g)
}
