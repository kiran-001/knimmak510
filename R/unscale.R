#' Reverse Scaling of a Numeric Vector
#'
#' This function reverses the scaling (standardization) applied to a numeric vector.
#' It uses the attributes 'scaled:center' and 'scaled:scale' saved during the scaling
#' to revert the vector to its original form.
#'
#' @param x A scaled numeric vector.
#'
#' @return A numeric vector with the scaling reversed, returning it to its original scale.
#'
#' @examples
#' original = c(1, 2, 3, 4, 5)
#' scaled = scale(original)
#' unscale(scaled) # should return c(1, 2, 3, 4, 5)
#'
#' @export
unscale = function(x) {
  if (is.null(attr(x, "scaled:center")) || is.null(attr(x, "scaled:scale"))) {
    stop("The input vector does not appear to be scaled using scale().")
  }
  (x * attr(x, "scaled:scale")) + attr(x, "scaled:center")
}
