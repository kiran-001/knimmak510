#' Approximate Data Using Principal Components
#'
#' This function performs PCA on the given data and uses the specified number of principal components
#' to reconstruct and approximate the data. The reconstructed data is rescaled and centered to match
#' the original data's scale and center.
#'
#' @param x A numeric matrix or data frame.
#' @param npc The number of principal components to use for the approximation.
#'
#' @return A numeric matrix of the approximated data, rescaled and recentered.
#'
#' @examples
#' data_matrix = matrix(rnorm(100), ncol=10)
#' approx_data = pcApprox(data_matrix, 5)
#'
#' @export
pcApprox = function(x, npc) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Input must be a matrix or data frame.")
  }
  if (npc > min(dim(x))) {
    stop("Number of PCs cannot exceed the smaller dimension of the input data.")
  }
  pca_result = prcomp(x, scale. = TRUE)
  approx = pca_result$x[, 1:npc] %*% t(pca_result$rotation[, 1:npc])
  scaled_center = attr(pca_result$scale, "scaled:center")
  scaled_scale = attr(pca_result$scale, "scaled:scale")
  approx = sweep(approx, 2, scaled_center, "+")
  sweep(approx, 2, scaled_scale, "*")
}
