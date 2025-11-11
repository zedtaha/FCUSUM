#' Corrected Akaike Information Criterion
#'
#' @description
#' Computes the corrected Akaike Information Criterion (AICc) for model
#' selection. AICc includes a correction term for finite sample sizes.
#'
#' @param model An object of class \code{lm}
#'
#' @return Numeric value representing the AICc
#'
#' @details
#' AICc = AIC + 2p(p+1)/(n-p-1)
#'
#' where p is the number of parameters and n is the sample size.
#'
#' @keywords internal
#' @noRd
aicc <- function(model) {
  n <- length(stats::resid(model))
  p <- length(stats::coef(model))
  aic <- stats::AIC(model)
  aic + (2 * p * (p + 1)) / (n - p - 1)
}
