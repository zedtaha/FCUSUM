#' Get Critical Values for Fourier CUSUM Test
#'
#' @description
#' Internal function to retrieve appropriate critical values for the Fourier
#' CUSUM cointegration test based on the number of regressors and maximum
#' frequency parameter.
#'
#' @param p Number of regressors in the cointegration model
#' @param kstar Maximum frequency parameter for Fourier approximation
#'
#' @return A list containing:
#' \describe{
#'     \item{critical_values}{Numeric vector with 1\%, 5\%, and 10\% critical values}
#'   \item{p}{Adjusted number of regressors used for critical value lookup}
#'   \item{k}{Adjusted frequency parameter used for critical value lookup}
#' }
#'
#' @details
#' Critical values are organized by:
#' \itemize{
#'   \item Number of regressors (p = 1, 2, 3, 4)
#'   \item Frequency parameter (k = 1, 2, 3)
#' }
#'
#' For values beyond the available table (p > 4 or k > 3), the function
#' uses the maximum available values as conservative estimates.
#'
#' @references
#' Based on simulation studies following Zaghdoudi (2025) methodology
#'
#' @keywords internal
#' @noRd
get_critical_values <- function(p, kstar) {
  # Critical value database
  crit_vals <- list(
    p1 = list(
      k1 = c("1%" = 0.3087608, "5%" = 0.3453433, "10%" = 0.3702899),
      k2 = c("1%" = 0.2994366, "5%" = 0.3376164, "10%" = 0.3602958),
      k3 = c("1%" = 0.2983359, "5%" = 0.3376654, "10%" = 0.3612651)
    ),
    p2 = list(
      k1 = c("1%" = 0.2990014, "5%" = 0.3318281, "10%" = 0.3556437),
      k2 = c("1%" = 0.2934757, "5%" = 0.3297304, "10%" = 0.3533944),
      k3 = c("1%" = 0.2959929, "5%" = 0.3331994, "10%" = 0.3559223)
    ),
    p3 = list(
      k1 = c("1%" = 0.2893073, "5%" = 0.3221990, "10%" = 0.3433905),
      k2 = c("1%" = 0.2833487, "5%" = 0.3136938, "10%" = 0.3333621),
      k3 = c("1%" = 0.2825690, "5%" = 0.3130642, "10%" = 0.3330271)
    ),
    p4 = list(
      k1 = c("1%" = 0.2814507, "5%" = 0.3131563, "10%" = 0.3318605),
      k2 = c("1%" = 0.2712004, "5%" = 0.3051386, "10%" = 0.3224608),
      k3 = c("1%" = 0.2700021, "5%" = 0.3024737, "10%" = 0.3207691)
    )
  )

  # Adjust parameters to available range
  p_adj <- min(p, 4)
  k_adj <- min(kstar, 3)

  # Construct lookup keys
  p_key <- paste0("p", p_adj)
  k_key <- paste0("k", k_adj)

  # Retrieve critical values
  cv <- crit_vals[[p_key]][[k_key]]

  # Fallback for missing values
  if (is.null(cv)) {
    warning("Critical values not available for p=", p, ", k*=", kstar,
            ". Using p=1, k=1 as default.")
    cv <- crit_vals$p1$k1
    p_adj <- 1
    k_adj <- 1
  }

  return(list(
    critical_values = cv,
    p = p_adj,
    k = k_adj
  ))
}
