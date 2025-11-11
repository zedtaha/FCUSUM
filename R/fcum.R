#' Fourier CUSUM Cointegration Test
#'
#' @description
#' Implements the Fourier CUSUM test for cointegration with structural breaks.
#' The test uses Fourier approximations to model smooth structural changes and
#' applies CUSUM statistics to test for cointegration stability.
#'
#' @param y Numeric vector or matrix. Dependent variable time series.
#' @param x Numeric vector or matrix. Independent variable(s) time series.
#'   Must have the same number of observations as y.
#' @param kstar Positive numeric value. Maximum frequency parameter for
#'   Fourier approximation. Determines the flexibility of structural break
#'   modeling. Default is 3.
#'
#' @return An object of class \code{fcum} containing:
#' \describe{
#'   \item{statistic}{The CUSUM test statistic}
#'    \item{critical_values}{Critical values at 1\%, 5\%, and 10\% significance levels}
#'   \item{p}{Number of regressors used for critical value lookup}
#'   \item{k}{Frequency parameter used for critical value lookup}
#'   \item{kstar}{Maximum frequency parameter (user input)}
#'   \item{best_frequency}{Optimal frequency selected by AICc criterion}
#'   \item{decision}{Test decision (reject or fail to reject null hypothesis)}
#'   \item{significance}{Significance level indicator (*, **, ***)}
#'   \item{best_model}{The best fitting lm model object}
#'   \item{call}{The matched function call}
#' }
#'
#' @details
#' The null hypothesis is that there exists a cointegrating relationship with
#' stable parameters. The alternative hypothesis is that the cointegrating
#' relationship is unstable or does not exist.
#'
#' The test searches over a grid of frequencies from 0.1 to \code{kstar} and
#' selects the optimal frequency using the corrected Akaike Information
#' Criterion (AICc). The CUSUM statistic is then computed from the residuals
#' of the best model.
#'
#' Critical values are based on simulation studies and depend on:
#' \itemize{
#'   \item The number of regressors (p)
#'   \item The frequency parameter (k)
#' }
#'
#' @references
#' Zaghdoudi, T. (2025). Fourier CUSUM Cointegration Test Methodology.
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#' n <- 100
#' x <- cumsum(rnorm(n))
#' y <- 2 + 1.5 * x + rnorm(n)
#'
#' # Run the test
#' result <- fcum(y, x, kstar = 3)
#' print(result)
#' summary(result)
#'
#' @export
fcum <- function(y, x, kstar = 3) {
  # Input validation
  if (missing(y) || missing(x)) {
    stop("Arguments 'y' and 'x' are required")
  }

  if (!is.numeric(y) || !is.numeric(x)) {
    stop("Arguments 'y' and 'x' must be numeric")
  }

  if (!is.numeric(kstar) || length(kstar) != 1 || kstar <= 0) {
    stop("Argument 'kstar' must be a positive numeric value")
  }

  # Convert inputs to matrices
  y <- suppressWarnings(as.matrix(as.numeric(y)))
  x <- suppressWarnings(as.matrix(as.numeric(x)))

  if (nrow(y) != nrow(x)) {
    stop("'y' and 'x' must have the same number of observations")
  }

  n <- nrow(y)
  if (n < 10) {
    stop("Insufficient observations for reliable test (minimum 10 required)")
  }

  # Prepare frequency grid
  pramk <- seq(0.1, kstar, 0.01)

  # Fit models across frequency grid
  model <- suppressWarnings(
    lapply(pramk, function(freq) {
      stats::lm(y ~ x +
                  cos(2 * pi * freq * seq_len(n) / n) +
                  sin(2 * pi * freq * seq_len(n) / n))
    })
  )

  # Select best model using AICc
  model_aic <- sapply(model, aicc)
  best_model <- model[[which.min(model_aic)]]
  best_freq <- pramk[which.min(model_aic)]

  # Compute CUSUM statistic
  u <- as.matrix(stats::resid(best_model))
  t <- nrow(u)
  lr <- as.numeric(sqrt(stats::var(u)))
  cus <- max(abs(cumsum(u)) / (lr * sqrt(t)))

  # Get critical values
  cv_list <- get_critical_values(ncol(x), kstar)
  p <- cv_list$p
  k <- cv_list$k
  cv <- cv_list$critical_values

  # Make decision
  if (cus > cv["1%"]) {
    decision <- "Reject H0 at 1% level"
    significance <- "***"
  } else if (cus > cv["5%"]) {
    decision <- "Reject H0 at 5% level"
    significance <- "**"
  } else if (cus > cv["10%"]) {
    decision <- "Reject H0 at 10% level"
    significance <- "*"
  } else {
    decision <- "Fail to reject H0"
    significance <- ""
  }

  # Create result object
  result <- structure(
    list(
      statistic = c(`CUSUM` = cus),
      critical_values = cv,
      p = p,
      k = k,
      kstar = kstar,
      best_frequency = best_freq,
      decision = decision,
      significance = significance,
      best_model = best_model,
      call = match.call()
    ),
    class = "fcum"
  )

  return(result)
}
