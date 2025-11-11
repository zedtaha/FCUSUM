#' Print Method for fcum Objects
#'
#' @param x An object of class \code{fcum}
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.fcum <- function(x, ...) {
  cat("\nFourier CUSUM Cointegration Test\n")
  cat("=================================\n\n")
  cat("CUSUM Statistic:", round(x$statistic, 6), x$significance, "\n\n")
  cat("Critical Values:\n")
  cat("  1%: ", round(x$critical_values["1%"], 6), "\n", sep = "")
  cat("  5%: ", round(x$critical_values["5%"], 6), "\n", sep = "")
  cat("  10%:", round(x$critical_values["10%"], 6), "\n\n", sep = "")
  cat("Decision:", x$decision, "\n\n")
  cat("Parameters:\n")
  cat("  Regressors (p):", x$p, "\n")
  cat("  Frequency (k): ", x$k, "\n")
  cat("  Max frequency (k*):", x$kstar, "\n")
  cat("  Optimal frequency:", round(x$best_frequency, 4), "\n")
  invisible(x)
}

#' Summary Method for fcum Objects
#'
#' @param object An object of class \code{fcum}
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
summary.fcum <- function(object, ...) {
  cat("\nFourier CUSUM Cointegration Test - Summary\n")
  cat("==========================================\n\n")
  cat("Call:\n")
  print(object$call)
  cat("\n")

  cat("Test Statistic:\n")
  cat("  CUSUM:", round(object$statistic, 6), object$significance, "\n\n")

  cat("Critical Values:\n")
  cat("  1% level: ", round(object$critical_values["1%"], 6), "\n", sep = "")
  cat("  5% level: ", round(object$critical_values["5%"], 6), "\n", sep = "")
  cat("  10% level:", round(object$critical_values["10%"], 6), "\n\n", sep = "")

  cat("Decision:", object$decision, "\n\n")

  cat("Model Selection:\n")
  cat("  Number of regressors (p):", object$p, "\n")
  cat("  Frequency parameter (k):  ", object$k, "\n")
  cat("  Max frequency (k*):       ", object$kstar, "\n")
  cat("  Optimal frequency:        ", round(object$best_frequency, 4), "\n\n")

  cat("Best Model Summary:\n")
  cat("-------------------\n")
  print(summary(object$best_model))

  invisible(object)
}
