#' @noRd
validate_X <- function(X) {
  if (!is.numeric(X)) {
    stop("X must be a numeric vector", call. = FALSE)
  }
  if (!is.vector(X) || is.list(X)) {
    stop("X must be a vector", call. = FALSE)
  }
  if (length(X) < 2) {
    stop("X must contain at least 2 observations for bootstrapping", call. = FALSE)
  }
  return(TRUE)
}

#' @noRd
validate_B <- function(B) {
  if (length(B) != 1) {
    stop("B must be a single integer value, not a vector of length ", length(B), call. = FALSE)
  }
  if (!is.numeric(B)) {
    stop("B must be a numeric value", call. = FALSE)
  }
  if (B != round(B)) {
    stop("B must be an integer", call. = FALSE)
  }
  if (B <= 0) {
    stop("B must be strictly positive", call. = FALSE)
  }
  if (B > 1e6) {
    warning("B is very large (> 1,000,000). This may take a long time.", call. = FALSE)
  }
  return(TRUE)
}

#' @noRd
validate_conf_level <- function(conf_level) {
  if (length(conf_level) != 1) {
    stop("conf_level must be a single numeric value", call. = FALSE)
  }
  if (!is.numeric(conf_level)) {
    stop("conf_level must be numeric", call. = FALSE)
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1 (exclusive)", call. = FALSE)
  }
  return(TRUE)
}

#' @noRd
validate_dist <- function(dist) {
  valid_distributions <- c("NonParam", "Norm", "Exp")
  if (!(dist %in% valid_distributions)) {
    stop("dist must be one of: ", paste(valid_distributions, collapse = ", "), call. = FALSE)
  }
  return(TRUE)
}

#' @noRd
validate_parameters <- function(dist, param1, param2) {
  if (dist == "Norm") {
    if (is.null(param2)) {
      stop("For Norm, param2 (sd) is required", call. = FALSE)
    }
    if (!is.numeric(param2)) {
      stop("For Norm, param2 (sd) must be numeric", call. = FALSE)
    }
    if (param2 <= 0) {
      stop("For Norm, param2 (sd) must be positive", call. = FALSE)
    }
    if (!is.null(param1) && !is.numeric(param1)) {
      stop("For Norm, param1 (mean) must be numeric", call. = FALSE)
    }
  }

  if (dist == "Exp") {
    if (is.null(param1)) {
      stop("For Exp, param1 (rate) is required", call. = FALSE)
    }
    if (!is.numeric(param1)) {
      stop("For Exp, param1 (rate) must be numeric", call. = FALSE)
    }
    if (param1 <= 0) {
      stop("For Exp, param1 (rate) must be positive", call. = FALSE)
    }
    if (!is.null(param2)){
      warning("For Exp, param2 is unused")
    }
  }

  return(TRUE)
}
#' Bootstrap
#'
#' This function allows to user to create an S3 object which contains values
#' based on the (non-)parametric bootstrapping estimated.
#'
#' @param X Original sample
#' @param B Number of replications
#' @param dist Specified distribution, allows for (non-)parametric estimation
#' must be "NonParam", "Norm" or "Exp", default is "NonParam"
#' @param param1 Parameter 1 of distribution in case of parametric estimation
#' @param param2 Parameter 2 of distribution in case of parametric estimation
#' @param conf_level Confidence levels for inference statistics
#'
#' @importFrom
#' stats quantile sd rnorm rexp
#'
#' @returns An S3 object of class "bootstrap"
#' @examples
#' \donttest{
#' set.seed(67)
#' data <- rnorm(500)
#'
#' # Non-parametric bootstrap
#'
#' result <- bootstrap(data, B = 1000, dist = "NonParam")
#' print(result)
#' summary(result)
#' plot(result)
#'
#' # Normal parametric bootstrap with estimated parameters
#'
#' result_norm <- bootstrap(data, B = 1000, dist = "Norm",
#'                                         param1 = mean(data),
#'                                         param2 = sd(data))
#'summary(result_norm)
#'}

#' @export
bootstrap <- function(X, B, dist = "NonParam", param1 = NULL, param2 = NULL, conf_level = 0.95) {

  # Run validator functions to verify input
  validate_X(X)
  validate_B(B)
  validate_conf_level(conf_level)
  validate_dist(dist)
  validate_parameters(dist, param1, param2)

  n <- length(X)
  bootstrap_means <- numeric(B)
  bootstrap_sds <- numeric(B)

  if (dist == "NonParam") {
    for (i in 1:B) {
      bs_sample <- sample(X, n, replace = TRUE)
      bootstrap_means[i] <- mean(bs_sample)
      bootstrap_sds[i] <- sd(bs_sample)
    }
  } else if (dist == "Norm") {
    if (is.null(param2)) stop("For Norm, param2 (sd) is required")
    for (i in 1:B) {
      bs_sample <- rnorm(n, mean = param1, sd = param2)
      bootstrap_means[i] <- mean(bs_sample)
      bootstrap_sds[i] <- sd(bs_sample)
    }
  } else if (dist == "Exp") {
    for (i in 1:B) {
      bs_sample <- rexp(n, rate = param1)
      bootstrap_means[i] <- mean(bs_sample)
      bootstrap_sds[i] <- sd(bs_sample)
    }
  } else {
    stop("'dist' must be 'NonParam', 'Norm', or 'Exp'")
  }

  #Compute the estimates of the mean and standard deviation
  estimate_mean <- mean(bootstrap_means)
  estimate_sd <- mean(bootstrap_sds)

  #Compute SE
  estimate_mean_se <- sd(bootstrap_means)
  estimate_sd_se <- sd(bootstrap_sds)

  # Compute percentile confidence intervals for the mean
  alpha <- 1 - conf_level
  ci_mean <- quantile(bootstrap_means, probs = c(alpha/2, 1 - alpha/2))
  ci_sd   <- quantile(bootstrap_sds,   probs = c(alpha/2, 1 - alpha/2))

  # Create object with class "bootstrap"
  result <- list(
    bootstrap_means = bootstrap_means,
    bootstrap_sds = bootstrap_sds,
    call = match.call(),
    B = B,
    dist = dist,
    parameters = list(param1 = param1, param2 = param2),
    mean = estimate_mean,
    standard_deviation = estimate_sd,
    mean_se = estimate_mean_se,
    sd_se = estimate_sd_se,
    conf_level = conf_level,
    ci_mean = ci_mean,
    ci_sd = ci_sd
  )
  class(result) <- "bootstrap"
  return(result)
}


#' Summary method for "bootstrap" objects
#'
#' @param object Bootstrap object
#' @param ... Additional arguments
#' @export
summary.bootstrap <- function(object, ...) {
  # Build a coefficients-style table, similar to summary.lm
  coef_table <- data.frame(
    Estimate   = c(object$mean, object$standard_deviation),
    Std.Error  = c(object$mean_se, object$sd_se),
    CI.Lower   = c(object$ci_mean[1], object$ci_sd[1]),
    CI.Upper   = c(object$ci_mean[2], object$ci_sd[2]),
    row.names  = c("Mean", "Std. Deviation")
  )

  result <- list(
    call       = object$call,
    dist       = object$dist,
    parameters = object$parameters,
    B          = object$B,
    conf_level = object$conf_level,
    table      = coef_table
  )

  class(result) <- "summary.bootstrap"
  return(result)
}

#' Print method for "summary.bootstrap" objects
#'
#' @param object bootstrap object
#' @param ... additional arguments
#' @export
print.summary.bootstrap <- function(x, digits = 4, ...) {
  cat("Call:\n")
  print(x$call)

  cat("\nBootstrap Settings:\n")
  cat("  Distribution        :", x$dist, "\n")
  if (!is.null(x$parameters$param1))
    cat("  param1              :", x$parameters$param1, "\n")
  if (!is.null(x$parameters$param2))
    cat("  param2              :", x$parameters$param2, "\n")
  cat("  Replications (B)    :", x$B, "\n")
  cat("  Confidence level    :", x$conf_level, "\n")

  cat("\nEstimates:\n")
  printCoefmat(x$table, digits = digits)

  invisible(x)
}

#' Plot method for "bootstrap" objects
#'
#' @param object bootstrap object
#' @param ... additional arguments
#' @export
plot.bootstrap <- function(object, ...) {
  old_par <- par(ask = TRUE)
  on.exit(par(old_par))

  # Histogram of bootstrap means
  hist(object$bootstrap_means,
       main = "Bootstrap Distribution: Mean",
       xlab = "Bootstrap Means",
       col = "lightgrey",
       border = "white",
       ...)
  abline(v = object$mean, col = "black", lwd = 2)
  abline(v = object$ci_mean, col = "red", lwd = 2, lty = 2)

  # Histogram of bootstrap SDs
  hist(object$bootstrap_sds,
       main = "Bootstrap Distribution: SD",
       xlab = "Bootstrap SDs",
       col = "lightgrey",
       border = "white",
       ...)
  abline(v = object$standard_deviation, col = "black", lwd = 2)
  abline(v = object$ci_sd, col = "red", lwd = 2, lty = 2)
}

