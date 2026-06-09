#' @noRd
bootstrap_cpp <- function(X, B, dist, param1 = NA_real_, param2 = NA_real_) {
  .Call(`_FinalProject_bootstrap_cpp`, X, B, dist, param1, param2)}

#' Bootstrap with cpp integration
#'
#' @param X Original sample
#' @param B Number of replications
#' @param dist Specified distribution, allows for (non-)parametric estimation
#' @param param1 Parameter of distribution in case of parametric estimation
#' @param param2 Parameter of distribution in case of parametric estimation
#' @param conf_level Confidence levels for inference statistics
#'
#' @returns An S3 object of class "bootstrap"
#' @export
bootstrap_cpp_integrated <- function(X, B, dist = "NonParam", param1 = NULL, param2 = NULL, conf_level = 0.95) {

  # Run all validations
  validate_X(X)
  validate_B(B)
  validate_conf_level(conf_level)
  validate_dist(dist)
  validate_parameters(dist, param1, param2)

  # Call C++ loop
  boot <- bootstrap_cpp(
    X, B, dist,
    param1 = if (is.null(param1)) NA_real_ else param1,
    param2 = if (is.null(param2)) NA_real_ else param2
  )

  bootstrap_means <- boot$bootstrap_means
  bootstrap_sds   <- boot$bootstrap_sds

  estimate_mean    <- mean(bootstrap_means)
  estimate_sd      <- mean(bootstrap_sds)
  estimate_mean_se <- sd(bootstrap_means)
  estimate_sd_se   <- sd(bootstrap_sds)

  alpha   <- 1 - conf_level
  ci_mean <- quantile(bootstrap_means, probs = c(alpha/2, 1 - alpha/2))
  ci_sd   <- quantile(bootstrap_sds,   probs = c(alpha/2, 1 - alpha/2))

  result <- list(
    bootstrap_means    = bootstrap_means,
    bootstrap_sds      = bootstrap_sds,
    call               = match.call(),
    B                  = B,
    dist               = dist,
    parameters         = list(param1 = param1, param2 = param2),
    mean               = estimate_mean,
    standard_deviation = estimate_sd,
    mean_se            = estimate_mean_se,
    sd_se              = estimate_sd_se,
    conf_level         = conf_level,
    ci_mean            = ci_mean,
    ci_sd              = ci_sd
  )
  class(result) <- "bootstrap"
  return(result)
}

#' @examples
#'
