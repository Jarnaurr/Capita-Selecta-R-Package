#' @noRd
bootstrap_cpp <- function(X, B, dist, param1 = NA_real_, param2 = NA_real_) {
  .Call(`_FinalProject_bootstrap_cpp`, X, B, dist, param1, param2)}

#' Bootstrap with C++ integration
#'
#' This function allows to user to create an S3 object which contains values
#' based on the (non-)parametric bootstrapping estimated. Compared to the
#' regular bootstrap function, this uses C++ integration in Rcpp for better
#' performance
#'
#' @param X Original sample
#' @param B Number of replications
#' @param dist Specified distribution, allows for (non-)parametric estimation
#' must be "NonParam", "Norm" or "Exp", default is "NonParam"
#' @param param1 Parameter 1 of distribution in case of parametric estimation
#' @param param2 Parameter 2 of distribution in case of parametric estimation
#' @param conf_level Confidence levels for inference statistics
#'
#' @importFrom stats quantile sd
#'
#' @returns An S3 object of class "bootstrap"
#' @examples
#' \donttest{
#' set.seed(67)
#' data <- rnorm(500)
#'
#' # Non-parametric bootstrap
#'
#' result_cpp <- bootstrap_cpp_integrated(data, B = 1000, dist = "NonParam")
#' print(result_cpp)
#' summary(result_cpp)
#' plot(result_cpp)
#'
#' # Normal parametric bootstrap with estimated parameters
#' result_norm_cpp <- bootstrap_cpp_integrated(data, B = 1000, dist = "Norm",
#'                                         param1 = mean(data),
#'                                         param2 = sd(data))
#'}
#' @export
bootstrap_cpp_integrated <- function(X, B, dist = "NonParam", param1 = NULL, param2 = NULL, conf_level = 0.95) {

  # Run validators to verify input
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

  #Returns a neat S3 object to run predefined methods on
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


