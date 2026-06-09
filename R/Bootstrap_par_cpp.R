#' @noRd
validate_cores <- function(cores, B) {
  if (is.null(cores)) {
    return(NULL)
  }

  if (!is.numeric(cores)) {
    stop("cores must be numeric", call. = FALSE)
  }

  if (length(cores) != 1) {
    stop("cores must be a single numeric value", call. = FALSE)
  }

  if (cores != round(cores)) {
    stop("cores must be an integer", call. = FALSE)
  }

  if (cores < 1) {
    stop("cores must be at least 1", call. = FALSE)
  }

  if (cores > B) {
    warning("cores cannot exceed B. Setting cores = B", call. = FALSE)
    cores <- B
  }
  return(cores)
}
#' Parallel Bootstrap with C++ Integration
#'
#' This function allows to user to create an S3 object which contains values
#' based on the (non-)parametric bootstrapping estimated. Compared to the
#' regular bootstrap function, this uses C++ integration and parallel computing
#' for better performance
#'
#' @param X Original sample (numeric vector)
#' @param B Total number of bootstrap replications
#' @param dist Distribution type: "NonParam", "Norm", or "Exp"
#' @param param1 Parameter 1 (mean for Norm, rate for Exp)
#' @param param2 Parameter 2 (sd for Norm, ignored for Exp)
#' @param conf_level Confidence level for intervals (default 0.95)
#' @param cores Number of CPU cores to use for parallel computing. Setting this as
#' "NULL" will reduce this function to "bootstrap_cpp_integrated(...)."
#'
#' @return An S3 object of class "bootstrap" with same structure as
#'         \code{bootstrap_cpp_integrated}
#'
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport parLapply
#' @importFrom stats quantile sd
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' x <- rnorm(100)
#'
#' # Parallel bootstrap with 4 cores
#' result_parallel <- bootstrap_parallel(x, B = 5000, cores = 4)
#' print(result_parallel)
#'
#' # Compare with non-parallel version
#' result_serial <- bootstrap_cpp_integrated(x, B = 5000)
#' }
#' @export
bootstrap_par_cpp <- function(X, B, dist = "NonParam",
                              param1 = NULL, param2 = NULL,
                              conf_level = 0.95, cores = NULL) {

  # Input validation
  validate_X(X)
  validate_B(B)
  validate_conf_level(conf_level)
  validate_dist(dist)
  validate_parameters(dist, param1, param2)
  cores <- validate_cores(cores, B)

  # If cores = NULL, fall back to serial
  if (is.null(cores)) {
    warning("cores = NULL. Falling back to serial bootstrap_cpp_integrated()", call. = FALSE)
    return(bootstrap_cpp_integrated(X, B, dist, param1, param2, conf_level))
  }

  # Split B into chunks (one per core)
  reps_per_core <- rep(floor(B / cores), cores)
  remainder <- B - sum(reps_per_core)
  if (remainder > 0) {
    reps_per_core[1:remainder] <- reps_per_core[1:remainder] + 1
  }

  # Function to run a chunk of bootstrap replications using the C++ backend
  # Note: X, dist, param1, param2 are taken from the closure
  run_chunk <- function(n_reps) {
    boot <- bootstrap_cpp(
      X, n_reps, dist,
      param1 = if (is.null(param1)) NA_real_ else param1,
      param2 = if (is.null(param2)) NA_real_ else param2
    )
    list(means = boot$bootstrap_means, sds = boot$bootstrap_sds)
  }

  # Set up parallel cluster
  cl <- parallel::makeCluster(cores, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Load the package namespace on each worker
  parallel::clusterEvalQ(cl, {
    library(FinalProject, quietly = TRUE)
  })

  # Export necessary objects to workers
  parallel::clusterExport(cl,
                          varlist = c("run_chunk", "X", "dist", "param1", "param2", "reps_per_core"),
                          envir = environment()
  )

  # Run chunks in parallel – call run_chunk with only the number of replications
  results <- parallel::parLapply(cl, seq_len(cores), function(i) {
    run_chunk(reps_per_core[i])   # Only pass n_reps
  })

  # Combine results
  all_means <- unlist(lapply(results, `[[`, "means"))
  all_sds   <- unlist(lapply(results, `[[`, "sds"))

  # Ensure exact length
  if (length(all_means) != B) {
    all_means <- all_means[1:B]
    all_sds   <- all_sds[1:B]
  }

  # Compute statistics
  estimate_mean    <- mean(all_means)
  estimate_sd      <- mean(all_sds)
  estimate_mean_se <- sd(all_means)
  estimate_sd_se   <- sd(all_sds)

  alpha   <- 1 - conf_level
  ci_mean <- quantile(all_means, probs = c(alpha/2, 1 - alpha/2))
  ci_sd   <- quantile(all_sds,   probs = c(alpha/2, 1 - alpha/2))

  # Return S3 object
  result <- list(
    bootstrap_means    = all_means,
    bootstrap_sds      = all_sds,
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
