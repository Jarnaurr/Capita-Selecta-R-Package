
#' Password Guesser
#'
#' This function allows to user to create an S3 object which contains values
#' based on the (non-)parametric bootstrapping estimated. Compared to the
#' regular bootstrap function, this uses C++ integration in Rcpp for better
#' performance
#'
#' @param X Original sample
#' @param B Number of replications
#' @param dist Specified distribution, allows for (non-)parametric estimation
#' @param param1 Parameter 1 of distribution in case of parametric estimation
#' @param param2 Parameter 2 of distribution in case of parametric estimation
#' @param conf_level Confidence levels for inference statistics
#'
#' @returns An S3 object of class "bootstrap"
#' @export
