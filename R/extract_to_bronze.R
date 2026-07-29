#' Extract dataset from UCI Machine Learning Repository and archive
#'
#' @param name A string. References a dataset downloadable from UCI machine
#' learning repository
#' @returns A dataset, which is a subset of input list
#' @examples
#' extract_to_bronze('Chronic Kidney Disease')
extract_to_bronze <- function(name) {
  mlr_data <- fetch_ucirepo(name = name)

  just_data = mlr_data$data
  X = just_data$features
  Y = just_data$targets

  dataset = as.data.frame(cbind(X, Y))

  return(dataset)
}
