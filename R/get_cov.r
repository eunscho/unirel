#' Obtain the covariance matrix
#'
#' If the input data is a square matrix, it is converted into a matrix,
#' otherwise the covariance matrix is obtained.
#' The covariance matrix uses the cov function of base,
#' and the option for processing missing values is 'pairwise.complete.obs'.
#'
#' @param data A dataframe or a matrix
#' @return The covariance matrix
#' @examples
#' get_cov(Graham1)
get_cov <- function(data) {
    if (nrow(data) == ncol(data)) {
        matrix <- as.matrix(data)
    } else {
        matrix <- cov(data, use = "pairwise.complete.obs")
    }
    return(matrix)
}
