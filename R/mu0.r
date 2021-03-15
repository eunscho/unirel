#' Obtain Ten Berge and Socan's (2004) mu0
#'
#' Obtain Ten Berge and Socan's (2004) mu0. mu0 equals coefficient alpha.
#' @author Eunseong Cho, \email{bene@kw.ac.kr}
#' @param data a dataframe or a matrix (unidimensional)
#' @references Ten Berge, J. M. F., & Zegers, F. E. (1978). A series of lower
#' bounds to the reliability of a test. Psychometrika, 43(4), 575-579.
#' @examples mu0(Graham1)
#' @seealso [alpha()]

mu0 <- function(data) {
    m <- get_cov(data)
    n <- nrow(m)/(nrow(m) - 1)
    off <- m
    diag(off) <- 0
    mu0 <- n * sum(off)/sum(m)
    return(mu0)
}
