#' Obtain Ten Berge and Socan's (2004) mu4
#'
#' Obtain Ten Berge and Socan's (2004) mu4.
#' @author Eunseong Cho, \email{bene@kw.ac.kr}
#' @param data a dataframe or a matrix (unidimensional)
#' @references Ten Berge, J. M. F., & Zegers, F. E. (1978). A series of lower
#' bounds to the reliability of a test. Psychometrika, 43(4), 575-579.
#' @examples mu4(Graham1)

mu4 <- function(data) {
    m <- get_cov(data)
    n <- nrow(m)/(nrow(m) - 1)
    off <- m
    diag(off) <- 0
    numerator <- sum(off) + sqrt(sum(off^2) + sqrt(sum(off^4) + sqrt(sum(off^6) + sqrt(n * sum(off^8)))))
    mu4 <- numerator/sum(m)
    return(mu4)
}
