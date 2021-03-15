#' Obtain Ten Berge and Socan's (2004) mu0 to mu5
#'
#' Obtain Ten Berge and Socan's (2004) mu0, mu1, mu2, mu3, mu4, and mu5.
#' @author Eunseong Cho, \email{bene@kw.ac.kr}
#' @param data a dataframe or a matrix (unidimensional)
#' @references Ten Berge, J. M. F., & Zegers, F. E. (1978). A series of lower
#' bounds to the reliability of a test. Psychometrika, 43(4), 575-579.
#' @examples mus(Graham1)
#' @return mu0 Ten Berge and Socan's mu0 reliability coefficient (coefficient alpha)
#' @return mu1 Ten Berge and Socan's mu1 reliability coefficient (Guttman's lambda2)
#' @return mu2 Ten Berge and Socan's mu2 reliability coefficient
#' @return mu3 Ten Berge and Socan's mu3 reliability coefficient
#' @return mu4 Ten Berge and Socan's mu4 reliability coefficient
#' @return mu5 Ten Berge and Socan's mu5 reliability coefficient
#' @seealso [psych::tenberg()] for a related function of the package psych
#'
mus <- function(data) {
    mu0 <- mu0(data)
    mu1 <- mu1(data)
    mu2 <- mu2(data)
    mu3 <- mu3(data)
    mu4 <- mu4(data)
    mu5 <- mu5(data)
    mus <- list(mu0 = mu0, mu1 = mu1, mu2 = mu2, mu3 = mu3, mu4 = mu4, mu5 = mu5)
    return(mus)
}
