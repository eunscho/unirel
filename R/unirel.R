#' Obtain various unidimensional reliablity coefficients
#'
#' R packages that provide unidimensional reliability coefficients include
#' Lambda4 and psych. These are usually separate functions, so comparing
#' multiple reliability estimates can take a long time. This function shows the
#'  reliability coefficients newly added in this package together with the
#'  reliability coefficients provided by Lambda4 and psych. This function can
#'  provide Table 2 of Cho(in press) immediately.
#'
#'@author Eunseong Cho, \email{bene@kw.ac.kr}
#'@param data a dataframe or a matrix (unidimensional)
#'@param Lambda4.include Whether to include the reliability coefficients
#'provided by the package Lambda4 (lambda5, lambda6, max_lambda, lambda4_max,
#'lambda4_75)
#'@param psych.include Whether to include reliability coefficients
#'(GLB.algebraic, GLB.fa) provided by the package psych
#'@return alpha coefficient alpha
#'@return lambda2 Guttman's lambda2
#'@return mu2 Ten Berge and Zegers' mu2
#'@return feldt Feldt's classical congeneric reliability coefficient
#'@return gilmer the Gilmer-Fedlt coefficient
#'@return joreskog unidimensional CFA (congeneric) reliability
#'@return lambda5 Guttman's lambda5
#'@return lambda5 Guttman's lambda6
#'@return max_lambda the maximum among lambda2, lambda5, and lambda6
#'@return lambda4_max the maximum among all possible lambda4
#'@return lambda4_75 the 75the percentile among all possible lambda4
#'@return glb.algebraic the original estimation of the greatest lower bounds
#'@return glb.fa an improvement of the greatest lower bounds by the psych package
#'@export unirel
#' @import Rcsdp
#' @references Cho, E. (in press). Neither Cronbach's alpha nor McDonald's
#' omega: A comment on Sijtsma and Pfadt. Psychometrika.
#' @examples unirel(Graham1)
#' @seealso [Lambda4::lambda5()] for a related function of the package Lambda4
#' @seealso [Lambda4::lambda6()] for a related function of the package Lambda4
#' @seealso [Lambda4::quant.lambda4()] for a related function of the package Lambda4
#' @seealso [psych::GLB.algebraic()] for a related function of the package psych
#' @seealso [psych::GLB.fa()] for a related function of the package psych

unirel <- function(data, Lambda4.include = TRUE, psych.include = TRUE) {
    alpha <- alpha(data)
    lambda2 <- mu1(data)
    mu2 <- mu2(data)
    feldt <- feldt(data)
    gilmer <- gilmer(data)
    joreskog <- joreskog(data)
    if (Lambda4.include) {
        stopifnot(requireNamespace("Lambda4"))
        lambda5 <- as.numeric(Lambda4::lambda5(data, missing = "pairwise"))
        lambda6 <- as.numeric(Lambda4::lambda6(data, missing = "pairwise"))
        max_lambda <- max(lambda2, lambda5, lambda6)
        lambda4_max <- Lambda4::quant.lambda4(data, missing = "pairwise",
                                     quantiles = 1)$lambda4.quantile
        lambda4_75 <- Lambda4::quant.lambda4(data, missing = "pairwise",
                                    quantiles = 0.75)$lambda4.quantile
    } else {
        lambda5 <- lambda6 <- max_lambda <- lambda4_max <- lambda4_75 <- NULL
    }
    if (psych.include) {
        stopifnot(requireNamespace("psych"))
        stopifnot(requireNamespace("Rcsdp"))
        glb.algebraic <- psych::glb.algebraic(data)$glb[1]
        glb.fa <- psych::glb.fa(data)$glb[1]
    } else {
        glb.algebraic <- glb.fa <- NULL
    }
    unirel <- list(alpha = alpha, lambda2 = lambda2,
                   mu2 = mu2, feldt = feldt,
                   gilmer = gilmer, joreskog = joreskog, lambda5 = lambda5,
                   lambda6 = lambda6, max_lambda = max_lambda,
                   lambda4_max = lambda4_max, lambda4_75 = lambda4_75,
                   glb.algebraic = glb.algebraic, glb.fa = glb.fa)
    class(unirel) <- c("unirel")
    print.unirel <- function(x, ...){
        cat("14 unidimensional reliability coefficients\n")
        cat("coefficient alpha (tau-equivalent reliability)\n")
        cat(round(x$alpha, DIGITS))
        cat("\nGuttman's lambda2\n")
        cat(round(x$lambda2, DIGITS))
        cat("\nTen Berge and Zegers' mu2\n")
        cat(round(x$mu2, DIGITS))
        cat("\nFeldt's classical congeneric reliability\n")
        cat(round(x$feldt, DIGITS))
        cat("\nGilmer-Feldt's reliability coefficient\n")
        cat(round(x$gilmer, DIGITS))
        cat("\nJoreskog's congeneric (unidimensional CFA) coefficient\n")
        cat(round(x$joreskog, DIGITS))
        cat("\nGuttman's lambda5\n")
        cat(round(x$lambda5, DIGITS))
        cat("\nGuttman's lambda6\n")
        cat(round(x$lambda6, DIGITS))
        cat("\nMaximum among Guttman's lambda2, lambda5, and lambda6\n")
        cat(round(x$max_lambda, DIGITS))
        cat("\nMaximum among all possible split-half reliability(lambda4)\n")
        cat(round(x$lambda4_max, DIGITS))
        cat("\n75th percentile among all possible split-half reliability(lambda4)\n")
        cat(round(x$lambda4_75, DIGITS))
        cat("\nGLB.algebraic (greatest lower bounds)\n")
        cat(round(x$glb.algebraic, DIGITS))
        cat("\nGLB.fa (greatest lower bounds)\n")
        cat(round(x$glb.fa, DIGITS))
    }
    return(unirel)
}
