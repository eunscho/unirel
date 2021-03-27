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
#'@return alpha coefficient alpha (tau-equivalent reliability)
#'@return lambda2 Guttman's lambda2
#'@return mu2 Ten Berge and Zegers' mu2
#'@return mu3 Ten Berge and Zegers' mu3
#'@return mu4 Ten Berge and Zegers' mu4
#'@return feldt Feldt's classical congeneric reliability coefficient
#'@return gilmer the Gilmer-Fedlt coefficient
#'@return joreskog unidimensional CFA (congeneric) reliability
#'@return hancock Hancock's H
#'@return heise Heise-Borhnstedt's Omega
#'@return kaisercaffrey Kaiser-Caffrey's alpha
#'@return lambda5 Guttman's lambda5
#'@return lambda5 Guttman's lambda6
#'@return max_lambda the maximum among lambda2, lambda5, and lambda6
#'@return lambda4_max the maximum among all possible lambda4
#'@return lambda4_75 the 75the percentile among all possible lambda4
#'@return glb.fa the greatest lower bounds by Ten Berge & Kiers (1991) method
#'@return glb.algebraic  the greatest lower bounds by Moltner & Revelle (2015) method
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
    alpha <- unirel::alpha(data)
    lambda2 <- mu1(data)
    mu2 <- mu2(data)
    mu3 <- mu3(data)
    mu4 <- mu4(data)
    feldt <- feldt(data)
    gilmer <- gilmer(data)
    joreskog <- joreskog(data)
    hancock <- hancock(data)
    heise <- heise(data)
    kaisercaffrey <- kaisercaffrey(data)
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
        glb.algebraic <- psych::glb.algebraic(data)$glb[1]
        glb.fa <- psych::glb.fa(data)$glb[1]
    } else {
        glb.algebraic <- glb.fa <- NULL
    }
    unirel <- list(alpha = alpha, lambda2 = lambda2,
                   mu2 = mu2, mu3 = mu3, mu4 = mu4, feldt = feldt,
                   gilmer = gilmer, joreskog = joreskog, hancock = hancock,
                   heise = heise, kaisercaffrey = kaisercaffrey,
                   lambda5 = lambda5,lambda6 = lambda6, max_lambda = max_lambda,
                   lambda4_max = lambda4_max, lambda4_75 = lambda4_75,
                   glb.algebraic = glb.algebraic, glb.fa = glb.fa)
    class(unirel) <- c("unirel")

    return(unirel)
}
