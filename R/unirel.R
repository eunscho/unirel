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
#'@return mu5 Ten Berge and Zegers' mu5
#'@return cc Feldt's classical congeneric reliability coefficient
#'@return gilmer the Gilmer-Fedlt coefficient
#'@return cr unidimensional CFA (congeneric) reliability
#'@return lambda5 Guttman's lambda5
#'@return lambda5 Guttman's lambda6
#'@return max_lambda the maximum among lambda2, lambda5, and lambda6
#'@return lambda4_max the maximum among all possible lambda4
#'@return lambda4_75 the 75the percentile among all possible lambda4
#'@return glb.algebraic the original estimation of the greatest lower bounds
#'@return glb.fa an improvement of the greatest lower bounds by the psych package
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
    mu5 <- mu5(data)
    cc <- cc(data)
    gilmer <- gilmer(data)
    cr <- cr(data)
    if (Lambda4.include) {
        stopifnot(require(Lambda4))
        lambda5 <- as.numeric(lambda5(data, missing = "pairwise"))
        lambda6 <- as.numeric(lambda6(data, missing = "pairwise"))
        max_lambda <- max(lambda2, lambda5, lambda6)
        lambda4_max <- quant.lambda4(data, missing = "pairwise",
                                     quantiles = 1)$lambda4.quantile
        lambda4_75 <- quant.lambda4(data, missing = "pairwise",
                                    quantiles = 0.75)$lambda4.quantile
    } else {
        lambda5 <- lambda6 <- max_lambda <- lambda4_max <- lambda4_75 <- NULL
    }
    if (psych.include) {
        stopifnot(require(psych))
        stopifnot(require(Rcsdp))
        glb.algebraic <- glb.algebraic(data)$glb[1]
        glb.fa <- glb.fa(data)$glb[1]
    } else {
        glb.algebraic <- glb.fa <- NULL
    }
    unirel <- list(alpha = alpha, lambda2 = lambda2, mu5 = mu5, cc = cc,
                   gilmer = gilmer, cr = cr, lambda5 = lambda5,
                   lambda6 = lambda6, max_lambda = max_lambda,
                   lambda4_max = lambda4_max, lambda4_75 = lambda4_75,
                   glb.algebraic = glb.algebraic, glb.fa = glb.fa)
    return(unirel)
}
