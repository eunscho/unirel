#' Obtain Joreskog's congeneric reliability (Unidimensional CFA reliability)
#'
#' Congeneric reliability is a reliability coefficient derived from
#' unidimensional confirmatory factor analysis (CFA).
#'
#'  Features: Congeneric reliability is a unidimensional reliability
#' coefficient based on a unidimensional confirmatory factor analysis (CFA)
#' model.
#'
#' Name: Congeneric reliability is called by a variety of names, general users
#' usually call it composite reliability, and reliability researchers often call
#' it omega. One of the reasons for this confusion is that studies that first
#' proposed this coefficient (Joreskog 1971) did not give this formula a name
#'  (Cho 2016). Joreskog (1971) proposed a matrix-form formula, and the
#'  commonly known non-matrix formula appears in Werts et al. (1974).
#'
#' Frequency of use: Congeneric reliability is the second most commonly used
#' reliability coefficient after coefficient alpha (Cho 2016)
#'
#' Accuracy: Congeneric reliability is the most accurate reliability coefficient
#' along with the Feldt-Gilmer coefficient (Cho in press)
#'
#' Computation: This function uses maximum likelihood as estimation,
#' unstandardized covariance matrix as input, and lavaan package as software.
#'
#' @param data a dataframe or a matrix (unidimensional)
#' @return congeneric reliability coefficient
#' @export joreskog
#' @examples joreskog(Graham1)
#' @references Cho, E. (2016). Making reliability reliable: A systematic
#' approach to reliability coefficients. Organizational Research Methods, 19(4),
#' 651-682.
#' @references Cho, E. (in press). Neither Cronbach's alpha nor McDonald's
#' omega: A comment on Sijtsma and Pfadt. Psychometrika.
#' @references Jöreskog, K. G. (1971). Statistical analysis of sets of
#' congeneric tests. Psychometrika, 36(2), 109-133.
#' @references Werts, C. E., Linn, R. L., & Jöreskog, K. G. (1974). Intraclass
#' reliability estimates: Testing structural assumptions. Educational and
#' Psychological Measurement, 34, 25-33.
#' @seealso [gilmer()] for the Gilmer-Fedlt coefficient
#' @seealso [feldt()] for classical congeneric reliability coefficient
#' @seealso [psych::omega()] for a related function of the package psych
#' @seealso [MBESS::ci.reliability()] for a related function of the package MBESS
#' @seealso [Lambda4::omega.tot()] for a related function of the package Lambda4
#' @family congenerics
#'
joreskog <- function(data) {
    stopifnot(requireNamespace("lavaan"))
    matrix <- get_cov(data)
    k <- nrow(matrix)
    rownames(matrix) <- character(length = k)
    for (i in 1:k) {
        rownames(matrix)[i] <- paste("V", i, sep = "")
        if (i == 1) {
            model_str <- paste("F =~ NA*V1")
        } else {
            model_str <- paste(model_str, " + V", i, sep = "")
        }
    }
    model_str <- paste(model_str, " \n F ~~ 1*F", sep = "", collapse = "\n")
    colnames(matrix) <- rownames(matrix)
    fit <- lavaan::cfa(model_str, sample.cov = matrix, sample.nobs = 500)
    est <- lavaan::inspect(fit, what = "est")
    if(any(est$theta < 0)) { # negative error
      joreskog <- NA
    } else {
      sum_lambda <- sum(est$lambda)
      sum_theta <- sum(est$theta)
      joreskog <- sum_lambda^2/(sum_lambda^2 + sum_theta)
    }
    class(joreskog) <- c("joreskog")
    return(joreskog)
}
