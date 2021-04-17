#' Obtain Hancock's H (CFA version of maximal reliability)
#'
#' Hancock's is the confirmatory factor analysis (CFA) version of maximal 
#' reliability. This coefficient takes the standardized factor loading as the 
#' reliability of each item, and finds the weight that maximizes the 
#' reliability. Hence, Hancock's H shows a different result than the reliability 
#' estimator using conventional unit weights.
#' @param data a dataframe or a matrix (unidimensional)
#' @return Hancock's H
#' @export hancock
#' @examples hancock(Graham1)
#' @references Cho, E. (in press). Neither Cronbach's alpha nor McDonald's
#' omega: A comment on Sijtsma and Pfadt. Psychometrika.
#' @references Hancock, G., & Mueller, R. O. (2001). Rethinking construct 
#' reliability within latent variable systems. In R. Cudeck, S. du Toit, & D. 
#' Sörbom (Eds.), Structural equation modeling: Present and future-A festschrift 
#' in honor of Karl Jöreskog (pp. 195-216). Scientific Software International.
#' @references Li, H., Rosenthal, R., & Rubin, D. B. (1996). Reliability of 
#' measurement in psychology: From Spearman-Brown to maximal reliability. 
#' Psychological Methods, 1(1), 98-107. 
#' @references McNeish, D. (2017). Thanks coefficient alpha, we’ll take it from 
#' here. Psychological Methods, 23(3), 412-433.
hancock <- function(data) {
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
      hancock <- NA
    } else {
      std_lambda <- lavaan::standardizedSolution(fit)$est.std[1:k]
      prop_lambda <- std_lambda^2 / (1 - std_lambda^2)
      hancock <- 1 / (1 + 1 / sum(prop_lambda))
    }
    class(hancock) <- c("hancock")
    return(hancock)
}
