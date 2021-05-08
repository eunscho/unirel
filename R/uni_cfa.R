#' Unidimensional confirmatory factor analysis
#' 
#' @param cov observed covariances
#' @param what e.g., "est", "std", "fit"
#' @param taueq if TRUE, a tau-equivalent model is estimated
#' @export uni_cfa
#' @return parameter estimates of unidimensional cfa model
#' @examples uni_cfa(Graham1)
uni_cfa <- function(cov, what = "est", nonneg_loading =FALSE, taueq = FALSE) {
  stopifnot(requireNamespace("lavaan"))
  k <- nrow(cov)
  rownames(cov) <- character(length = k)
  for (i in 1:k) {
    rownames(cov)[i] <- paste0("V", i)
    if (i == 1) {
      model_str <- paste("F =~ NA*V1")
    } else if (taueq) { # tau-equivalent
      model_str <- paste0(model_str, " + equal('F=~V1')*V", i)
    } else { # congeneric
      model_str <- paste0(model_str, " + l", i, "*V", i)
      if (nonneg_loading) {
        model_str <- paste0(model_str, "l", i, "> .0000001")
      }
    }
  }
  model_str <- paste0(model_str, " \n F ~~ 1*F", collapse = "\n")
  for (i in 1:k) { # to prevent negative errors
    model_str <- paste0(model_str, "\n V", i, " ~~ l", i, "*V", i, "\n l", i, "> 0.0000001")
  }
  colnames(cov) <- rownames(cov)
  fit <- lavaan::cfa(model_str, sample.cov = cov, sample.nobs = 500)
  out <- lavaan::inspect(fit, what = what)
  return(out)
}
