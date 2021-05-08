#' Unidimensional confirmatory factor analysis
#' 
#' @param cov observed covariances
#' @param what e.g., "est", "std", "fit"
#' @param nonneg_loading if TRUE, constraint loadings to nonnegative values
#' @param taueq if TRUE, a tau-equivalent model is estimated
#' @examples uni_cfa(Graham1)
#' @export uni_cfa
#' @return parameter estimates of unidimensional cfa model
uni_cfa <- function(cov, what = "est", nonneg_loading = FALSE, taueq = FALSE) {
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
      }
  }
  colnames(cov) <- rownames(cov)
  model_str <- paste0(model_str, " \n F ~~ 1*F", collapse = "\n")
  if (!taueq) { # congeneric
    for (i in 1:k) { # to prevent negative errors
      model_str <- paste0(model_str, "\n V", i, " ~~ e", i, "*V", i, "\n e", i, "> 0.0000001")
      if (i > 1 & nonneg_loading) { # prevent negative loadings
        model_str <- paste0(model_str, "\n l", i, "> .0000001")
      }
    }
  }  
  fit <- lavaan::cfa(model_str, sample.cov = cov, sample.nobs = 500)
  if (lavaan::inspect(fit, what = "converged")) {
    out <- lavaan::inspect(fit, what = what)
  } else {
    out <- NA
  }
  return(out)
}
