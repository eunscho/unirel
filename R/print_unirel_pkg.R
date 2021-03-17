DIGITS <- 6
labelled_output <- function(label){
  function(x, ...){
    cat(paste0(label, "\n"))
    cat(round(x[[1]], DIGITS), "\n")
  }
}

print.alpha <- labelled_output("coefficient alpha (tau-equivalent reliability)")
print.mu2 <- labelled_output("Ten Berge and Zegers' mu2")
print.feldt <- labelled_output("Feldt's classical congeneric reliability")
print.gilmer <- labelled_output("Gilmer-Feldt's reliability coefficient")
print.joreskog <- labelled_output("Joreskog's congeneric (unidimensional CFA) coefficient")

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

