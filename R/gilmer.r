#' Obtain the Gilmer-Feldt reliability coefficient
#'
#' The Gilmer-Feldt coefficient is a unidimensional reliability coefficient
#' based on a congeneric model. The congeneric model is a model that allows
#' the length, discrimination, or importance of items to be different,
#' and is the least restrictive model among the models derived from
#' the classical test theory. The Gilmer-Feldt coefficient has the advantage
#' of being less computational than congeneric reliability (Joreskog 1971)
#' which uses confirmatory factor analysis.However, the Gilmer-Feldt coefficient
#'  derives a value very close to congeneric reliability (Cho in press).
#'  Feldt and Charter (2003) offers a user-friendly review of the Gilmer-Feldt
#'  coefficient.
#'
#' @param data A dataframe or a matrix (unidimensional)
#' @usage gilmer(data)
#' @return The Gilmer-Fedlt coefficient
#' @examples gilmer(Graham1)
#' @references Cho, E. (in press). Neither Cronbach's alpha nor McDonald's omega: A comment on Sijtsma and Pfadt. Psychometrika.
#' @references Feldt, L. S., & Charter, R. A. (2003). Estimation of internal consistency reliability when test parts vary in effective length. Measurement and Evaluation in Counseling and Development, 36(1), 23-27
#' @references Gilmer, J. S., & Feldt, L. S. (1983). Reliability estimation for a test with parts of unknown lengths. Psychometrika, 48(1), 99–111.
#' @references Jöreskog, K. G. (1971). Statistical analysis of sets of congeneric tests. Psychometrika, 36(2), 109–133.
#'
#'
gilmer <- function(data) {
  matrix <- get_cov(data)
  k <- nrow(matrix)
  total <- sum(matrix)
  nondiag <- numeric(k)
  for (i in 1:k) {
    nondiag[i] <- rowSums(matrix)[i] - diag(matrix)[i]
  }
  key_row <- matrix[order(nondiag)[k], ] # A, B, C, D in Feldt and Charter
  D <- (nondiag - key_row) / (max(nondiag) - key_row)
  Q <- sum(D)^2
  W <- 0
  for (i in 1:k) {
    W <- W + D[i]^2
  }
  gilmer <- (Q / (Q - W)) * (sum(nondiag) / total)
  return(gilmer)
}
