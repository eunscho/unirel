#' Obtain classical congeneric reliability coefficient
#'
#' Feldt's classical congeneric reliability (Feldt & Brennan 1989) is a unidimensional  reliability
#'  coefficient based on a congeneric model. The congeneric model is a model
#' that allows the length, discrimination, or importance of items to be
#' different, and is the least restrictive model among the models derived
#' from the classical test theory. The congeneric reliability proposed by
#' Joreskog (1971) uses an optimization technique called maximum likelihood to
#' estimate the "length" of an item. Classical congeneric reliablity uses a
#' simpler logic, using the ratio of the sum of the covariance of the item to
#' the sum of the total covariance as an estimate of the length of the item
#' (Cho 2016).This coefficient is slightly less accurate than the Gilmer-Fedlt
#' coefficient or congeneric reliability (Cho in press).
#'
#' @usage cc(data)
#' @param data a dataframe or a matrix (unidimensional)
#' @return classical congeneric reliability coefficient
#' @examples cc(Graham1)
#' @references Cho, E. (2016). Making reliability reliable: A systematic
#' approach to reliability coefficients. Organizational Research Methods, 19(4),
#'  651–682.
#' @references Cho, E. (in press). Neither Cronbach's alpha nor McDonald's
#' omega: A comment on Sijtsma and Pfadt. Psychometrika.
#' @references Feldt, L. S., & Brennan, R. L. (1989). Reliability.
#' In R. L. Linn (Ed.), Educational measurement (3rd ed., pp. 105–146).
#' American Council on Education and Macmillan.
#' @references Jöreskog, K. G. (1971). Statistical analysis of sets of
#' congeneric tests. Psychometrika, 36(2), 109–133.
#' @seealso [gilmer()] for the Gilmer-Fedlt coefficient
#' @seealso [cr()] for congeneric reliability
#'
cc <- function(data) {
  matrix <- get_cov(data)
  k <- nrow(matrix)
  total <- sum(matrix)
  nondiag_sum <- total - sum(diag(matrix))
  sqrd_lambda  <- 0
  for (i in 1:k) {
    sqrd_lambda = sqrd_lambda + (rowSums(matrix)[i] / (total)) ^ 2
  }
  cc <- nondiag_sum / ((1 - sqrd_lambda) * total)
  return(cc)
}
