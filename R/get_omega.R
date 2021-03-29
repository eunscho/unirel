get_omega <- function(data) {
  psych <- psych::omega(data, nfactors = 1)
  psych.omega <- psych$omega.tot
  Lambda4.omega <- Lambda4::omega.tot(data, factors = 1)
  omega <- list(psych.omega = psych.omega, Lambda4.omega = Lambda4.omega)
  return(omega)
}