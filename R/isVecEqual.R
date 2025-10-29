#' Compara se dois vetores possuem os mesmos elementos sem se importar com a ordem
#'
#' @param v1 Vetor 1.
#' @param v2 Vetor 2.
#'
#' @returns TRUE ou FALSE
#' @export
#'
isVecEqual <- function(v1, v2) {
  v1 <- as.vector(v1)
  v2 <- as.vector(v2)
  return(identical(sort(v1), sort(v2)))
}
