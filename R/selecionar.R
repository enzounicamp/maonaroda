#' @title Selecionar: seleciona e altera o nome!
#' @description Seleciona e altera o nome das colunas de um data frame utilizando uma *expr*.
#'
#' @param dados Um data frame
#' @param ... Argumentos do tipo expr(*Nome*) = '*nome*', onde *Nome* é o nome de display da coluna, e '*nome*' é o nome da coluna que será renomeada.
#'
#' @returns Retorna o data frame com as colunas selecionadas e os nomes atualizados
#' @export
#'
#'
#'
#' @examples
#' df <- datasets::iris
#' COL_SEPAL_LENGTH <- rlang::expr(`Sepal Length`)
#' selecionar(df, COL_SEPAL_LENGTH = 'Sepal.Length')
selecionar <- function(dados, ...) {
  args <- rlang::enexprs(...)
  cols <- sapply(args, FUN = rlang::as_name)

  cols_dados <- colnames(dados)
  mask_vector <- cols %in% cols_dados

  if(!all(cols %in% cols_dados)) {
    cols_ausentes <- cols[!(cols %in% cols_dados)]
    cols <- cols[cols %in% cols_dados]
    warning(glue::glue('As colunas {str_flatten_comma(cols_ausentes)} foram passadas como argumento e não estão presentes nos dados. Prosseguindo apenas com as que estão.'))
  }

  dados <- dplyr::select(dados, dplyr::all_of(cols))

  col_names <-
    sapply(names(args)[mask_vector], function(nome) {
      x <- get(nome)
      return(rlang::as_name(x))
    })

  colnames(dados) <- col_names

  return(dados)

}
