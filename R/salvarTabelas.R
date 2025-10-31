#' Salve as tabelas da sua lista
#'
#' @param tabelas_list Uma lista com tabelas (salva recursivo).
#' @param formato 'csv' ou 'xlsx'.
#' @param save_path Pasta para salvar as tabelas.
#' @param progresso Parâmetro usado pela recursão. Não deve ser utilizado
#'
#' @export
#'
salvarTabelas <- function(lista_tabelas, formato = 'csv', path = 'output/tabelas', progresso = '') {

  if('list' %in% class(lista_tabelas)) {
    if(progresso == '') message('Iniciando...')
    else message(glue::glue('{progresso}: {path}'))

    if(!dir.exists(path)) dir.create(path, recursive = TRUE)

    nomes <- names(lista_tabelas)
    if(is.null(nomes)) nomes <- glue::glue('tabela_{1:length(lista_tabelas)}')

    for(i in seq_along(lista_tabelas)) {
      salvarTabelas(
        lista_tabelas[[i]],
        formato = formato,
        path = file.path(path, nomes[i]),
        progresso = glue::glue('{progresso}[{i}/{length(lista_tabelas)}]')
      )
    }

  } else if(is.data.frame(lista_tabelas)) {

    if(formato == 'csv') {
      path <- glue::glue('{path}.csv')
      readr::write_csv(lista_tabelas, file = path)
    } else if(formato == 'xlsx') {
      path <- glue::glue('{path}.xlsx')
      writexl::write_xlsx(
        lista_tabelas,
        path = path
      )
    } else(stop(glue::glue('Formato "{formato}" não reconhecido.')))


    message(glue::glue('{progresso}: {path} OK!'))
  } else {
    message(glue::glue('Ignorando {path}'))
  }

}
