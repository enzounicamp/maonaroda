#' Salve as tabelas da sua lista
#'
#' @param tabelas_list Uma lista com tabelas (salva recursivo).
#' @param save_path Pasta para salvar os gráficos.
#'
#' @export
#'
salvarTabelas <- function(tabelas_list, save_path) {
  print('Iniciando...')
  num_tabelas <- length(tabelas_list)
  nomes_arqs <- glue('{names(tabelas_list)}.xlsx')

  for(i in seq_along(tabelas_list)) {
    tabela <- tabelas_list[[i]]

    if('list' %in% class(tabela)) {
      save_settings <- tabela[['save_settings']]
      path <-
        if('relative_save_path' %in% names(save_settings))
          file.path(save_path, save_settings[['relative_save_path']])
      else save_path
      tabela <- tabela$tabela
    } else path <- save_path

    if(!dir.exists(path)) dir.create(path, recursive = TRUE)

    write_xlsx(tabela,
               path = file.path(path, nomes_arqs[i])
    )

    print(glue('({i}/{num_tabelas}) {nomes_arqs[i]}'))
  }

  print('Finalizado!')
}
