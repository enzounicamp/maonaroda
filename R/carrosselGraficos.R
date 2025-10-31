#' Printa todos os gráficos de uma lista recursivamente!
#'
#' @param lista_graficos Uma lista com gráficos.
#' @param iterativo Para a cada gráfico printado e espera o usuário apertar "Enter".
#' @param nome_var Um nome que será usado no nome da variável no print no prompt.
#'
#' @export
#'
carrosselGraficos <- function(lista_graficos, iterativo = FALSE, nome_var = '') {

  if('list' %in% class(lista_graficos)) {
    lista_nomes <- glue::glue('{nome_var}/{names(lista_graficos)}')
    for(i in seq_along(lista_graficos)) {
      carrosselGraficos(lista_graficos[[i]], iterativo = iterativo, nome_var = lista_nomes[i])
    }
  } else if(is_ggplot(lista_graficos) | 'recordedplot' %in% class(lista_graficos)) {
    print(lista_graficos)
    if(iterativo) readline(glue::glue('{nome_var} (Enter para passar): '))
    else Sys.sleep(0.1)
  }

  return(NULL)

}
