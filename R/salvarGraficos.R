#' Salve os gráficos da sua lista (recursivamente!)
#'
#' @param lista_graficos Uma lista com gráficos (salva recursivo).
#' @param path Pasta para salvar os gráficos.
#' @param progresso Parâmetro usado pela recursão. Não deve ser utilizado
#'
#' @export
#'
salvarGraficos <- function(lista_graficos, path = 'output/graficos', progresso = '') {

  if('list' %in% class(lista_graficos)) {
    if(progresso == '') message('Iniciando...')
    else message(glue::glue('{progresso}: {path}'))

    nomes <- names(lista_graficos)
    if(is.null(nomes)) nomes <- glue::glue('grafico_{1:length(lista_graficos)}')

    for(i in seq_along(lista_graficos)) {
      salvarGraficos(
        lista_graficos[[i]],
        path = glue::glue('{path}/{nomes[i]}'),
        progresso = glue::glue('{progresso}[{i}/{length(lista_graficos)}]')
        )
    }

  } else if(is_ggplot(lista_graficos) | 'recordedplot' %in% class(lista_graficos)) {
    save_settings <- lista_graficos[['save_settings']]

    device <- save_settings[['device']]
    units <- save_settings[['units']]
    dpi <- save_settings[['dpi']]
    width <- save_settings[['width']]
    height <- save_settings[['height']]

    device <- if(is.null(device)) 'png' else device

    suppressMessages(
      ggplot2::ggsave(
        filename = glue::glue('{stringr::str_split_i(path, "/", -1)}.{device}'),
        path =
          stringr::str_split_1(path, '/') %>%
          .[-length(.)] %>%
          str_flatten(collapse = '/'),
        plot = lista_graficos,
        device = device,
        width = if(is.null(width)) NA else width,
        height = if(is.null(height)) NA else height,
        units = if(is.null(units)) 'cm' else units,
        dpi = if(is.null(dpi)) 300 else dpi,
        create.dir = TRUE)
    )

    message(glue::glue('{progresso}: {path}.{device} OK!'))

  } else {
    message(glue::glue('Ignorando {path}'))
  }

}
