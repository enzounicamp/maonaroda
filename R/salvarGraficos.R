#' Salve os gráficos da sua lista
#'
#' @param lista_graficos Uma lista com gráficos (salva recursivo).
#' @param save_path Pasta para salvar os gráficos.
#'
#' @export
#'
salvarGraficos <- function(lista_graficos, save_path) {
  print('Iniciando...')
  num_graficos <- length(lista_graficos)
  nomes_arqs <- glue('{names(lista_graficos)}.png')

  for(i in seq_along(lista_graficos)) {
    plot_temp <- lista_graficos[[i]]
    save_settings <- plot_temp[['save_settings']]

    device <- save_settings[['device']]
    units <- save_settings[['units']]
    dpi <- save_settings[['dpi']]
    width <- save_settings[['width']]
    height <- save_settings[['height']]
    path <-
      if('relative_save_path' %in% names(save_settings)) file.path(save_path, save_settings[['relative_save_path']])
    else save_path


    ggsave(filename = nomes_arqs[i],
           path = path,
           plot = plot_temp,
           device = if(is.null(device)) 'png' else device,
           width = if(is.null(width)) NA else width,
           height = if(is.null(height)) NA else height,
           units = if(is.null(units)) 'cm' else units,
           dpi = if(is.null(dpi)) 300 else dpi,
           create.dir = TRUE)

    print(glue('({i}/{num_graficos}) {nomes_arqs[i]}'))
  }

  print('Finalizado!')
}
