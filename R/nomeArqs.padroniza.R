#' Padroniza a numeração dos nomes dos arquivos.
#'
#' @param caminho O caminho (pasta) cujo arquivos se deseja padronizar o nome.
#' @param recursivo Se os nomes devem ser padronizados recursivamente.
#'
#' @export

nomeArqs.padroniza <- function(caminho = 'R', recursivo = TRUE) {
	arquivos <- list.files(
		path = caminho,
		pattern = '^[0-9]{1,2} -',
		full.names = FALSE,
		recursive = FALSE
	)

	is.dir <- dir.exists(file.path(caminho, arquivos))

	dirs <- arquivos[is.dir]

	if (length(dirs) > 0 && recursivo) {
		sapply(dirs, function(dir) {
			nomeArqs.padroniza(file.path(caminho, dir))
		})
	}

	names(arquivos) <- arquivos

	for (i in seq_along(arquivos)) {
		arquivos[i] <- stringr::str_replace(
			string = arquivos[i],
			pattern = '^[0-9]{1,2}',
			replacement = stringr::str_pad(
				i,
				width = 2,
				side = 'left',
				pad = '0'
			)
		)
	}

	for (i in seq_along(arquivos)) {
		file.rename(
			from = file.path(caminho, names(arquivos)[i]),
			to = file.path(caminho, arquivos[i])
		)
	}

	return(TRUE)
}
