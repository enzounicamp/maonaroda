#' Soma 1 na numeração do nome dos arquivos a partir de um determinado número.
#'
#' @param numero A partir deste número (incluso), a numeração dos arquivos será adicionada de 1.
#' @param caminho O caminho (pasta) cujo arquivos se deseja adicionar 1 no nome.
#'
#' @export
#'

nomeArqs.somaUm <- function(numero, caminho) {
	arquivos <- list.files(
		path = caminho,
		pattern = '^[0-9]{2} -',
		recursive = FALSE
	)

	novo_nome <-
		sapply(
			X = arquivos,
			FUN = function(arq) {
				num <- as.numeric(stringr::str_extract(arq, pattern = '^[0-9]{2}'))

				if (num < numero) {
					return(arq)
				}

				arq <- stringr::str_replace(
					arq,
					pattern = '^[0-9]{2}',
					replacement = stringr::str_pad(
						num + 1,
						width = 2,
						side = 'left',
						pad = '0'
					)
				)

				return(arq)
			}
		)

	for (i in seq_along(arquivos)) {
		file.rename(
			from = file.path(caminho, names(novo_nome)[i]),
			to = file.path(caminho, novo_nome[i])
		)
	}
}
