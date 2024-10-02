#' Instala e carrega pacotes do CRAN e do GitHub
#'
#' A funçao `u_load` automatiza o processo de instalaçao, atualizaçao e carregamento de pacotes do CRAN e do GitHub.
#' Ela verifica se os pacotes estao instalados, instala os que nao estao, atualiza os desatualizados e carrega todos os pacotes especificados.
#' Ao final, exibe um resumo dos pacotes instalados, atualizados, carregados e possiveis erros que ocorreram durante o processo.
#'
#' Os pacotes podem ser fornecidos de diferentes maneiras:
#' - **Nomes sem aspas** (simbolos), como `dplyr`
#' - **Nomes com aspas** (strings), como `"dplyr"`
#' - **Vetores de caracteres**, como `c("dplyr", "ggplot2")`
#' - **Repositorios do GitHub**, no formato `usuario/repositorio`, com ou sem aspas
#'
#' @param ... Lista de pacotes a serem processados. Podem ser nomes de pacotes do CRAN ou do GitHub fornecidos como simbolos (sem aspas),
#'            strings (com aspas) ou vetores de caracteres. Repositorios do GitHub devem estar no formato `usuario/repositorio`.
#'
#' @return A funçao nao retorna um valor, mas produz saida no console com informações sobre o processo.
#'
#' @examples
#' \dontrun{
#' # Instalar e carregar pacotes do CRAN usando nomes sem aspas
#' u_load(dplyr, ggplot2)
#'
#' # Instalar e carregar pacotes do CRAN usando strings
#' u_load('dplyr', 'ggplot2')
#'
#' # Instalar e carregar pacotes do GitHub usando nomes sem aspas
#' u_load(hadley/emo, tidyverse/ggplot2)
#'
#' # Instalar e carregar pacotes do GitHub usando strings
#' u_load('hadley/emo', 'tidyverse/ggplot2')
#'
#' # Usar um objeto que contem nomes de pacotes
#' my_packages <- c('dplyr', 'ggplot2', 'gustavohom/forestdeck')
#' u_load(my_packages)
#'
#' # Combinar diferentes formatos
#' u_load(dplyr, 'ggplot2', hadley/emo, 'tidyverse/ggplot2', my_packages)
#' }
#'
#' @export

u_load <- function(...) {
  # Captura os argumentos como expressões
  args <- match.call(expand.dots = FALSE)$...

  # Cria um vetor vazio para armazenar os pacotes
  pkg <- c()

  for (i in seq_along(args)) {
    # Captura o nome do argumento como string
    arg_name <- deparse(args[[i]])

    # Verifica se o objeto existe no ambiente pai
    if (exists(arg_name, envir = parent.frame())) {
      # Usa `get` apenas se o objeto for do tipo character
      obj_class <- class(get(arg_name, envir = parent.frame()))

      # Se o objeto for do tipo character, pega o valor e adiciona
      if (obj_class == "character") {
        arg_value <- get(arg_name, envir = parent.frame())
        pkg <- append(pkg, arg_value)
      } else {
        # Caso contrario, adiciona apenas o nome do objeto
        pkg <- append(pkg, arg_name)
      }
    } else {
      # Se o objeto nao existir, adiciona o nome do argumento
      arg_name <- gsub('[\'"]', '', arg_name)
      pkg <- append(pkg, arg_name)
    }
  }

  # Identifica pacotes do GitHub e do CRAN
  github <- grep("^.*?/.*?$", pkg, value = TRUE)
  github_names <- sub("^.*/", "", github)
  cran <- pkg[!(pkg %in% github)]

  pkg_names <- sub("^.*/", "", pkg)

  # Lista de pacotes instalados e disponiveis
  installed <- utils::installed.packages()[, "Package"]
  old <- utils::old.packages()
  old_pkgs <- if(!is.null(old)) rownames(old) else character(0)

  # Pacotes que precisam ser instalados
  not_installed <- pkg_names[!pkg_names %in% installed]

  # Pacotes que precisam ser atualizados
  to_update <- pkg_names[pkg_names %in% old_pkgs]

  # Inicializa variaveis para armazenar mensagens e erros
  installed_cran <- character(0)
  installed_github <- character(0)
  updated_pkgs <- character(0)
  loaded_pkgs <- character(0)
  errors_cran <- list()
  errors_github <- list()
  errors_load <- list()

  # Instala pacotes do CRAN
  for (pkg_name in cran) {
    pkg_short_name <- sub("^.*/", "", pkg_name)
    if (pkg_short_name %in% not_installed) {
      result <- try(utils::install.packages(pkg_short_name), silent = TRUE)
      if (inherits(result, "try-error")) {
        errors_cran[[pkg_short_name]] <- geterrmessage()
      } else if (pkg_short_name %in% utils::installed.packages()[, "Package"]) {
        installed_cran <- c(installed_cran, pkg_short_name)
      } else {
        errors_cran[[pkg_short_name]] <- "Falha ao instalar do CRAN."
      }
    }
  }

  # Instala pacotes do GitHub
  if (length(github) > 0) {
    if (!"remotes" %in% utils::installed.packages()[, "Package"]) {
      utils::install.packages("remotes")
    }
    for (repo in github) {
      pkg_short_name <- sub("^.*/", "", repo)
      if (pkg_short_name %in% not_installed) {
        result <- try(remotes::install_github(repo, upgrade = "always"), silent = TRUE)
        if (inherits(result, "try-error")) {
          errors_github[[pkg_short_name]] <- geterrmessage()
        } else if (pkg_short_name %in% utils::installed.packages()[, "Package"]) {
          installed_github <- c(installed_github, pkg_short_name)
        } else {
          errors_github[[pkg_short_name]] <- "Falha ao instalar do GitHub."
        }
      }
    }
  }

  # Atualiza pacotes
  for (pkg_name in to_update) {
    result <- try(utils::install.packages(pkg_name), silent = TRUE)
    if (inherits(result, "try-error")) {
      errors_cran[[pkg_name]] <- geterrmessage()
    } else {
      updated_pkgs <- c(updated_pkgs, pkg_name)
    }
  }

  # Carrega os pacotes
  for (p in pkg_names) {
    if (p %in% utils::installed.packages()[, "Package"]) {
      result <- try(suppressPackageStartupMessages(library(p, character.only = TRUE)), silent = TRUE)
      if (inherits(result, "try-error")) {
        errors_load[[p]] <- geterrmessage()
      } else {
        loaded_pkgs <- c(loaded_pkgs, p)
      }
    } else {
      errors_load[[p]] <- "Pacote nao esta instalado."
    }
  }

  cat('\n',
      '#--------------------------------------------------\n',
      '#\n',
      '#                PROCESSO CONCLUIDO               \n',
      '#\n',
      '#--------------------------------------------------\n\n')

  # Exibe as mensagens
  if (length(installed_cran) > 0) {
    cat("Pacotes instalados do CRAN:\n")
    cat(paste("- ", installed_cran, collapse = "\n"), "\n\n")
  }

  if (length(installed_github) > 0) {
    cat("Pacotes instalados do GitHub:\n")
    cat(paste("- ", installed_github, collapse = "\n"), "\n\n")
  }

  if (length(updated_pkgs) > 0) {
    cat("Pacotes atualizados:\n")
    cat(paste("- ", updated_pkgs, collapse = "\n"), "\n\n")
  }

  if (length(loaded_pkgs) > 0) {
    cat("Pacotes carregados:\n")
    cat(paste("- ", loaded_pkgs, collapse = "\n"), "\n\n")
  }

  # Exibe erros do CRAN
  if (length(errors_cran) > 0) {
    cat("Ocorreram erros ao instalar ou atualizar pacotes do CRAN:\n")
    for (name in names(errors_cran)) {
      cat("Erro no pacote '", name, "': ", errors_cran[[name]], "\n", sep = "")
    }
    cat("\n")
  }

  # Exibe erros do GitHub
  if (length(errors_github) > 0) {
    cat("Ocorreram erros ao instalar pacotes do GitHub:\n")
    for (name in names(errors_github)) {
      cat("Erro no pacote '", name, "': ", errors_github[[name]], "\n", sep = "")
    }
    cat("\n")
  }

  # Exibe erros ao carregar pacotes
  if (length(errors_load) > 0) {
    cat("Ocorreram erros ao carregar os pacotes:\n")
    for (name in names(errors_load)) {
      cat("Erro no pacote '", name, "': ", errors_load[[name]], "\n", sep = "")
    }
    cat("\n")
  }
}
