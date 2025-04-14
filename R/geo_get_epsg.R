#' Obter Código EPSG para SIRGAS 2000 ou WGS 84
#'
#' Retorna o código EPSG numérico correspondente a um sistema de coordenadas
#' (SIRGAS 2000 ou WGS 84) e, opcionalmente, uma zona UTM.
#'
#' @param system_name Nome do sistema: "SIRGAS_2000" ou "WGS_84" (não
#'   diferencia maiúsculas/minúsculas).
#' @param utm_zone String opcional indicando a zona UTM (ex: "23S", "10N").
#'   Se `NULL` ou omitido, retorna o código EPSG geográfico do sistema.
#'   (não diferencia maiúsculas/minúsculas).
#'
#' @return Um valor `integer` representando o código EPSG, ou `NA_integer_`
#'   se a combinação não for encontrada na base de dados interna.
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @examples
#' \dontrun{
#' # --- Exemplos (requerem que o pacote esteja carregado) ---
#'
#' # SIRGAS 2000
#' get_epsg("SIRGAS_2000")          # Geográfico -> 4674
#' get_epsg("sirgas_2000", "23s") # Projetado -> 31983
#' get_epsg("SIRGAS_2000", "15N") # Projetado -> 31969
#'
#' # WGS 84
#' get_epsg("WGS_84")             # Geográfico -> 4326
#' get_epsg("wgs_84", "32n")    # Projetado -> 32632
#' get_epsg("WGS_84", "1S")     # Projetado -> 32701
#'
#' # Casos não encontrados
#' get_epsg("SIRGAS_2000", "99S") # Retorna NA_integer_
#' get_epsg("NAD83")             # Gera erro (sistema inválido)
#' }
geo_get_epsg <- function(system_name = 'SIRGAS_2000', utm_zone = NULL) {

  # Padronizar e validar sistema
  valid_systems <- c("SIRGAS_2000", "WGS_84")
  sys_lookup <- toupper(system_name)

  if (!sys_lookup %in% valid_systems) {
    stop("Sistema de coordenadas inválido. Use 'SIRGAS_2000' ou 'WGS_84'.")
  }

  # Filtrar com base na presença ou ausência de utm_zone
  if (is.null(utm_zone) || utm_zone == "") {
    # --- Busca Geográfica ---
    result <- epsg_data |> # epsg_data é acessada diretamente (dados internos)
      dplyr::filter(system_lookup == sys_lookup, type == "geographic")

  } else {
    # --- Busca Projetada ---
    zone_lookup <- toupper(utm_zone)

    # Validação simples do formato da zona
    if (!stringr::str_detect(zone_lookup, "^\\d{1,2}[NS]$")) {
      warning("Formato da zona UTM inválido: '", utm_zone,
              "'. Use o formato como '23S' ou '5N'. Retornando NA.", call. = FALSE)
      return(NA_integer_)
    }

    result <- epsg_data |> # epsg_data é acessada diretamente
      dplyr::filter(system_lookup == sys_lookup, type == "projected", zone == zone_lookup)
  }

  # Retornar resultado
  if (nrow(result) == 1) {
    return(result$epsg)
  } else {
    # Não encontrado (ou, inesperadamente, mais de 1 - o que não deve acontecer aqui)
    warning("Combinação de sistema '", system_name, "'",
            if (!is.null(utm_zone) && utm_zone != "") paste0(" e zona '", utm_zone, "'"),
            " não encontrada. Retornando NA.", call. = FALSE)
    return(NA_integer_)
  }
}
