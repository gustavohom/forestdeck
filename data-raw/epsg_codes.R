# --- Código para data-raw/epsg_codes.R (executado uma vez durante o desenvolvimento) ---

# Carregar pacotes necessários para a criação dos dados
# install.packages(c("tibble", "dplyr", "stringr")) # Se necessário
library(tibble)
library(dplyr)
library(stringr)

# Criar o tibble diretamente com os dados
epsg_data_raw <- tibble::tribble(
  ~epsg, ~description,
  # SIRGAS 2000 Geográfico
  4674, "SIRGAS 2000",
  # SIRGAS 2000 UTM Projetado
  5396, "SIRGAS 2000 / UTM zone 26S",
  6210, "SIRGAS 2000 / UTM zone 23N",
  6211, "SIRGAS 2000 / UTM zone 24N",
  31965, "SIRGAS 2000 / UTM zone 11N",
  31966, "SIRGAS 2000 / UTM zone 12N",
  31967, "SIRGAS 2000 / UTM zone 13N",
  31968, "SIRGAS 2000 / UTM zone 14N",
  31969, "SIRGAS 2000 / UTM zone 15N",
  31970, "SIRGAS 2000 / UTM zone 16N",
  31971, "SIRGAS 2000 / UTM zone 17N",
  31972, "SIRGAS 2000 / UTM zone 18N",
  31973, "SIRGAS 2000 / UTM zone 19N",
  31974, "SIRGAS 2000 / UTM zone 20N",
  31975, "SIRGAS 2000 / UTM zone 21N",
  31976, "SIRGAS 2000 / UTM zone 22N",
  31977, "SIRGAS 2000 / UTM zone 17S",
  31978, "SIRGAS 2000 / UTM zone 18S",
  31979, "SIRGAS 2000 / UTM zone 19S",
  31980, "SIRGAS 2000 / UTM zone 20S",
  31981, "SIRGAS 2000 / UTM zone 21S",
  31982, "SIRGAS 2000 / UTM zone 22S",
  31983, "SIRGAS 2000 / UTM zone 23S",
  31984, "SIRGAS 2000 / UTM zone 24S",
  31985, "SIRGAS 2000 / UTM zone 25S",
  # WGS 84 Geográfico
  4326, "WGS 84",
  # WGS 84 UTM Projetado (Norte)
  32601, "WGS 84 / UTM zone 1N", 32602, "WGS 84 / UTM zone 2N", 32603, "WGS 84 / UTM zone 3N",
  32604, "WGS 84 / UTM zone 4N", 32605, "WGS 84 / UTM zone 5N", 32606, "WGS 84 / UTM zone 6N",
  32607, "WGS 84 / UTM zone 7N", 32608, "WGS 84 / UTM zone 8N", 32609, "WGS 84 / UTM zone 9N",
  32610, "WGS 84 / UTM zone 10N", 32611, "WGS 84 / UTM zone 11N", 32612, "WGS 84 / UTM zone 12N",
  32613, "WGS 84 / UTM zone 13N", 32614, "WGS 84 / UTM zone 14N", 32615, "WGS 84 / UTM zone 15N",
  32616, "WGS 84 / UTM zone 16N", 32617, "WGS 84 / UTM zone 17N", 32618, "WGS 84 / UTM zone 18N",
  32619, "WGS 84 / UTM zone 19N", 32620, "WGS 84 / UTM zone 20N", 32621, "WGS 84 / UTM zone 21N",
  32622, "WGS 84 / UTM zone 22N", 32623, "WGS 84 / UTM zone 23N", 32624, "WGS 84 / UTM zone 24N",
  32625, "WGS 84 / UTM zone 25N", 32626, "WGS 84 / UTM zone 26N", 32627, "WGS 84 / UTM zone 27N",
  32628, "WGS 84 / UTM zone 28N", 32629, "WGS 84 / UTM zone 29N", 32630, "WGS 84 / UTM zone 30N",
  32631, "WGS 84 / UTM zone 31N", 32632, "WGS 84 / UTM zone 32N", 32633, "WGS 84 / UTM zone 33N",
  32634, "WGS 84 / UTM zone 34N", 32635, "WGS 84 / UTM zone 35N", 32636, "WGS 84 / UTM zone 36N",
  32637, "WGS 84 / UTM zone 37N", 32638, "WGS 84 / UTM zone 38N", 32639, "WGS 84 / UTM zone 39N",
  32640, "WGS 84 / UTM zone 40N", 32641, "WGS 84 / UTM zone 41N", 32642, "WGS 84 / UTM zone 42N",
  32643, "WGS 84 / UTM zone 43N", 32644, "WGS 84 / UTM zone 44N", 32645, "WGS 84 / UTM zone 45N",
  32646, "WGS 84 / UTM zone 46N", 32647, "WGS 84 / UTM zone 47N", 32648, "WGS 84 / UTM zone 48N",
  32649, "WGS 84 / UTM zone 49N", 32650, "WGS 84 / UTM zone 50N", 32651, "WGS 84 / UTM zone 51N",
  32652, "WGS 84 / UTM zone 52N", 32653, "WGS 84 / UTM zone 53N", 32654, "WGS 84 / UTM zone 54N",
  32655, "WGS 84 / UTM zone 55N", 32656, "WGS 84 / UTM zone 56N", 32657, "WGS 84 / UTM zone 57N",
  32658, "WGS 84 / UTM zone 58N", 32659, "WGS 84 / UTM zone 59N", 32660, "WGS 84 / UTM zone 60N",
  # WGS 84 UTM Projetado (Sul)
  32701, "WGS 84 / UTM zone 1S", 32702, "WGS 84 / UTM zone 2S", 32703, "WGS 84 / UTM zone 3S",
  32704, "WGS 84 / UTM zone 4S", 32705, "WGS 84 / UTM zone 5S", 32706, "WGS 84 / UTM zone 6S",
  32707, "WGS 84 / UTM zone 7S", 32708, "WGS 84 / UTM zone 8S", 32709, "WGS 84 / UTM zone 9S",
  32710, "WGS 84 / UTM zone 10S", 32711, "WGS 84 / UTM zone 11S", 32712, "WGS 84 / UTM zone 12S",
  32713, "WGS 84 / UTM zone 13S", 32714, "WGS 84 / UTM zone 14S", 32715, "WGS 84 / UTM zone 15S",
  32716, "WGS 84 / UTM zone 16S", 32717, "WGS 84 / UTM zone 17S", 32718, "WGS 84 / UTM zone 18S",
  32719, "WGS 84 / UTM zone 19S", 32720, "WGS 84 / UTM zone 20S", 32721, "WGS 84 / UTM zone 21S",
  32722, "WGS 84 / UTM zone 22S", 32723, "WGS 84 / UTM zone 23S", 32724, "WGS 84 / UTM zone 24S",
  32725, "WGS 84 / UTM zone 25S", 32726, "WGS 84 / UTM zone 26S", 32727, "WGS 84 / UTM zone 27S",
  32728, "WGS 84 / UTM zone 28S", 32729, "WGS 84 / UTM zone 29S", 32730, "WGS 84 / UTM zone 30S",
  32731, "WGS 84 / UTM zone 31S", 32732, "WGS 84 / UTM zone 32S", 32733, "WGS 84 / UTM zone 33S",
  32734, "WGS 84 / UTM zone 34S", 32735, "WGS 84 / UTM zone 35S", 32736, "WGS 84 / UTM zone 36S",
  32737, "WGS 84 / UTM zone 37S", 32738, "WGS 84 / UTM zone 38S", 32739, "WGS 84 / UTM zone 39S",
  32740, "WGS 84 / UTM zone 40S", 32741, "WGS 84 / UTM zone 41S", 32742, "WGS 84 / UTM zone 42S",
  32743, "WGS 84 / UTM zone 43S", 32744, "WGS 84 / UTM zone 44S", 32745, "WGS 84 / UTM zone 45S",
  32746, "WGS 84 / UTM zone 46S", 32747, "WGS 84 / UTM zone 47S", 32748, "WGS 84 / UTM zone 48S",
  32749, "WGS 84 / UTM zone 49S", 32750, "WGS 84 / UTM zone 50S", 32751, "WGS 84 / UTM zone 51S",
  32752, "WGS 84 / UTM zone 52S", 32753, "WGS 84 / UTM zone 53S", 32754, "WGS 84 / UTM zone 54S",
  32755, "WGS 84 / UTM zone 55S", 32756, "WGS 84 / UTM zone 56S", 32757, "WGS 84 / UTM zone 57S",
  32758, "WGS 84 / UTM zone 58S", 32759, "WGS 84 / UTM zone 59S", 32760, "WGS 84 / UTM zone 60S"
)

# Processar a tabela para extrair informações estruturadas
epsg_data <- epsg_data_raw %>%
  dplyr::mutate(
    # Extrair o sistema base (SIRGAS 2000 ou WGS 84)
    system = dplyr::case_when(
      stringr::str_detect(description, "SIRGAS 2000") ~ "SIRGAS 2000",
      stringr::str_detect(description, "WGS 84") ~ "WGS 84",
      TRUE ~ NA_character_ # Caso algo inesperado ocorra
    ),
    # Identificar o tipo (geográfico ou projetado) e extrair a zona
    type = dplyr::if_else(stringr::str_detect(description, "UTM zone"), "projected", "geographic"),
    zone = stringr::str_extract(description, "\\d{1,2}[NS]$") # Extrai '##N' ou '##S' do final
  ) %>%
  # Criar coluna de busca padronizada (maiúsculas, underscore)
  dplyr::mutate(
    system_lookup = stringr::str_replace_all(toupper(system), " ", "_")
  ) %>%
  # Selecionar e reordenar colunas finais
  dplyr::select(epsg, system, system_lookup, type, zone, description) %>%
  # Ordenar para consistência (opcional)
  dplyr::arrange(system, type, zone)

# Visualizar a tabela processada (apenas para verificação)
# print(epsg_data)
# print(dplyr::glimpse(epsg_data))

# SALVAR OS DADOS INTERNOS (comando a ser executado no console com o pacote carregado via devtools)
usethis::use_data(epsg_data, internal = TRUE, overwrite = TRUE)
