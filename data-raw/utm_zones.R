## code to prepare `utm_zones` dataset goes here
# Carregar pacotes necessários para este script
library(sf)
library(usethis)
library(dplyr) # Opcional, para manipulação de dados se necessário

# 1. Definir o caminho para o arquivo .shp
#    (Use o caminho completo ou relativo a partir da raiz do projeto)
path_to_shp <- "./data-raw/fusos_utm/FUSOS_UTM.shp"
# ou relativo (se o shapefile estiver dentro do projeto, talvez em data-raw/input_data/):
# path_to_shp <- "data-raw/input_data/meu_shapefile.shp"

# 2. Ler o Shapefile como um objeto sf
#    sf::st_read lida automaticamente com a leitura do .shp e seus arquivos associados (.dbf, .shx etc.)
#    Ele também tentará ler e definir o Sistema de Referência de Coordenadas (CRS)
raw_utm_zone_sf <- sf::st_read(path_to_shp) # Ajuste o ENCODING se necessário (comum para dados do Brasil)

# 3. (Opcional) Processar/Limpar os dados
#    - Renomear colunas
#    - Selecionar colunas relevantes
#    - Verificar/Transformar CRS (se necessário)
#    - Filtrar geometrias/atributos
# Exemplo: selecionar colunas e garantir um CRS específico (ex: SIRGAS 2000, EPSG:4674)
utm_zone_sf <- raw_utm_zone_sf %>%
  sf::st_transform(crs = 4674) # Garante que os dados estejam no CRS desejado

# Verificação (opcional, mas recomendado)
# print(head(utm_zone_sf))
# print(sf::st_crs(utm_zone_sf))
# plot(sf::st_geometry(utm_zone_sf))

# 4. Salvar o objeto sf processado para o pacote
#    O nome que você usa aqui será o nome do objeto de dados dentro do pacote.
#    usethis::use_data() salva o objeto no formato .rda dentro do diretório data/
usethis::use_data(utm_zone_sf, overwrite = TRUE)

# Fim do script data-raw/utm_zone.R
