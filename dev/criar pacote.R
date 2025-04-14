library(devtools)


# use_package("tibble")
# use_package("stringr")
# use_package("dplyr")
use_package("sf")
# usethis::use_import_from('graphics', 'abline')

# usethis::use_data_raw("limites_municipais") #cria arquivos para dados internos

document()
check()
load_all()
