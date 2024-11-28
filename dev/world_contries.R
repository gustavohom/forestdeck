# Carregar bibliotecas necessárias
library(dplyr)
library(tibble)

world_contries <- function(){

# África

# Norte da África
northern_africa <- tibble(
  Country = c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia"),
  Code = c("ALG", "EGY", "LBA", "MAR", "SUD", "TUN"),
  Continent = "Africa",
  Subcontinent = "Northern Africa"
)

# Oeste da África
western_africa <- tibble(
  Country = c("Benin", "Burkina Faso", "Cabo Verde", "Côte d’Ivoire", "Gambia", "Ghana",
              "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Mauritania", "Niger",
              "Nigeria", "Senegal", "Sierra Leone", "Togo"),
  Code = c("BEN", "BUR", "CPV", "CIV", "GAM", "GHA",
           "GUI", "GBS", "LBR", "MLI", "MTN", "NIG",
           "NGR", "SEN", "SLE", "TOG"),
  Continent = "Africa",
  Subcontinent = "Western Africa"
)

# África Central
middle_africa <- tibble(
  Country = c("Angola", "Cameroon", "Central African Republic", "Chad", "Congo",
              "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon",
              "Sao Tome and Principe"),
  Code = c("ANG", "CMR", "CAF", "CHA", "CGO",
           "COD", "GEQ", "GAB", "STP"),
  Continent = "Africa",
  Subcontinent = "Middle Africa"
)

# Leste da África
eastern_africa <- tibble(
  Country = c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya",
              "Madagascar", "Malawi", "Mauritius", "Mozambique", "Rwanda", "Seychelles",
              "Somalia", "South Sudan", "Uganda", "United Republic of Tanzania",
              "Zambia", "Zimbabwe"),
  Code = c("BDI", "COM", "DJI", "ERI", "ETH", "KEN",
           "MAD", "MAW", "MRI", "MOZ", "RWA", "SEY",
           "SOM", "SSD", "UGA", "TAN",
           "ZAM", "ZIM"),
  Continent = "Africa",
  Subcontinent = "Eastern Africa"
)

# Sul da África
southern_africa <- tibble(
  Country = c("Botswana", "Lesotho", "Namibia", "South Africa", "Eswatini"),
  Code = c("BOT", "LES", "NAM", "RSA", "SWZ"),
  Continent = "Africa",
  Subcontinent = "Southern Africa"
)

# Américas

# América do Norte
north_america <- tibble(
  Country = c("Canada", "United States of America", "Mexico"),
  Code = c("CAN", "USA", "MEX"),
  Continent = "Americas",
  Subcontinent = "North America"
)

# América Central
central_america <- tibble(
  Country = c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama"),
  Code = c("BIZ", "CRC", "ESA", "GUA", "HON", "NCA", "PAN"),
  Continent = "Americas",
  Subcontinent = "Central America"
)

# Caribe
caribbean <- tibble(
  Country = c("Antigua and Barbuda", "Aruba", "Bahamas", "Barbados", "Bermuda",
              "Cayman Islands", "Cuba", "Dominica", "Dominican Republic", "Grenada",
              "Haiti", "Jamaica", "Puerto Rico", "Saint Kitts and Nevis", "Saint Lucia",
              "Saint Vincent and the Grenadines", "Trinidad and Tobago", "Virgin Islands, British",
              "Virgin Islands, US"),
  Code = c("ANT", "ARU", "BAH", "BAR", "BER",
           "CAY", "CUB", "DMA", "DOM", "GRN",
           "HAI", "JAM", "PUR", "SKN", "LCA",
           "VIN", "TTO", "IVB", "ISV"),
  Continent = "Americas",
  Subcontinent = "Caribbean"
)

# América do Sul
south_america <- tibble(
  Country = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
              "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"),
  Code = c("ARG", "BOL", "BRA", "CHI", "COL", "ECU",
           "GUY", "PAR", "PER", "SUR", "URU", "VEN"),
  Continent = "Americas",
  Subcontinent = "South America"
)

# Ásia

# Ásia Ocidental
western_asia <- tibble(
  Country = c("Bahrain", "Iraq", "Jordan", "Kuwait", "Lebanon", "Oman", "Palestine",
              "Qatar", "Saudi Arabia", "Syrian Arab Republic", "United Arab Emirates", "Yemen"),
  Code = c("BRN", "IRQ", "JOR", "KUW", "LBN", "OMA", "PLE",
           "QAT", "KSA", "SYR", "UAE", "YEM"),
  Continent = "Asia",
  Subcontinent = "Western Asia"
)

# Ásia Central
central_asia <- tibble(
  Country = c("Afghanistan", "Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan"),
  Code = c("AFG", "KAZ", "KGZ", "TJK", "TKM", "UZB"),
  Continent = "Asia",
  Subcontinent = "Central Asia"
)

# Ásia Meridional
southern_asia <- tibble(
  Country = c("Bangladesh", "Bhutan", "India", "Iran", "Maldives", "Nepal", "Pakistan", "Sri Lanka"),
  Code = c("BAN", "BHU", "IND", "IRI", "MDV", "NEP", "PAK", "SRI"),
  Continent = "Asia",
  Subcontinent = "Southern Asia"
)

# Ásia Oriental
eastern_asia <- tibble(
  Country = c("People’s Republic of China", "Japan", "Democratic People's Republic of Korea",
              "Republic of Korea", "Mongolia", "Chinese Taipei", "Hong Kong, China"),
  Code = c("CHN", "JPN", "PRK", "KOR", "MGL", "TPE", "HKG"),
  Continent = "Asia",
  Subcontinent = "Eastern Asia"
)

# Sudeste Asiático
southeast_asia <- tibble(
  Country = c("Brunei Darussalam", "Cambodia", "Indonesia", "Lao People's Democratic Republic",
              "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand",
              "Democratic Republic of Timor-Leste", "Vietnam"),
  Code = c("BRU", "CAM", "INA", "LAO",
           "MAS", "MYA", "PHI", "SGP", "THA",
           "TLS", "VIE"),
  Continent = "Asia",
  Subcontinent = "Southeast Asia"
)

# Europa

# Europa Setentrional
northern_europe <- tibble(
  Country = c("Denmark", "Estonia", "Finland", "Iceland", "Ireland", "Latvia", "Lithuania",
              "Norway", "Sweden", "United Kingdom"),
  Code = c("DEN", "EST", "FIN", "ISL", "IRL", "LAT", "LTU",
           "NOR", "SWE", "GBR"),
  Continent = "Europe",
  Subcontinent = "Northern Europe"
)

# Europa Ocidental
western_europe <- tibble(
  Country = c("Austria", "Belgium", "France", "Germany", "Liechtenstein", "Luxembourg",
              "Monaco", "Netherlands", "Switzerland"),
  Code = c("AUT", "BEL", "FRA", "GER", "LIE", "LUX",
           "MON", "NED", "SUI"),
  Continent = "Europe",
  Subcontinent = "Western Europe"
)

# Europa Oriental
eastern_europe <- tibble(
  Country = c("Belarus", "Bulgaria", "Czechia", "Hungary", "Poland", "Republic of Moldova",
              "Romania", "Russian Federation", "Slovakia", "Ukraine", "Georgia", "Armenia", "Azerbaijan"),
  Code = c("BLR", "BUL", "CZE", "HUN", "POL", "MDA",
           "ROU", "RUS", "SVK", "UKR", "GEO", "ARM", "AZE"),
  Continent = "Europe",
  Subcontinent = "Eastern Europe"
)

# Europa Meridional
southern_europe <- tibble(
  Country = c("Albania", "Andorra", "Bosnia and Herzegovina", "Croatia", "Greece", "Italy",
              "Kosovo", "Malta", "Montenegro", "North Macedonia", "Portugal", "San Marino",
              "Serbia", "Slovenia", "Spain", "Türkiye", "Cyprus", "Israel"),
  Code = c("ALB", "AND", "BIH", "CRO", "GRE", "ITA",
           "KOS", "MLT", "MNE", "MKD", "POR", "SMR",
           "SRB", "SLO", "ESP", "TUR", "CYP", "ISR"),
  Continent = "Europe",
  Subcontinent = "Southern Europe"
)

# Oceania

# Australásia
australasia <- tibble(
  Country = c("Australia", "New Zealand"),
  Code = c("AUS", "NZL"),
  Continent = "Oceania",
  Subcontinent = "Australasia"
)

# Melanésia
melanesia <- tibble(
  Country = c("Fiji", "Papua New Guinea", "Solomon Islands", "Vanuatu"),
  Code = c("FIJ", "PNG", "SOL", "VAN"),
  Continent = "Oceania",
  Subcontinent = "Melanesia"
)

# Micronésia
micronesia <- tibble(
  Country = c("Federated States of Micronesia", "Guam", "Kiribati", "Marshall Islands",
              "Nauru", "Palau"),
  Code = c("FSM", "GUM", "KIR", "MHL", "NRU", "PLW"),
  Continent = "Oceania",
  Subcontinent = "Micronesia"
)

# Polinésia
polynesia <- tibble(
  Country = c("American Samoa", "Cook Islands", "Samoa", "Tonga", "Tuvalu"),
  Code = c("ASA", "COK", "SAM", "TGA", "TUV"),
  Continent = "Oceania",
  Subcontinent = "Polynesia"
)

# Combinar todos os tibbles em um único data frame
world_data <- bind_rows(
  # África
  northern_africa, western_africa, middle_africa, eastern_africa, southern_africa,
  # Américas
  north_america, central_america, caribbean, south_america,
  # Ásia
  western_asia, central_asia, southern_asia, eastern_asia, southeast_asia,
  # Europa
  northern_europe, western_europe, eastern_europe, southern_europe,
  # Oceania
  australasia, melanesia, micronesia, polynesia
)

return(world_data)

}


world_data <- world_contries()
# Salvar como CSV
#write.csv(world_data, "world_data_complete.csv", row.names = FALSE)
