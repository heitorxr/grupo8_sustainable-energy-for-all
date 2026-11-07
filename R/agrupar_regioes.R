library(tidyverse)

agrupar_regioes <- function(dados) {
  # Recebe a base bruta já importada, ainda sem a coluna regiao.
  ids <- c("country_name", "country_code", "yr")
  if (!is.data.frame(dados) || !all(ids %in% names(dados))) {
    stop("Forneca uma tabela com country_name, country_code e yr.")
  }

  # Códigos dos países e territórios em cada região.
  # Inclui as correspondências específicas de ANT, CHI, KSV e TWN.
  # Antes de somar, conferir territórios históricos e seus sucessores.
  codigos <- list(
    "África" = c(
      "AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COG", "COM",
      "CPV", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH", "GAB", "GHA", "GIN",
      "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", "MAR", "MDG", "MLI",
      "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", "REU", "RWA",
      "SDN", "SEN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO",
      "TUN", "TZA", "UGA", "ZAF", "ZAR", "ZMB", "ZWE"
    ),
    "Américas" = c(
      "ABW", "ANT", "ARG", "ATG", "BHS", "BLZ", "BMU", "BOL", "BRA", "BRB",
      "CAN", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU",
      "FLK", "GLP", "GRD", "GRL", "GTM", "GUF", "GUY", "HND", "HTI", "JAM",
      "KNA", "LCA", "MAF", "MEX", "MSR", "MTQ", "NIC", "PAN", "PER", "PRI",
      "PRY", "SLV", "SPM", "SUR", "SXM", "TCA", "TTO", "URY", "USA", "VCT",
      "VEN", "VGB", "VIR"
    ),
    "Ásia" = c(
      "AFG", "ARE", "ARM", "AZE", "BGD", "BHR", "BRN", "BTN", "CHN", "CYP",
      "GEO", "HKG", "IDN", "IND", "IRN", "IRQ", "ISR", "JOR", "JPN", "KAZ",
      "KGZ", "KHM", "KOR", "KWT", "LAO", "LBN", "LKA", "MAC", "MDV", "MMR",
      "MNG", "MYS", "NPL", "OMN", "PAK", "PHL", "PRK", "QAT", "SAU", "SGP",
      "SYR", "THA", "TJK", "TKM", "TMP", "TUR", "TWN", "UZB", "VNM", "WBG",
      "YEM"
    ),
    "Europa" = c(
      "ADO", "ALB", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CHI", "CZE",
      "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "FRO", "GBR", "GIB", "GRC",
      "HRV", "HUN", "IMY", "IRL", "ISL", "ITA", "KSV", "LIE", "LTU", "LUX",
      "LVA", "MCO", "MDA", "MKD", "MLT", "MNE", "NLD", "NOR", "POL", "PRT",
      "ROM", "RUS", "SMR", "SRB", "SVK", "SVN", "SWE", "UKR"
    ),
    "Oceania" = c(
      "ASM", "AUS", "COK", "FJI", "FSM", "GUM", "KIR", "MHL", "MNP", "NCL",
      "NZL", "PLW", "PNG", "PYF", "SLB", "TON", "TUV", "VUT", "WSM"
    )
  )

  # Estes agregados já representam grupos e não entram novamente como países.
  renda <- c("HIC", "LIC", "LMC", "LMY", "MIC", "NOC", "OEC", "UMC")
  geograficos <- c(
    "GCA", "GCN", "GEA", "GEE", "GEJ", "GEU", "GLC", "GNA",
    "GNF", "GOC", "GSA", "GSE", "GSS", "GWA", "GWL"
  )
  codigos_agregados <- c(renda, geograficos)

  # Uma linha por código, com a região correspondente.
  mapa <- enframe(codigos, name = "regiao", value = "country_code") %>%
    unnest_longer(country_code)

  # Cada código deve ter uma única região.
  if (anyDuplicated(mapa$country_code) > 0) {
    stop("Há códigos atribuídos a mais de uma região.")
  }

  # Mantém apenas países e territórios.
  paises <- dados %>%
    filter(!country_code %in% codigos_agregados)

  # Procura códigos sem região definida.
  faltantes <- paises %>%
    distinct(country_code) %>%
    anti_join(mapa, by = "country_code")

  if (nrow(faltantes) > 0) {
    stop("Códigos ainda sem classificação: ", paste(faltantes$country_code, collapse = ", "))
  }

  # Adiciona a região pelo código da localidade.
  paises <- paises %>%
    left_join(mapa, by = "country_code")

  # Guarda os agregados originais em outra tabela.
  agregados_originais <- dados %>%
    filter(country_code %in% codigos_agregados)

  # Conta cada localidade uma vez, independentemente do ano.
  resumo <- paises %>%
    distinct(country_code, regiao) %>%
    count(regiao, name = "localidades") %>%
    arrange(factor(regiao, levels = names(codigos)))

  list(
    paises = paises,
    agregados_originais = agregados_originais,
    resumo = resumo
  )
}
