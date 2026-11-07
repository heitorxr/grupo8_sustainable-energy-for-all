library(tidyverse)

agrupar_regioes <- function(dados) {
  # Antes de agrupar, conferimos se a tabela possui os identificadores necessários.
  ids <- c("country_name", "country_code", "yr")
  if (!is.data.frame(dados) || !all(ids %in% names(dados))) {
    stop("Forneca uma tabela com country_name, country_code e yr.")
  }

  # Cada lista reúne os códigos originais de uma região, sem renomear os países.

  # ANT, CHI, KSV e TWN têm correspondências geográficas complementares explícitas.
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

  # Estes códigos já representam grupos, não países individuais.
  # Eles precisam ficar separados para não serem contados novamente.
  renda <- c("HIC", "LIC", "LMC", "LMY", "MIC", "NOC", "OEC", "UMC")
  geograficos <- c(
    "GCA", "GCN", "GEA", "GEE", "GEJ", "GEU", "GLC", "GNA",
    "GNF", "GOC", "GSA", "GSE", "GSS", "GWA", "GWL"
  )
  codigos_agregados <- c(renda, geograficos)

  # enframe transforma a lista numa tabela; unnest_longer cria uma linha por código.
  mapa <- enframe(codigos, name = "regiao", value = "country_code") %>%
    unnest_longer(country_code)

  # Um mesmo código não pode pertencer a duas regiões.
  duplicados <- mapa %>%
    count(country_code) %>%
    filter(n > 1)

  if (nrow(duplicados) > 0) {
    stop("Há códigos atribuídos a mais de uma região.")
  }

  # filter mantém os países e territórios; os valores dos indicadores ficam iguais.
  paises <- dados %>%
    filter(!country_code %in% codigos_agregados)

  # anti_join encontra códigos da base que ainda não aparecem no mapa de regiões.
  faltantes <- paises %>%
    distinct(country_code) %>%
    anti_join(mapa, by = "country_code")

  if (nrow(faltantes) > 0) {
    stop("Códigos ainda sem classificação: ", paste(faltantes$country_code, collapse = ", "))
  }

  # left_join acrescenta a região usando o código como chave, sem resumir os dados.
  paises <- paises %>%
    left_join(mapa, by = "country_code")

  # Preservamos também os grupos de renda e as regiões que já vieram na fonte.
  agregados_originais <- dados %>%
    filter(country_code %in% codigos_agregados)

  # distinct evita contar cada localidade novamente em cada ano da série.
  # count informa quantas localidades foram atribuídas a cada uma das cinco regiões.
  resumo <- paises %>%
    distinct(country_code, regiao) %>%
    count(regiao, name = "localidades") %>%
    arrange(factor(regiao, levels = names(codigos)))

  # Devolvemos as três tabelas em memória, sem salvar CSVs ou calcular médias regionais.
  # Territórios históricos ainda precisam de conferência antes de qualquer soma.
  list(
    paises = paises,
    agregados_originais = agregados_originais,
    resumo = resumo
  )
}
