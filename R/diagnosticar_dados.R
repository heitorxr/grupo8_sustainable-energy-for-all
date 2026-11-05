# Diagnóstico somente de leitura. Não limpa, imputa, filtra ou salva dados tratados.
# Executar na raiz: Rscript R/diagnosticar_dados.R
# Uso de IA: apoio à programação; resultados calculados sobre o CSV original.
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
raiz <- if (length(script)) dirname(dirname(normalizePath(script))) else getwd()
entrada <- file.path(raiz, "dados", "brutos", "energy_cleaned.csv")
saida <- file.path(raiz, "resultados", "diagnostico")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
hash_antes <- unname(tools::md5sum(entrada))
# Tudo é lido como texto para identificar falhas de tipo sem alterar o arquivo.
dados <- read.csv(entrada, colClasses = "character", na.strings = character(),
                  check.names = FALSE, fileEncoding = "UTF-8", strip.white = FALSE)
dicionario <- read.csv(file.path(raiz, "dados", "metadados", "dicionario_fonte.csv"),
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8")
stopifnot(nrow(dados) > 0, identical(names(dados), dicionario$variavel))
ausente <- function(x) is.na(x) | trimws(x) %in% c("", "NA")
nomes_numericos <- dicionario$variavel[dicionario$tipo_fonte %in% c("integer", "double")]
numero <- function(x) suppressWarnings(as.numeric(x))
perfil <- do.call(rbind, lapply(names(dados), function(nome) {
  x <- dados[[nome]]; falta <- ausente(x)
  numerica <- nome %in% nomes_numericos
  z <- if (numerica) numero(x) else rep(NA_real_, length(x))
  valido <- !falta & is.finite(z)
  percentual <- grepl("_pct$|^perc_", nome)
  data.frame(variavel = nome, tipo_fonte = dicionario$tipo_fonte[match(nome, dicionario$variavel)],
    linhas = length(x), ausentes = sum(falta), ausentes_pct = 100 * mean(falta),
    distintos_presentes = length(unique(x[!falta])),
    espacos_nas_pontas = sum(!falta & x != trimws(x)),
    falhas_tipo_numerico = if (numerica) sum(!falta & !is.finite(z)) else NA_integer_,
    minimo = if (any(valido)) min(z[valido]) else NA_real_,
    maximo = if (any(valido)) max(z[valido]) else NA_real_,
    zeros = if (numerica) sum(valido & z == 0) else NA_integer_,
    negativos = if (numerica) sum(valido & z < 0) else NA_integer_,
    percentuais_fora_0_100 = if (percentual) sum(valido & (z < 0 | z > 100)) else NA_integer_)
}))
medidas <- as.matrix(dados[nomes_numericos[nomes_numericos != "yr"]])
faltas <- matrix(ausente(as.vector(medidas)), nrow = nrow(medidas))
locais <- unique(dados[c("country_code", "country_name")])
chave <- paste(dados$country_code, dados$yr, sep = "|")
frequencias <- table(chave)
chaves_repetidas <- data.frame(chave = names(frequencias)[frequencias > 1],
                               ocorrencias = as.integer(frequencias[frequencias > 1]))
resumir_grupo <- function(indices) {
  anos <- numero(dados$yr[indices]); anos <- anos[is.finite(anos)]
  data.frame(linhas = length(indices), anos_distintos = length(unique(anos)),
    ano_inicial = if (length(anos)) min(anos) else NA,
    ano_final = if (length(anos)) max(anos) else NA,
    medidas_ausentes_pct = 100 * mean(faltas[indices, , drop = FALSE]),
    linhas_sem_medidas = sum(rowSums(!faltas[indices, , drop = FALSE]) == 0))
}
por_local <- do.call(rbind, lapply(seq_len(nrow(locais)), function(i) {
  idx <- which(dados$country_code == locais$country_code[i] & dados$country_name == locais$country_name[i])
  cbind(locais[i, , drop = FALSE], resumir_grupo(idx))
}))
por_ano <- do.call(rbind, lapply(sort(unique(dados$yr)), function(ano) {
  idx <- which(dados$yr == ano)
  data.frame(yr = ano, localidades = length(unique(dados$country_code[idx])), resumir_grupo(idx))
}))
exportar <- function(x, nome) write.csv(x, file.path(saida, nome), row.names = FALSE,
                                      na = "", fileEncoding = "UTF-8")
exportar(perfil, "perfil_variaveis.csv")
exportar(por_local, "cobertura_por_localidade.csv")
exportar(por_ano, "cobertura_por_ano.csv")
exportar(chaves_repetidas, "chaves_repetidas.csv")
anos <- numero(dados$yr)
prioridade <- perfil[order(-perfil$ausentes_pct), ]
linhas <- c("DIAGNÓSTICO INICIAL — SEM TRATAMENTO",
  sprintf("Base: %d linhas, %d variáveis, %d pares código/nome, anos %d a %d.",
          nrow(dados), ncol(dados), nrow(locais), min(anos, na.rm = TRUE), max(anos, na.rm = TRUE)),
  sprintf("Duplicatas em todas as colunas lidas: %d. Chaves código/ano repetidas: %d.",
          sum(duplicated(dados)), nrow(chaves_repetidas)),
  sprintf("Campos numéricos com falha de conversão: %d. Células inválidas: %d.",
          sum(perfil$falhas_tipo_numerico > 0, na.rm = TRUE), sum(perfil$falhas_tipo_numerico, na.rm = TRUE)),
  sprintf("Ausência nos indicadores (sem identificadores e ano): %.2f%%.", 100 * mean(faltas)),
  sprintf("Colunas totalmente ausentes: %d. Linhas sem nenhum indicador: %d.",
          sum(perfil$ausentes == nrow(dados)), sum(rowSums(!faltas) == 0)),
  sprintf("Células percentuais fora de [0,100]: %d. Células numéricas negativas: %d.",
          sum(perfil$percentuais_fora_0_100, na.rm = TRUE), sum(perfil$negativos, na.rm = TRUE)),
  sprintf("Espaços nas pontas: %d. Identificadores/ano ausentes: %d.",
          sum(perfil$espacos_nas_pontas), sum(vapply(dados[c("country_name", "country_code", "yr")], function(x) sum(ausente(x)), integer(1)))),
  "", "Variáveis com maior ausência:",
  sprintf("- %s: %.2f%%", head(prioridade$variavel, 6), head(prioridade$ausentes_pct, 6)),
  "", "DECISÕES NECESSÁRIAS (NÃO EXECUTADAS):",
  "- Escolher indicadores e recorte temporal conforme cobertura; não remover toda linha com qualquer NA.",
  "- Conferir códigos e nomes antes de definir quais localidades comparar.",
  "- Definir tipos numéricos e fatores na futura etapa de preparação.",
  "- Investigar percentuais fora da faixa e negativos por indicador; sinalizar não significa corrigir ou excluir.",
  "- Distinguir zero observado de ausência e preservar unidades diferentes.",
  "- Avaliar variáveis sem informação ou constantes antes de usá-las nas análises.",
  "", "O arquivo já se chama energy_cleaned.csv na fonte TidyTuesday; é bruto para este projeto, não a origem SE4ALL anterior à curadoria.",
  "A classificação de ausências e as conversões numéricas são apenas diagnósticas, em memória.",
  "Nenhuma imputação, deduplicação, interpolação, exclusão ou transformação foi aplicada.",
  "Uso de IA: apoio à programação e à leitura do diagnóstico; contagens produzidas pelo código R.")
stopifnot(identical(hash_antes, unname(tools::md5sum(entrada))))
writeLines(enc2utf8(linhas), file.path(saida, "diagnostico.txt"), useBytes = TRUE)
cat(paste(linhas[1:9], collapse = "\n"), "\n")
cat("OK: arquivo original inalterado; tabelas em resultados/diagnostico.\n")
