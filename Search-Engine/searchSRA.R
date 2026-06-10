library(rentrez)
library(XML)

# Output directory (defaults to ./results). Override with the SEARCH_ENGINE_DIR env var.
dir <- Sys.getenv("SEARCH_ENGINE_DIR", unset = "results")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

# NCBI API key (optional): set the ENTREZ_API_KEY environment variable to raise the
# rate limit from 3 to 10 requests/second. NEVER hardcode the key in this file.
entrez_api_key <- Sys.getenv("ENTREZ_API_KEY")
if (nzchar(entrez_api_key)) {
  set_entrez_key(entrez_api_key)
}

# Fazendo uma busca no ncbi e armazenando os id's (rápido)
res <- entrez_search(
  db = "sra",
  term = "(\"homo sapiens\"[Organism]) AND (Brazil*) AND ((mutat*) OR (variant*)) AND \"sra public\"[Filter]",
  #term = "03/29/2024 [PDAT]", # O que restamos buscando é definido nesse ponto
  use_history = TRUE,
  retmax = 77777
)

# Registrando o xml de cada id (demorado)
recs <- entrez_fetch(
  db = "sra",
  #id = res$ids,
  web_history = res$web_history,
  rettype = "xml",
  parsed = TRUE
  )

# Salvando o resultado da busca em um arquivo XML
saveXML(recs, file.path(dir, "resultXML.xml"))
# Abrindo o resultado da busca
resultXML <- xmlParse(file.path(dir, "resultXML.xml"))

# Criando um dataframe vazio
df <- data.frame(
  Id = character(),
  City = character(),
  ChavePrimaria = character(),
  DataColeta = character(),
  Instituto = character(),
  Instrumento = character(),
  Estrategia = character(),
  Fonte = character(),
  Selecao = character(),
  stringsAsFactors = FALSE
)

# Separando o resultado de cada busca a partir da tag EXPERIMENT_PACKAGE
experiment_packages <- getNodeSet(resultXML, "//EXPERIMENT_PACKAGE")

# Itere sobre cada EXPERIMENT_PACKAGE
for (i in seq_along(experiment_packages))
{
  # Selecionando um "nó"
  exp_package <- experiment_packages[[i]]

  # # Realizando uma busca no xml
  cidade <- xpathSApply(exp_package, ".//Organization/Contact/Address/City", xmlValue)

  # Verificando se a busca anterior obteve resultando
  if (cidade[1] == 'NULL')
  {
    # Buscando a partir de outra tag
    cidade <- xpathSApply(exp_package, ".//Organization/Address/City", xmlValue)

    if (cidade[1] == 'NULL')
    {
      # Buscando a tag VALUE irmã da tag TAG que possui 'geo_loc_name' como conteúdo
      cidade <- xpathApply(exp_package, ".//TAG[text()='geo_loc_name']/following-sibling::VALUE[1]", xmlValue)
      }
  }
  
  data_coleta <- list()
  
  # Algumas outras informações mais fáceis de obter (algumas estão bugando devido terem mais de um resultado)
  primary_id <- xpathApply(exp_package, ".//RUN/IDENTIFIERS/PRIMARY_ID", xmlValue)
  data_coleta <- xpathApply(exp_package, ".//TAG[text()='collection_date']/following-sibling::VALUE[1]", xmlValue)[1]
  instituto <- xpathApply(exp_package, ".//Institution", xmlValue)[1]
  instrumento <- xpathApply(exp_package, ".//INSTRUMENT_MODEL", xmlValue)
  estrategia <- xpathApply(exp_package, ".//LIBRARY_STRATEGY", xmlValue)
  fonte <- xpathApply(exp_package, ".//LIBRARY_SOURCE", xmlValue)
  selecao <- xpathApply(exp_package, ".//LIBRARY_SELECTION", xmlValue)
  
  if(is.null(data_coleta[1])){
    data_coleta <- "NULL"
  }
  
  if(is.null(cidade)){
    cidade <- "NULL"
  }
  
  if(is.null(instituto[[1]])){
    instituto <- "NULL"
  }

  # Juntando todos dados obtidos e registrando no dataframe
  registro <- c(res$ids[i], cidade, primary_id, data_coleta, instituto, instrumento, estrategia, fonte, selecao)
  df <- rbind(df, registro)
}

# Salvando o dataframe como um arquivo CSV
write.csv(df, file.path(dir, "resultTable.csv"), row.names = TRUE)

# Contando quantos elementos na segunda coluna possuem o conteúdo "NULL"
contagem <- sum(df[,2] == "NULL")

padrao <- "2024-03"
indices <- grep(padrao, df[,4])
qt <- length(indices)

df_Result <- data.frame()
df_Result <- rbind(df[indices,])
write.csv(df_Result, file.path(dir, "candidatos.csv"), row.names = FALSE)