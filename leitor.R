ler_gastos <- function(arquivo = "ano-atual.csv"){
  #' Lê um csv criado a partir dos dados de gastos dos deputados da 
  #' Câmara e seta os tipos de colunas mais convenientemente. 
  require("readr")
  require("dplyr", warn.conflicts = FALSE)
  
  gastos = read_csv(arquivo, 
                    col_types = list(
                      datEmissao = col_datetime()
                    ))
  gastos = gastos %>% 
    mutate_each(funs(as.factor), sgPartido, sgUF, txNomeParlamentar, indTipoDocumento)
  return(gastos)
} 

ler_gastos2 <- function(arquivo = "ano-atual.csv"){
  #' Lê um csv criado a partir dos dados de gastos dos deputados da 
  #' Câmara e seta os tipos de colunas mais convenientemente. 
  #' Versão sem readr, para máquinas onde não é possível instalar esse pacote. 
  #' É um pouco mais lenta que a com readr.
  require("dplyr", warn.conflicts = FALSE)
  
  gastos = read.csv(arquivo, stringsAsFactors = FALSE)
  gastos = gastos %>% 
    mutate_each(funs(as.factor), sgPartido, sgUF, txNomeParlamentar, indTipoDocumento)
  return(gastos)
} 

decide_estado <- function(estado){
  require("dplyr", warn.conflicts = FALSE)
  if(estado %in% c("MA", "PI", "CE","RN", "SE", "BA", "PB", "PE", "AL")){
    result <- "Nordeste"
  }
  else if(estado %in% c("AM", "RR", "AP","PA", "TO", "RO", "AC")){
    result <- "Norte"
  }
  else if(estado %in% c("MT", "MS", "GO")){
    result <- "Centro-Oeste"
  }
  else if(estado %in% c("SP", "RJ", "ES","MG")){
    result <- "Sudeste"
  }
  else if(estado %in% c("PR", "RS", "SC")){
    result <- "Sul"
  }
  return(result)
  
}