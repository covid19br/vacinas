if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(scales)){install.packages("scales"); library(scales)}

#' prepare_table:
#' Lê os arquivos brutos de dados do PNI-SI, seleciona apenas as colunas relevantes
#' e realiza a limpeza e reorganização dos dados.
#' @estado: character. Sigla do estado do Brasil para leitura.
#' @write.dose.types: logical. Se a função deve salvar uma tabela em .csv com as categorias
#' presentes na coluna "vacina_descricao_dose"
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' @data_base: character. Data da base de dados, no formato %Y-%m-%d

read.doses <- function(estado, write.dose.types = TRUE, 
                          input_folder = "dados/",
                          output_folder = "output/",
                          data_base = "2021-09-03") {
  
  todas_vacinas <- fread(paste0(input_folder,"dados_",data_base,"_",estado,".csv"), 
                         select = c("vacina_descricao_dose", "vacina_codigo"), 
                         colClasses = c("vacina_descricao_dose" = "character",
                                        "vacina_codigo" = "integer"), 
                         encoding = "UTF-8")
  
  print("Data succesfully loaded")
  print("Preparing data... 1")
  
  
  #write.csv(data.frame(table(todas_vacinas$vacina_descricao_dose)), 
   #           file = paste0(output_folder,estado,"_dose_types_log.csv"), quote = FALSE, row.names = FALSE)
    #print("csv ok")
  #res = data.frame(table(todas_vacinas$vacina_descricao_dose))
  #save(res, file = "dosetypes.Rdata")
  load("nomes_doses.Rdata")
  
  x = (todas_vacinas %>% filter(vacina_descricao_dose %in% nomes_doses[3:4]) %>% table())
  print(x)
}

load("nomes_doses.Rdata")
print(nomes_doses)

read.doses("SP")


