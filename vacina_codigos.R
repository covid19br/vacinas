if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(scales)){install.packages("scales"); library(scales)}

read.vac.code <- function(input_folder, data_base, estado){ 
  
todas_vacinas <- fread(paste0(input_folder,"dados_",data_base,"_",estado,".csv"), 
                       select = c("vacina_codigo","vacina_nome"),
                       colClasses = c("vacina_nome" = "factor",
                                      "vacina_codigo" = "integer"), encoding = "UTF-8")

mat <- todas_vacinas %>% select(vacina_nome, vacina_codigo) %>% table()
write.csv(mat, file = paste0(estado,"_code_table.csv"), quote = FALSE)

}

estados_br <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

ini = Sys.time()

for (estado_n in estados_br){
  print(estado_n)
  read.vac.code(estado = estado_n, data_base = "2021-08-06", input_folder = "dados/")
}

fin = Sys.time()

print(fin - ini)
