if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(scales)){install.packages("scales"); library(scales)}


prepara_historico_tati <- function(estado, 
                              data_base = as.Date("2021-10-21"),
                              input_folder = "output/",
                              output_folder = "output/") {
  
  if(class(data_base) == "character") data_base <- as.Date(data_base)
  
  tabela_vacinas <- fread(paste0(input_folder,estado, "_PNI_clean.csv"), 
                          colClasses = c("id" = "factor",
                                         "data" = "Date", "vacina" = "integer","n" = "integer",
                                         "doses" = "factor","idade" = "integer"), encoding = "UTF-8")
  
  count_doses <- tabela_vacinas %>% group_by(id) %>% summarise(m = n())
  tabela_vacinas <- tabela_vacinas %>% left_join(count_doses, by = "id")
  
  tabela <- tabela_vacinas %>% select(-id) %>% 
    filter(data > "2021-01-01") %>%
    filter(data <= data_base) %>%
    drop_na(vacina, idade, data, doses) %>%
    mutate(vacina = factor(vacina, levels = c(85,86,87,88), labels = c("AZ","Coronavac","Pfizer","Janssen")),
           agegroup = cut(idade, 
                          breaks = c(0,20,40,60,75,Inf), 
                          include.lowest = T, 
                          right = F, 
                          labels = F)) %>%
    mutate(agegroup = factor(agegroup)) %>%
    count(vacina, agegroup, data,doses, .drop = FALSE)
  
  write.csv(tabela, file= paste0(output_folder,"doses_aplicadas_",estado,"_tati",".csv"), row.names = FALSE, quote = FALSE)
}

estados_br <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

for(estado in estados_br){
  print(paste0("calculando estado ",estado))
  prepara_historico_tati(estado)
  print("Done")
}