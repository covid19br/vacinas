suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(viridis)){install.packages("viridis"); library(viridis)}
  if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(scales)){install.packages("scales"); library(scales)}
  if(!require(optparse)){install.packages("scales"); library(optparse)}
})


end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}

#' prepare_table:
#' Lê os arquivos brutos de dados do PNI-SI, seleciona apenas as colunas relevantes
#' e realiza a limpeza e reorganização dos dados.
#' @estado: character. Sigla do estado do Brasil para leitura.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' @data_base: character. Data da base de dados, no formato %Y-%m-%d
#' @split: logical. Informa se os dados a serem lidos foram separados anteriormente ou não, e processa de acordo.

contar_doses_municipio <- function(estado,
                          input_folder = "dados/",
                          output_folder = "output/",
                          data_base = "2022-05-10",
                          split = FALSE) {
  if(split){
    pattern <- paste0("split_sorted_limpo_dados_",data_base,"_",estado)
    arquivos <- list.files(input_folder,pattern)
    indice = 0
    
    for(arquivo in arquivos){
      
      vacinas <- fread(paste0(input_folder,arquivo),
                       select = c("paciente_id", "paciente_dataNascimento", 
                                  "vacina_dataAplicacao",
                                  "paciente_enumSexoBiologico",
                                  "paciente_endereco_coIbgeMunicipio",
                                  #"paciente_endereco_uf",
                                  "estabelecimento_municipio_codigo"),
                       colClasses = c("paciente_id" = "factor",
                                      "paciente_dataNascimento" = "Date",
                                      "vacina_dataAplicacao" = "Date",
                                      "paciente_enumSexoBiologico" = "factor",
                                      #"paciente_endereco_uf" = "factor",
                                      "paciente_endereco_coIbgeMunicipio" = "factor",
                                      "estabelecimento_municipio_codigo" = "factor"), 
                       encoding = "UTF-8") %>%
        data.frame()
      
      indice = indice + 1
      
      print(paste0(estado, " data succesfully loaded. Preparing data: split #",indice))
      
      vacinas$paciente_id <- factor(vacinas$paciente_id)
      
      vacinas[vacinas == ""] <- NA
      
      # Clean Data
      vacinas <- vacinas %>% 
        distinct(.keep_all = TRUE) %>%
        filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao >= as.Date("2021-01-17")) %>%
        drop_na() %>%
        filter(paciente_dataNascimento > "1900-01-01") %>%
        filter(paciente_enumSexoBiologico %in% c("F","M")) %>%
        mutate(paciente_enumSexoBiologico = droplevels(paciente_enumSexoBiologico))
      
      # Filtrar municipios de residencia com poucos registros
      muni_excluir <- vacinas %>% count(paciente_endereco_coIbgeMunicipio) %>% filter(n <= 30) %>% select(-n)
      
      colnames(vacinas) <- c("id", "nasc", "date","sexo","muni_pac","muni_apli")
      
      # Filter ids with more than 3 doses and compute age at first dose  
      
      vacinas <- vacinas %>% 
        group_by(id) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        filter(n < 4) %>%
        select(-n) %>%
        
        group_by(id) %>% 
        mutate(nasc = min(nasc, na.rm = T)) %>%
        ungroup() %>%
        mutate(idade = as.numeric(date - nasc) %/% 365.25) %>% 
        select(-nasc) %>%
        mutate(agegroup = factor(cut(idade, 
                                     c(seq(0,90,10),Inf),
                                     include.lowest = T, 
                                     right = F,
                                     labels = F))) %>%
        select(-idade) %>%
        mutate(SE = as.integer(end.of.epiweek(date) - as.Date("2021-01-02")) / 7) %>%
        arrange(id, date) %>% 
        group_by(id) %>% 
        mutate(doses = 1:n()) %>%
        ungroup()
      
      filename = paste0(output_folder,"municipios/muni_paciente_wide_",estado,"_",indice,".csv")
      print(paste0("Salvando: ",filename))
      fwrite(vac_wide, file = filename)
      
      rm(vac_wide);gc()
      
      vac_muni_pac <- vacinas %>%
        filter(muni_pac != c("None")) %>%
        filter(muni_pac != c("999999")) %>%
        drop_na(muni_pac) %>%
        anti_join(muni_excluir, by = c("muni_pac" = "paciente_endereco_coIbgeMunicipio")) %>%
        select(muni_pac, doses, agegroup, sexo, SE) %>%
        drop_na() %>%
        mutate(muni_pac = factor(muni_pac, levels = unique(muni_pac)),
               agegroup = factor(agegroup, 
                                 levels = levels(agegroup), 
                                 labels = paste0("I",levels(agegroup))),
               doses = factor(doses, levels = c(1:3), labels = paste0("D",1:3)),
               SE = factor(SE, levels = seq(1,max(SE)))) %>%
        count(muni_pac, doses, agegroup, sexo, SE) %>%
        complete(muni_pac, doses, agegroup, sexo, SE, fill = list(n = 0)) %>%
        arrange(muni_pac, doses, agegroup, sexo, SE) %>%
        group_by(muni_pac, doses, agegroup, sexo) %>%
        mutate(m = cumsum(n)) %>%
        ungroup() %>%
        select(-n) %>%
        mutate(SE = as.numeric(SE)) %>%
        pivot_wider(id_cols = muni_pac,
                    names_from = c(doses,agegroup,sexo,SE),
                    values_from = m,
                    values_fn = first,
                    values_fill = 0)
      
      vac_muni_apli <- vacinas %>%
        select(muni_apli, doses, agegroup, sexo, SE) %>%
        drop_na() %>%
        mutate(muni_apli = factor(muni_apli, levels = unique(muni_apli)),
               agegroup = factor(agegroup, 
                                 levels = levels(agegroup), 
                                 labels = paste0("I",levels(agegroup))),
               doses = factor(doses, levels = c(1:3), labels = paste0("D",1:3)),
               SE = factor(SE, levels = seq(1,max(SE)))) %>%
        count(muni_apli, doses, agegroup, sexo, SE) %>%
        complete(muni_apli, doses, agegroup, sexo, SE, fill = list(n = 0)) %>%
        arrange(muni_apli, doses, agegroup, sexo, SE) %>%
        group_by(muni_apli, doses, agegroup, sexo) %>%
        mutate(m = cumsum(n)) %>%
        ungroup() %>%
        select(-n) %>%
        mutate(SE = as.numeric(SE)) %>%
        pivot_wider(id_cols = muni_apli,
                    names_from = c(doses,agegroup,sexo,SE),
                    values_from = m,
                    values_fn = first,
                    values_fill = 0)
      
      filename = paste0(output_folder,"municipios/muni_aplicacao_wide_",estado,"_",indice,".csv")
      print(paste0("Salvando: ",filename))
      fwrite(vac_wide, file = filename)
      
      rm(vacinas);gc()
      
    } # for
  } else {
    
    # Non split
    
    vacinas <- fread(paste0(input_folder,"limpo_dados_",data_base,"_",estado,".csv"),
                     select = c("paciente_id", "paciente_dataNascimento", 
                                      "vacina_dataAplicacao",
                                       "paciente_enumSexoBiologico",
                                      "paciente_endereco_coIbgeMunicipio",
                                     #"paciente_endereco_uf",
                                      "estabelecimento_municipio_codigo"),
                           colClasses = c("paciente_id" = "factor",
                                          "paciente_dataNascimento" = "Date",
                                          "vacina_dataAplicacao" = "Date",
                                          "paciente_enumSexoBiologico" = "factor",
                                         #"paciente_endereco_uf" = "factor",
                                          "paciente_endereco_coIbgeMunicipio" = "factor",
                                          "estabelecimento_municipio_codigo" = "factor"), 
                           encoding = "UTF-8") %>%
      data.frame()
    
    print(paste0(estado, " data succesfully loaded. Preparing data... 1"))
    
    vacinas$paciente_id <- factor(vacinas$paciente_id)
    
    vacinas[vacinas == ""] <- NA

    # Clean Data
    vacinas <- vacinas %>% 
      distinct(.keep_all = TRUE) %>%
      filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao >= as.Date("2021-01-17")) %>%
      drop_na() %>%
      filter(paciente_dataNascimento > "1900-01-01") %>%
      filter(paciente_enumSexoBiologico %in% c("F","M")) %>%
      mutate(paciente_enumSexoBiologico = droplevels(paciente_enumSexoBiologico))
  
    # Filtrar municipios de residencia com poucos registros
    muni_excluir <- vacinas %>% count(paciente_endereco_coIbgeMunicipio) %>% filter(n <= 30) %>% select(-n)
    
    colnames(vacinas) <- c("id", "nasc", "date","sexo","muni_pac","muni_apli")
  
    # Filter ids with more than 3 doses and compute age at first dose  
    
    vacinas <- vacinas %>% 
      group_by(id) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      filter(n < 4) %>%
      select(-n) %>%
      
      group_by(id) %>% 
      mutate(nasc = min(nasc, na.rm = T)) %>%
      ungroup() %>%
      mutate(idade = as.numeric(date - nasc) %/% 365.25) %>% 
      select(-nasc) %>%
      mutate(agegroup = factor(cut(idade, 
                                   c(seq(0,90,10),Inf),
                                   include.lowest = T, 
                                   right = F,
                                   labels = F))) %>%
      select(-idade) %>%
      mutate(SE = as.integer(end.of.epiweek(date) - as.Date("2021-01-02")) / 7) %>%
      arrange(id, date) %>% 
      group_by(id) %>% 
      mutate(doses = 1:n()) %>%
      ungroup()
    
    filename = paste0(output_folder,"municipios/muni_paciente_wide_",estado,".csv")
    print(paste0("Salvando: ",filename))
    fwrite(vac_wide, file = filename)
    
    rm(vac_wide);gc()
    
    vac_muni_pac <- vacinas %>%
      filter(muni_pac != c("None")) %>%
      filter(muni_pac != c("999999")) %>%
      drop_na(muni_pac) %>%
      anti_join(muni_excluir, by = c("muni_pac" = "paciente_endereco_coIbgeMunicipio")) %>%
      select(muni_pac, doses, agegroup, sexo, SE) %>%
      drop_na() %>%
      mutate(muni_pac = factor(muni_pac, levels = unique(muni_pac)),
             agegroup = factor(agegroup, 
                               levels = levels(agegroup), 
                               labels = paste0("I",levels(agegroup))),
             doses = factor(doses, levels = c(1:3), labels = paste0("D",1:3)),
             SE = factor(SE, levels = seq(1,max(SE)))) %>%
    count(muni_pac, doses, agegroup, sexo, SE) %>%
    complete(muni_pac, doses, agegroup, sexo, SE, fill = list(n = 0)) %>%
    arrange(muni_pac, doses, agegroup, sexo, SE) %>%
    group_by(muni_pac, doses, agegroup, sexo) %>%
    mutate(m = cumsum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    mutate(SE = as.numeric(SE)) %>%
                  pivot_wider(id_cols = muni_pac,
                  names_from = c(doses,agegroup,sexo,SE),
                  values_from = m,
                  values_fn = first,
                  values_fill = 0)
    
    vac_muni_apli <- vacinas %>%
      select(muni_apli, doses, agegroup, sexo, SE) %>%
      drop_na() %>%
      mutate(muni_apli = factor(muni_apli, levels = unique(muni_apli)),
             agegroup = factor(agegroup, 
                               levels = levels(agegroup), 
                               labels = paste0("I",levels(agegroup))),
             doses = factor(doses, levels = c(1:3), labels = paste0("D",1:3)),
             SE = factor(SE, levels = seq(1,max(SE)))) %>%
      count(muni_apli, doses, agegroup, sexo, SE) %>%
      complete(muni_apli, doses, agegroup, sexo, SE, fill = list(n = 0)) %>%
      arrange(muni_apli, doses, agegroup, sexo, SE) %>%
      group_by(muni_apli, doses, agegroup, sexo) %>%
      mutate(m = cumsum(n)) %>%
      ungroup() %>%
      select(-n) %>%
      mutate(SE = as.numeric(SE)) %>%
      pivot_wider(id_cols = muni_apli,
                  names_from = c(doses,agegroup,sexo,SE),
                  values_from = m,
                  values_fn = first,
                  values_fill = 0)
    
    filename = paste0(output_folder,"municipios/muni_aplicacao_wide_",estado,".csv")
    print(paste0("Salvando: ",filename))
    fwrite(vac_wide, file = filename)
    
    rm(vacinas);gc()
  }
  
}

### SCRIPT

data_base <- list.files("dados/") %>% 
  grep("^dados_.*.csv", ., value = T) %>%
  substr(7,16) %>%
  as.Date() %>%
  max(na.rm = T)

# Processar municípios de estados sem split

files <- list.files("^limpo_dados")[!grepl("[1-9].csv", list.files("limpo_dados"))]

for(j in files) {
  contar_doses_municipio(estado = j, data_base = data_base, split = FALSE)
 }

# Processar municípios de estados com split

files <- list.files("^split_sorted_limpo_dados_")

for(j in files) {
  contar_doses_municipio(estado = j, data_base = data_base, split = TRUE)
}

print("Processamento de municípios: finalizado.")  

### Proximos passos:
# Implementar juntar tudo em uma unica tabela, somar municipios de estados diferentes



