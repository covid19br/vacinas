suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(viridis)){install.packages("viridis"); library(viridis)}
  if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(scales)){install.packages("scales"); library(scales)}
  if(!require(optparse)){install.packages("scales"); library(optparse)}
  if(!require(stringr)){install.packages("stringr"); library(stringr)}
  if(!require(crunch)){install.packages("crunch"); library(crunch)}
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
  
  data_base_title <- format(as.Date(data_base), format = "%Y_%m_%d")
  
  if(split){
    pattern <- paste0("split_sorted_limpo_dados_",data_base,"_",estado)
    arquivos <- grep(pattern, list.files(input_folder), value = T)
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
      
      df.log <- data.frame(paciente_id = sum(is.na(vacinas$paciente_id)),
                 paciente_dataNascimento = sum(is.na(vacinas$paciente_dataNascimento)),
                 vacina_dataAplicacao = sum(is.na(vacinas$vacina_dataAplicacao)),
                 paciente_enumSexoBiologico = sum(is.na(vacinas$paciente_enumSexoBiologico)),
                 paciente_enumSexoBiologico_empty = sum(vacinas$paciente_enumSexoBiologico == ""),
                 paciente_enumSexoBiologico_n = length(unique(vacinas$paciente_enumSexoBiologico)),
                 paciente_endereco_uf = sum(is.na(vacinas$paciente_endereco_uf)),
                 paciente_endereco_coIbgeMunicipio = sum(is.na(vacinas$paciente_endereco_coIbgeMunicipio)),
                 estabelecimento_municipio_codigo = sum(is.na(vacinas$estabelecimento_municipio_codigo)),
                 file = arquivo,
                 indice = indice)
      
      filename <- paste0("log_municipios_", data_base_title,".csv")
      
      if(any(grepl(filename, list.files(paste0(output_folder,"log/"))))) {
        
        # Acrescenta o log para o arquivo anterior
        
        log_table_todos <- read.csv(paste0(output_folder,"log/",filename), row.names = 1)
        log_table_todos <- bind_rows(log_table_todos, df.log)
        write.csv(log_table_todos, file = paste0(output_folder, "log/", filename))
        
      } else {
        
        # Cria um arquivo novo
        write.csv(df.log, file = paste0(output_folder, "log/", filename))
        
      }
      
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
     # muni_excluir <- vacinas %>% count(paciente_endereco_coIbgeMunicipio) %>% filter(n <= 10) %>% select(-n)
      
      colnames(vacinas) <- c("id", "nasc", "date","sexo","muni_pac","muni_apli")
      
      # Filter doses with more than 3 doses and compute age at first dose  
      
      vacinas <- vacinas %>% 
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
        ungroup() %>%
        filter(n < 4)
      
      vac_muni_pac <- vacinas %>%
        filter(muni_pac != c("None")) %>%
        filter(muni_pac != c("999999")) %>%
        drop_na(muni_pac) %>%
        #anti_join(muni_excluir, by = c("muni_pac" = "paciente_endereco_coIbgeMunicipio")) %>%
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
      
      filename = paste0(output_folder,"municipios/muni_paciente_wide_",estado,"_",indice,".csv")
      print(paste0("Salvando: ",filename))
      fwrite(vac_muni_pac, file = filename)
      
      rm(vac_muni_pac);gc()
      
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
      fwrite(vac_muni_apli, file = filename)
      
      rm(vacinas, vac_muni_apli);gc()
      
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
    
    df.log <- data.frame(paciente_id = sum(is.na(vacinas$paciente_id)),
                         paciente_dataNascimento = sum(is.na(vacinas$paciente_dataNascimento)),
                         vacina_dataAplicacao = sum(is.na(vacinas$vacina_dataAplicacao)),
                         paciente_enumSexoBiologico = sum(is.na(vacinas$paciente_enumSexoBiologico)),
                         paciente_enumSexoBiologico_empty = sum(vacinas$paciente_enumSexoBiologico == ""),
                         paciente_enumSexoBiologico_n = length(unique(vacinas$paciente_enumSexoBiologico)),
                         paciente_endereco_uf = sum(is.na(vacinas$paciente_endereco_uf)),
                         paciente_endereco_coIbgeMunicipio = sum(is.na(vacinas$paciente_endereco_coIbgeMunicipio)),
                         estabelecimento_municipio_codigo = sum(is.na(vacinas$estabelecimento_municipio_codigo)),
                         file = arquivo,
                         indice = 1)
    
    filename <- paste0("log_municipios_", data_base_title,".csv")
    
    if(any(grepl(filename, list.files(paste0(output_folder,"log/"))))) {
      
      # Acrescenta o log para o arquivo anterior
      
      log_table_todos <- read.csv(paste0(output_folder,"log/",filename), row.names = 1)
      log_table_todos <- bind_rows(log_table_todos, df.log)
      write.csv(log_table_todos, file = paste0(output_folder, "log/", filename))
      
    } else {
      
      # Cria um arquivo novo
      write.csv(df.log, file = paste0(output_folder, "log/", filename))
      
    }
    
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
   # muni_excluir <- vacinas %>% count(paciente_endereco_coIbgeMunicipio) %>% filter(n <= 10) %>% select(-n)
    
    colnames(vacinas) <- c("id", "nasc", "date","sexo","muni_pac","muni_apli")
  
    # Filter ids with more than 3 doses and compute age at first dose  
    
    vacinas <- vacinas %>% 

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
      ungroup() %>%
      filter(n < 4)
    
    vac_muni_pac <- vacinas %>%
      filter(muni_pac != c("None")) %>%
      filter(muni_pac != c("999999")) %>%
      drop_na(muni_pac) %>%
      #anti_join(muni_excluir, by = c("muni_pac" = "paciente_endereco_coIbgeMunicipio")) %>%
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
    
    filename = paste0(output_folder,"municipios/muni_paciente_wide_",estado,".csv")
    print(paste0("Salvando: ",filename))
    fwrite(vac_muni_pac, file = filename)
    
    rm(vac_muni_pac);gc()
    
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
    fwrite(vac_muni_apli, file = filename)
    
    rm(vacinas, vac_muni_apli);gc()
  }
  
}

### SCRIPT

data_base <- list.files("dados/") %>% 
  grep("^dados_.*.csv", ., value = T) %>%
  substr(7,16) %>%
  as.Date() %>%
  max(na.rm = T)

if(is.infinite(data_base)) {
  print("Data em '^dados_.*.csv' não encontrada. Tentando ^limpo_dados_.*.csv")
  data_base <- list.files("dados/") %>% 
    grep("^limpo_dados_.*.csv", ., value = T) %>%
    substr(13,22) %>%
    as.Date() %>%
    max(na.rm = T)
}

print(paste0("Data_base: ",data_base))

# Processar municípios de estados sem split

files_out <- list.files("dados/")
estados <- substr(grep("^limpo_dados",files_out, value = T), 24,25)

for(j in estados) {
  print(j)
  contar_doses_municipio(estado = j, data_base = data_base, split = FALSE)
 }

# Processar municípios de estados com split

print("starting SP")
contar_doses_municipio(estado = "SP", data_base = data_base, split = TRUE)

print("Processamento de dados: finalizado")
#####

print("Iniciando a união das tabelas")

files <- list.files("output/municipios")
files_pac <- grep("paciente", files, value = T)
files_apl <- grep("aplica", files, value = T)

df_pac <- data.frame()
for(i in files_pac) {
  print(i)
  pac <- fread(paste0("output/municipios/",i)) %>% data.frame()
  df_pac <- bind_rows(df_pac, pac)
}

# Corrigir código de cidades satélite em Brasília
df_pac$muni_pac[grepl("^53",df_pac$muni_pac)] <- "530010"

# Agrupar resultados para todos os municípios
final_pac <- df_pac %>% mutate(muni_pac = factor(muni_pac)) %>% group_by(muni_pac) %>% summarise_all(sum)

# Somar colunas com idades 90+ com as colunas 80-89, para tornar 80+
final_pac2 <- final_pac
final_pac2[,grep("I9",colnames(final_pac2))] = final_pac2[,grep("I9",colnames(final_pac2))] + final_pac2[,grep("I10",colnames(final_pac2))]
final_pac2 <- final_pac2[,-grep("I10",colnames(final_pac2))]

# Salvar
filename <- "output/sipni_muni_residencia.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(final_pac2, file = filename)

# Agrupar em adolescentes, adultos e idosos
final_pac2 <- final_pac

final_pac2[,grep("I3",colnames(final_pac2))] = final_pac2[,grep("I3",colnames(final_pac2))] + 
  final_pac2[,grep("I4",colnames(final_pac2))] +
  final_pac2[,grep("I5",colnames(final_pac2))] +
  final_pac2[,grep("I6",colnames(final_pac2))] 

final_pac2[,grep("I7",colnames(final_pac2))] = final_pac2[,grep("I7",colnames(final_pac2))] + 
  final_pac2[,grep("I8",colnames(final_pac2))] +
  final_pac2[,grep("I9",colnames(final_pac2))] +
  final_pac2[,grep("I10",colnames(final_pac2))] 

final_pac2 <- final_pac2[,c(1,grep("I1_|I2_|I3_|I7_",colnames(final_pac2)))]

colnames(final_pac2) <- gsub("I1_", "IA_", colnames(final_pac2))
colnames(final_pac2) <- gsub("I2_", "IB_", colnames(final_pac2))
colnames(final_pac2) <- gsub("I3_", "IC_", colnames(final_pac2))
colnames(final_pac2) <- gsub("I7_", "ID_", colnames(final_pac2))

# Salvar
filename <- "output/sipni_muni_residencia_agrupado.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(final_pac2, file = filename)

###
df_apl <- data.frame()
for(i in files_apl) {
  print(i)
  apl <- fread(paste0("output/municipios/",i)) %>% data.frame()
  df_apl <- bind_rows(df_apl, apl)
}

# Corrigir código de cidades satélite em Brasília
df_apl$muni_apli[grepl("^53",df_apl$muni_apli)] <- "530010"

# Agrupar resultados para todos os municípios
final_apl <- df_apl %>% mutate(muni_apli = factor(muni_apli)) %>% group_by(muni_apli) %>% summarise_all(sum)

# Agrupar 80-89 e 90+ em 80+
final_apl2 <- final_apl
final_apl2[,grep("I9",colnames(final_apl2))] = final_apl2[,grep("I9",colnames(final_apl2))] + final_apl2[,grep("I10",colnames(final_apl2))]
final_apl2 <- final_apl2[,-grep("I10",colnames(final_apl2))]

filename <- "output/sipni_muni_aplicacao.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(final_apl2, file = filename)

# Agrupar em adolescentes, adultos e idosos
final_apl2 <- final_apl

final_apl2[,grep("I3",colnames(final_apl2))] = final_apl2[,grep("I3",colnames(final_apl2))] + 
                                                final_apl2[,grep("I4",colnames(final_apl2))] +
                                                final_apl2[,grep("I5",colnames(final_apl2))] +
                                                final_apl2[,grep("I6",colnames(final_apl2))] 
  
final_apl2[,grep("I7",colnames(final_apl2))] = final_apl2[,grep("I7",colnames(final_apl2))] + 
                                                final_apl2[,grep("I8",colnames(final_apl2))] +
                                                final_apl2[,grep("I9",colnames(final_apl2))] +
                                                final_apl2[,grep("I10",colnames(final_apl2))] 

final_apl2 <- final_apl2[,c(1,grep("I1_|I2_|I3_|I7_",colnames(final_apl2)))]

colnames(final_apl2) <- gsub("I1_", "IA_", colnames(final_apl2))
colnames(final_apl2) <- gsub("I2_", "IB_", colnames(final_apl2))
colnames(final_apl2) <- gsub("I3_", "IC_", colnames(final_apl2))
colnames(final_apl2) <- gsub("I7_", "ID_", colnames(final_apl2))

filename <- "output/sipni_muni_aplicacao_agrupado.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(final_apl2, file = filename)

#### Formato long: municipio de residência

pac_long <- final_pac %>%
  gather(key = "key", value = "n", -muni_pac) %>%
  mutate(SE = substr(key, 8, nchar(key)) %>% 
               sub("_", "", x = .) %>%
               sub("F", "", x = .) %>%
               sub("M", "", x = .) %>%
               as.numeric(),
         dose = factor(substr(key, 2,2)),
         agegroup = factor(gsub("_","",substr(key, 5,6)), 
                                levels = 1:10, ordered = T),
         sex = factor(ifelse(grepl("F", key), "F", "M"))) %>%
         select(-key) %>%
         arrange(muni_pac, sex, dose, agegroup, SE) %>%
         rename(codigo_municipio = muni_pac)
  
# Reordenar colunas
pac_long <- pac_long[,c("codigo_municipio","SE","dose","agegroup","sex","n")]

filename <- "output/sipni_muni_residencia_long.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(pac_long, file = filename)

#### Formato long: municipio de aplicacao

apl_long <- final_apl %>%
  gather(key = "key", value = "n", -muni_apli) %>%
  mutate(SE = substr(key, 8, nchar(key)) %>% 
           sub("_", "", x = .) %>%
           sub("F", "", x = .) %>%
           sub("M", "", x = .) %>%
           as.numeric(),
         dose = factor(substr(key, 2,2)),
         agegroup = factor(gsub("_","",substr(key, 5,6)), 
                           levels = 1:10, ordered = T),
         sex = factor(ifelse(grepl("F", key), "F", "M"))) %>%
  select(-key) %>%
  arrange(muni_apli, sex, dose, agegroup, SE) %>%
  rename(codigo_municipio = muni_apli)

# Reordenar colunas
apl_long <- apl_long[,c("codigo_municipio","SE","dose","agegroup","sex","n")]

filename <- "output/sipni_muni_aplicacao_long.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(apl_long, file = filename)

print("Finalizado.")
###
# 
# ibge <- read_csv2("dados/municipios_codigos.csv")
# 
# ibge2 <- ibge %>% select(`Código Município Completo`, Nome_Município, Nome_UF) %>%
#   rename(codigo = `Código Município Completo`,
#          municipio = Nome_Município,
#          UF = Nome_UF) %>%
#   mutate(codigo = factor(substr(codigo,1,6)))
# 
# fw3 <- apl_long %>% left_join(ibge2, by = c("codigo_municipio" = "codigo"), na_matches = "never")
