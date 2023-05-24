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
                          data_base = "2022-07-19",
                          split = FALSE) {
  
  data_base_title <- format(as.Date(data_base), format = "%Y_%m_%d")
  ceiling_month = ceiling_date(ymd(data_base), "month")
  
  if(split){
    pattern <- paste0("split_sorted_limpo_dados_",data_base,"_",estado)
    arquivos <- grep(pattern, list.files(input_folder), value = T)
    indice = 0
    
    for(arquivo in arquivos){
      
      vacinas <- fread(paste0(input_folder,arquivo),
                       select = c("paciente_id", "paciente_dataNascimento", 
                                  "vacina_dataAplicacao",
                                  "vacina_codigo",
                                  "paciente_endereco_coIbgeMunicipio",
                                  "estabelecimento_municipio_codigo"),
                       colClasses = c("paciente_id" = "factor",
                                      "paciente_dataNascimento" = "Date",
                                      "vacina_dataAplicacao" = "Date",
                                      "vacina_codigo" = "numeric",
                                      "paciente_endereco_coIbgeMunicipio" = "factor",
                                      "estabelecimento_municipio_codigo" = "factor"), 
                       encoding = "UTF-8") %>%
        data.frame()
      
      indice = indice + 1
      
      print(paste0(estado, " data succesfully loaded. Preparing data: split #",indice))
      
      ###### Create log of removed values due to missing data
      
      df.log <- data.frame(paciente_id = sum(is.na(vacinas$paciente_id)),
                 paciente_dataNascimento = sum(is.na(vacinas$paciente_dataNascimento)),
                 vacina_dataAplicacao = sum(is.na(vacinas$vacina_dataAplicacao)),
                 vacina_codigo = sum(is.na(vacinas$vacina_codigo)),
                 paciente_endereco_uf = sum(is.na(vacinas$paciente_endereco_uf)),
                 paciente_endereco_coIbgeMunicipio = sum(is.na(vacinas$paciente_endereco_coIbgeMunicipio)),
                 estabelecimento_municipio_codigo = sum(is.na(vacinas$estabelecimento_municipio_codigo)),
                 state = estado,
                 indice = indice)
      
      filename <- paste0("log_municipios_yale_", data_base_title,".csv")
      
      if(any(grepl(filename, list.files(paste0(output_folder,"log/"))))) {
        
        # Acrescenta o log para o arquivo anterior
        
        log_table_todos <- read.csv(paste0(output_folder,"log/",filename), row.names = 1)
        log_table_todos <- bind_rows(log_table_todos, df.log)
        write.csv(log_table_todos, file = paste0(output_folder, "log/", filename))
        
      } else {
        
        # Cria um arquivo novo
        write.csv(df.log, file = paste0(output_folder, "log/", filename))
        
      }
      
      ###### End of log of removed values due to missing data
      
      vacinas$paciente_id <- factor(as.numeric(factor(vacinas$paciente_id)))
      
      vacinas[vacinas == ""] <- NA
      
      gc()
      
      # Clean Data
      vacinas <- vacinas %>% 
        distinct(.keep_all = TRUE) %>%
        filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao >= as.Date("2021-01-17")) %>%
        drop_na() %>%
        filter(paciente_dataNascimento > "1900-01-01")
      
      colnames(vacinas) <- c("id", "nasc", "date","vacina","muni_pac","muni_apli")
      
      # Filter ids with more than 5 doses and compute age at first dose  
      vacinas <- vacinas %>% 
        group_by(id) %>% 
        mutate(nasc = min(nasc, na.rm = T)) %>%
        ungroup() %>%
        mutate(idade = as.numeric(date - nasc) %/% 365.25)  %>% 
        select(-nasc)
      
      # Standardize codes for vaccines
      vacinas$vacina[vacinas$vacina == 89] <- 85
      vacinas$vacina[vacinas$vacina == 98] <- 86
      vacinas$vacina[vacinas$vacina %in% c(99,102,103)] <- 87
      
      
      vacinas <- vacinas %>%
        mutate(agegroup = factor(cut(idade,
                                     breaks = c(0,18,seq(30,90,10),Inf),
                                     include.lowest = T, 
                                     right = F,
                                     labels = F))) %>%
        select(-idade) %>%
        mutate(month = ceiling_date(ymd(date), "month")) %>%
        arrange(id, date) %>% 
        group_by(id) %>% 
        mutate(doses = 1:n(),
               muni_pac = first(muni_pac),
               muni_apli = first(muni_apli)) %>%
        ungroup() %>%
        filter(doses < 6)
      
      vac_muni_pac <- vacinas %>%
        filter(muni_pac != c("None")) %>%
        filter(muni_pac != c("999999")) %>%
        drop_na(muni_pac) %>%
        select(muni_pac, doses, agegroup, month, vacina) %>%
        drop_na() %>%
        mutate(muni_pac = factor(muni_pac, levels = unique(muni_pac)),
               doses = factor(doses, levels = c(1:5), labels = paste0("D",1:5))) %>%
        count(muni_pac, doses, agegroup, month, vacina) %>%
        complete(muni_pac, doses, agegroup, month = seq.Date(as.Date("2021-02-01"), ceiling_month, by="month"), 
                 vacina, fill = list(n = 0)) %>%
        arrange(muni_pac, doses, agegroup, month, vacina)
      
      filename = paste0(output_folder,"municipios/muni_pac_yale_",estado,"_",indice,".csv") # muni_paciente_wide_ -> muni_yale
      print(paste0("Salvando: ",filename))
      fwrite(vac_muni_pac, file = filename)
      
      rm(vac_muni_pac);gc()
      
      vac_muni_apli <- vacinas %>%
        filter(muni_apli != c("None")) %>%
        filter(muni_apli != c("999999")) %>%
        drop_na(muni_apli) %>%
        select(muni_apli, doses, agegroup, month, vacina) %>%
        drop_na() %>%
        mutate(muni_apli = factor(muni_apli, levels = unique(muni_apli)),
               doses = factor(doses, levels = c(1:5), labels = paste0("D",1:5))) %>%
        count(muni_apli, doses, agegroup, month, vacina) %>%
       # complete(muni_apli, doses, agegroup, month = seq.Date(as.Date("2021-02-01"), ceiling_month, by="month"), 
       #          vacina, fill = list(n = 0)) %>%
        arrange(muni_apli, doses, agegroup, month, vacina)
      
      filename = paste0(output_folder,"municipios/muni_apli_yale_",estado,"_",indice,".csv")
      print(paste0("Salvando: ",filename))
      fwrite(vac_muni_apli, file = filename)
      
      rm(vacinas, vac_muni_apli);gc()
      
    } # for
  } else {
    
    # Non split
    
    vacinas <- fread(paste0(input_folder,"limpo_dados_",data_base,"_",estado,".csv"),
                     select = c("paciente_id", "paciente_dataNascimento", 
                                "vacina_dataAplicacao",
                                "vacina_codigo",
                                "paciente_endereco_coIbgeMunicipio",
                                "estabelecimento_municipio_codigo"),
                     colClasses = c("paciente_id" = "factor",
                                    "paciente_dataNascimento" = "Date",
                                    "vacina_dataAplicacao" = "Date",
                                    "vacina_codigo" = "numeric",
                                    "paciente_endereco_coIbgeMunicipio" = "factor",
                                    "estabelecimento_municipio_codigo" = "factor"), 
                     encoding = "UTF-8") %>%
      data.frame()
    
    print(paste0(estado, " data succesfully loaded. Preparing data... 1"))
    
    ###### Create log of removed values due to missing data
    df.log <- data.frame(paciente_id = sum(is.na(vacinas$paciente_id)),
                         paciente_dataNascimento = sum(is.na(vacinas$paciente_dataNascimento)),
                         vacina_dataAplicacao = sum(is.na(vacinas$vacina_dataAplicacao)),
                         vacina_codigo = sum(is.na(vacinas$vacina_codigo)),
                         paciente_endereco_uf = sum(is.na(vacinas$paciente_endereco_uf)),
                         paciente_endereco_coIbgeMunicipio = sum(is.na(vacinas$paciente_endereco_coIbgeMunicipio)),
                         estabelecimento_municipio_codigo = sum(is.na(vacinas$estabelecimento_municipio_codigo)),
                         state = estado,
                         indice = 1)
    
    filename <- paste0("log_municipios_yale_", data_base_title,".csv")
    
    if(any(grepl(filename, list.files(paste0(output_folder,"log/"))))) {
      
      # Acrescenta o log para o arquivo anterior
      
      log_table_todos <- read.csv(paste0(output_folder,"log/",filename), row.names = 1)
      log_table_todos <- bind_rows(log_table_todos, df.log)
      write.csv(log_table_todos, file = paste0(output_folder, "log/", filename))
      
    } else {
      
      # Cria um arquivo novo
      write.csv(df.log, file = paste0(output_folder, "log/", filename))
      
    }
    ###### End of log of removed values due to missing data
    
    vacinas$paciente_id <- factor(as.numeric(factor(vacinas$paciente_id)))
    
    vacinas[vacinas == ""] <- NA
    
    # Clear cache
    gc()

    # Clean Data
    vacinas <- vacinas %>% 
      distinct(.keep_all = TRUE) %>%
      filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao >= as.Date("2021-01-17")) %>%
      drop_na() %>%
      filter(paciente_dataNascimento > "1900-01-01")
  
    colnames(vacinas) <- c("id", "nasc", "date","vacina","muni_pac","muni_apli")
  
    # Filter ids with more than 5 doses and compute age at first dose  
    vacinas <- vacinas %>% 
      group_by(id) %>% 
      mutate(nasc = min(nasc, na.rm = T)) %>%
      ungroup() %>%
      mutate(idade = as.numeric(date - nasc) %/% 365.25)  %>% 
      select(-nasc)
    
    # Standardize codes for vaccines
    vacinas$vacina[vacinas$vacina == 89] <- 85
    vacinas$vacina[vacinas$vacina == 98] <- 86
    vacinas$vacina[vacinas$vacina %in% c(99,102,103)] <- 87
    
    
    vacinas <- vacinas %>%
      mutate(agegroup = factor(cut(idade,
                                   breaks = c(0,18,seq(30,90,10),Inf),
                                   include.lowest = T, 
                                   right = F,
                                   labels = F))) %>%
      select(-idade) %>%
      mutate(month = ceiling_date(ymd(date), "month")) %>%
      arrange(id, date) %>% 
      group_by(id) %>% 
      mutate(doses = 1:n(),
             muni_pac = first(muni_pac),
             muni_apli = first(muni_apli)) %>%
      ungroup() %>%
      filter(doses < 6)
    
    vac_muni_pac <- vacinas %>%
      filter(muni_pac != c("None")) %>%
      filter(muni_pac != c("999999")) %>%
      drop_na(muni_pac) %>%
      select(muni_pac, doses, agegroup, month, vacina) %>%
      drop_na() %>%
      mutate(muni_pac = factor(muni_pac, levels = unique(muni_pac)),
             doses = factor(doses, levels = c(1:5), labels = paste0("D",1:5))) %>%
      count(muni_pac, doses, agegroup, month, vacina) %>%
      complete(muni_pac, doses, agegroup, month = seq.Date(as.Date("2021-02-01"), ceiling_month, by="month"), 
               vacina, fill = list(n = 0)) %>%
      arrange(muni_pac, doses, agegroup, month, vacina)
    
    filename = paste0(output_folder,"municipios/muni_pac_yale_",estado,".csv")
    print(paste0("Salvando: ",filename))
    fwrite(vac_muni_pac, file = filename)
    
    rm(vac_muni_pac);gc()
    
    vac_muni_apli <- vacinas %>%
      filter(muni_apli != c("None")) %>%
      filter(muni_apli != c("999999")) %>%
      drop_na(muni_apli) %>%
      select(muni_apli, doses, agegroup, month, vacina) %>%
      drop_na() %>%
      mutate(muni_apli = factor(muni_apli, levels = unique(muni_apli)),
             doses = factor(doses, levels = c(1:5), labels = paste0("D",1:5))) %>%
      count(muni_apli, doses, agegroup, month, vacina) %>%
      #complete(muni_apli, doses, agegroup, month = seq.Date(as.Date("2021-02-01"), ceiling_month, by="month"), 
      #         vacina, fill = list(n = 0)) %>%
      arrange(muni_apli, doses, agegroup, month, vacina)
    
    filename = paste0(output_folder,"municipios/muni_apli_yale_",estado,".csv")
    print(paste0("Salvando: ",filename))
    fwrite(vac_muni_apli, file = filename)
    
    rm(vacinas, vac_muni_apli);gc()
  }
  
}

### SCRIPT

print("Iniciando extração de dados para projeto com a Yale University")

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
files_pac <- grep("pac_yale", files, value = T)
files_apl <- grep("apli_yale", files, value = T)

df_pac <- data.frame()
for(i in files_pac) {
  print(i)
  pac <- fread(paste0("output/municipios/",i), 
               colClasses = c("muni_pac" = "character",
                              "doses" = "factor",
                              "agegroup" = "factor",
                              "month" = "Date",
                              "vacina" = "integer",
                              "n" = "integer")) %>% data.frame()
  df_pac <- bind_rows(df_pac, pac)
  rm(pac);gc()
}

# Corrigir código de cidades satélite em Brasília
df_pac$muni_pac[grepl("^53",df_pac$muni_pac)] <- "530010"

# Agrupar resultados para todos os municípios
final_pac <- df_pac %>% 
  mutate(muni_pac = factor(muni_pac)) %>% 
  group_by(muni_pac, doses, agegroup, month, vacina) %>% 
  summarise(n = sum(n, na.rm =T)) %>%
  ungroup() %>%
  mutate(vacina = factor(vacina, levels = 85:88, labels = c("AZ","CV","PF","JS")))

# Salvar
filename <- "output/sipni_muni_residencia_yale.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(final_pac2, file = filename)

###
df_apl <- data.frame()
for(i in files_apl) {
  print(i)
  apl <- fread(paste0("output/municipios/",i), 
               colClasses = c("muni_pac" = "character",
                              "doses" = "factor",
                              "agegroup" = "factor",
                              "month" = "Date",
                              "vacina" = "integer",
                              "n" = "integer")) %>% data.frame()
  df_apl <- bind_rows(df_apl, apl)
  rm(apl);gc()
}

# Corrigir código de cidades satélite em Brasília
df_apl$muni_apli[grepl("^53",df_apl$muni_apli)] <- "530010"

# Agrupar resultados para todos os municípios
final_apl <- df_apl %>% 
  mutate(muni_apli = factor(muni_apli)) %>% 
  group_by(muni_apli, doses, agegroup, month, vacina) %>% 
  summarise(n = sum(n, na.rm =T)) %>%
  ungroup() %>%
  mutate(vacina = factor(vacina, levels = 85:88, labels = c("AZ","CV","PF","JS")))

filename <- "output/sipni_muni_aplicacao_yale.csv.gz"
print(paste0("Salvando: ", filename))
write.csv.gz(final_apl2, file = filename)

print("Finalizada a extração de dados para Yale University.")
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
