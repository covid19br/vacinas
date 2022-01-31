if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
if(!require(scales)){install.packages("scales"); library(scales)}
if(!require(optparse)){install.packages("scales"); library(optparse)}

doses_nomes <- function(x){
  
  if(grepl("Reforço|Adicional",x,ignore.case = T)){
    return("R")
  } 
  if(grepl("1",x)){
    return("D1")
  }
  if(grepl("2",x)){
    return("D2")
  }
  return("DU")
}

#' prepare_table:
#' Lê os arquivos brutos de dados do PNI-SI, seleciona apenas as colunas relevantes
#' e realiza a limpeza e reorganização dos dados.
#' @estado: character. Sigla do estado do Brasil para leitura.
#' @write.dose.types: logical. Se a função deve salvar uma tabela em .csv com as categorias
#' presentes na coluna "vacina_descricao_dose"
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' @data_base: character. Data da base de dados, no formato %Y-%m-%d

prepare_table <- function(estado, write.dose.types = TRUE,
                          input_folder = "dados/",
                          output_folder = "output/",
                          data_base = "2022-01-15",split = F) {
  if(split){
    pattern <- paste0("split_sorted_limpo_dados_",data_base,"_",estado)
    arquivos <- list.files(input_folder,pattern)
    indice = 0
    for(arquivo in arquivos){
      todas_vacinas <- fread(paste0(input_folder,arquivo), 
                             select = c("paciente_id", "paciente_dataNascimento", "vacina_dataAplicacao", 
                                        "vacina_descricao_dose", "vacina_codigo"),
                             colClasses = c("paciente_id" = "factor",
                                            "paciente_dataNascimento" = "Date",
                                            "vacina_dataAplicacao" = "Date",
                                            "vacina_descricao_dose" = "character",
                                            "vacina_codigo" = "integer"), 
                             encoding = "UTF-8")
      print("Data succesfully loaded")
      print("Preparing data... 1")
      indice = indice+1
      todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 89] <- 85
      
      todas_vacinas <- todas_vacinas %>% 
        distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE) %>% 
        distinct(paciente_id,vacina_dataAplicacao,.keep_all = TRUE)
      
      if(write.dose.types) {
        write.csv(data.frame(table(todas_vacinas$vacina_descricao_dose)), 
                  file = paste0(output_folder,estado,"_dose_types_log.csv"), quote = FALSE, row.names = FALSE)
      }
      
      # Substitute Dose names and transform into factor
      #dose_types <- sort(unique(todas_vacinas$vacina_descricao_dose))
      # load("nomes_doses.Rdata")
      todas_vacinas$dose <- as.factor(sapply(todas_vacinas$vacina_descricao_dose,doses_nomes))
      # todas_vacinas$dose
      # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[1],todas_vacinas$vacina_descricao_dose)] <- "D1"
      # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[2],todas_vacinas$vacina_descricao_dose)] <- "D2"
      # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[3],todas_vacinas$vacina_descricao_dose) &
                                            # todas_vacinas$vacina_codigo == 88] <- "DU"
      # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[4],todas_vacinas$vacina_descricao_dose) &
                                            # todas_vacinas$vacina_codigo == 88] <- "DU"
      todas_vacinas <- todas_vacinas %>% filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao > as.Date("2021-01-17"))
      todas_vacinas <- todas_vacinas %>% drop_na()
      todas_vacinas <- todas_vacinas %>% filter(paciente_dataNascimento > "1900-01-01")
      
      ##
      print("Preparing data... 2")
      
      ##########################
      
      tabela_vacinas <- todas_vacinas %>% filter(vacina_codigo %in% c(85,86,87,88)) %>% select(-vacina_descricao_dose) %>% as.data.frame()
      rm(todas_vacinas);gc()
      colnames(tabela_vacinas) <- c("id", "nasc", "data","vacina","doses")
      
      nreg <- tabela_vacinas %>% select(id) %>% group_by(id) %>% count() # Contar frequencia registros_id
      tabela_vacinas <- tabela_vacinas %>% left_join(nreg, by = "id") %>% filter(n <= 5) %>% mutate(id = factor(id)) # Cortar ids com mais de 2 registros
      
      rm(nreg);gc()
      
      # Filter if it has more than one D1 or D2 per ID
      remove_ids <- tabela_vacinas %>% group_by(id, doses, .drop = FALSE) %>% summarise(m = n()) %>% filter(!((doses != "R" & m <= 1) | doses == "R"))
      tabela_vacinas = tabela_vacinas %>% filter(!(id %in% remove_ids$id)) %>% mutate(id = droplevels(id))
      
      rm(remove_ids);gc()
      
      age <- tabela_vacinas %>%
        filter(doses == "D1" | doses == "DU") %>% mutate(idade = floor(time_length(data - nasc, "years"))) %>%
        select(id,idade)
      tabela_vacinas <- tabela_vacinas %>% left_join(age, by = "id") %>% select(-nasc)
      # tabela_vacinas$complete_d1 <- tabela_vacinas$data + 14 < as.Date(data_base) & 
      #   (tabela_vacinas$doses == levels(tabela_vacinas$doses)[1] |
      #      tabela_vacinas$doses == levels(tabela_vacinas$doses)[3])
      # tabela_vacinas$complete <- tabela_vacinas$data + 14 < as.Date(data_base) & 
      #   (tabela_vacinas$doses == levels(tabela_vacinas$doses)[2] |
      #      tabela_vacinas$doses == levels(tabela_vacinas$doses)[3])
      print("Saving data")
      
      fwrite(tabela_vacinas, file = paste0(output_folder, estado,"_",indice, "_PNI_clean.csv"))
    }
  } else {
    todas_vacinas <- fread(paste0(input_folder,"limpo_dados_",data_base,"_",estado,".csv"), 
                           select = c("paciente_id", "paciente_dataNascimento", "vacina_dataAplicacao", 
                                      "vacina_descricao_dose", "vacina_codigo"),
                           colClasses = c("paciente_id" = "factor",
                                          "paciente_dataNascimento" = "Date",
                                          "vacina_dataAplicacao" = "Date",
                                          "vacina_descricao_dose" = "character",
                                          "vacina_codigo" = "integer"), 
                           encoding = "UTF-8")
    
    print("Data succesfully loaded")
    print("Preparing data... 1")
    
    todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 89] <- 85
    
    todas_vacinas <- todas_vacinas %>% 
      distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE) %>% 
      distinct(paciente_id,vacina_dataAplicacao,.keep_all = TRUE)
    
    if(write.dose.types) {
      write.csv(data.frame(table(todas_vacinas$vacina_descricao_dose)), 
                file = paste0(output_folder,estado,"_dose_types_log.csv"), quote = FALSE, row.names = FALSE)
    }
    
    # Substitute Dose names and transform into factor
    #dose_types <- sort(unique(todas_vacinas$vacina_descricao_dose))
    # load("nomes_doses.Rdata")
    todas_vacinas$dose <- as.factor(sapply(todas_vacinas$vacina_descricao_dose,doses_nomes))
    # todas_vacinas$dose
    # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[1],todas_vacinas$vacina_descricao_dose)] <- "D1"
    # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[2],todas_vacinas$vacina_descricao_dose)] <- "D2"
    # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[3],todas_vacinas$vacina_descricao_dose) &
    # todas_vacinas$vacina_codigo == 88] <- "DU"
    # todas_vacinas$vacina_descricao_dose[grepl(nomes_doses[4],todas_vacinas$vacina_descricao_dose) &
    # todas_vacinas$vacina_codigo == 88] <- "DU"
    todas_vacinas <- todas_vacinas %>% filter(vacina_dataAplicacao < as.Date(data_base) & vacina_dataAplicacao > as.Date("2021-01-17"))
    todas_vacinas <- todas_vacinas %>% drop_na()
    todas_vacinas <- todas_vacinas %>% filter(paciente_dataNascimento > "1900-01-01")
    
    ##
    print("Preparing data... 2")
    
    ##########################
    
    tabela_vacinas <- todas_vacinas %>% filter(vacina_codigo %in% c(85,86,87,88)) %>% select(-vacina_descricao_dose) %>% as.data.frame()
    rm(todas_vacinas);gc()
    colnames(tabela_vacinas) <- c("id", "nasc", "data","vacina","doses")
    
    nreg <- tabela_vacinas %>% select(id) %>% group_by(id) %>% count() # Contar frequencia registros_id
    tabela_vacinas <- tabela_vacinas %>% left_join(nreg, by = "id") %>% filter(n < 5) %>% mutate(id = factor(id)) # Cortar ids com mais de 5 registros
    rm(nreg);gc()
    
    # Filter if it has more than one D1 or D2 per ID
    remove_ids <- tabela_vacinas %>% group_by(id, doses, .drop = FALSE) %>% summarise(m = n()) %>% filter(!((doses != "R" & m <= 1) | doses == "R"))
    tabela_vacinas = tabela_vacinas %>% filter(!(id %in% remove_ids$id)) %>% mutate(id = droplevels(id))
  
#rm(remove_ids);gc()
    
    age <- tabela_vacinas %>%
      filter(doses == "D1" | doses == "DU") %>% mutate(idade = floor(time_length(data - nasc, "years"))) %>%
      select(id,idade)
    tabela_vacinas <- tabela_vacinas %>% left_join(age, by = "id") %>% select(-nasc)
    
    # tabela_vacinas$complete_d1 <- tabela_vacinas$data + 14 < as.Date(data_base) & 
    #   (tabela_vacinas$doses == levels(tabela_vacinas$doses)[1] |
    #      tabela_vacinas$doses == levels(tabela_vacinas$doses)[3])
    # tabela_vacinas$complete <- tabela_vacinas$data + 14 < as.Date(data_base) & 
    #   (tabela_vacinas$doses == levels(tabela_vacinas$doses)[2] |
    #      tabela_vacinas$doses == levels(tabela_vacinas$doses)[3])
    print("Saving data")
    
    filename = paste0(output_folder, estado, "_PNI_clean.csv")
    print(paste0("Salvando: ", filename))
    fwrite(tabela_vacinas, file = filename)
  }
}

#' prepara_historico:
#' Prepara e salva duas tabela com historico entre as doses.
#' A primeira tabela indica a frequencia de individuos em função 
#' do tempo de vacina de acordo com vacina, grupo etário e 
#' tempo entre data da primeira dose e data da base de dados.
#' A segunda tabela indica a frequencia de individuos de cada faixa etaria
#' que já receberam a segunda dose.
#' 
#' @estado: character. Sigla do estado do Brasil
#' @data_base: Date. Data da base de dados.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' 
#' Details: a função salva as tabelas de output em um arquivo .csv

prepara_historico <- function(estado = "SP", 
                              data_base = as.Date("2022-01-15"),
                              input_folder = "output/",
                              output_folder = "output/",
                              split = FALSE) {
  
  if(class(data_base) == "character") data_base <- as.Date(data_base)
  
  if(split) {
   
 # if(!require(doParallel)){install.packages("doParallel"); library(doParallel)}
  #registerDoParallel(cores=detectCores()-1)
    
  splited_files <- grep("[1-9]_PNI_clean.csv", list.files(input_folder), value = T)
  files <- grep(estado, splited_files, value = T)
  
  #tabela_wide = tibble()
  
  for(j in 1:length(files)) {
  print(files[j])
    
  tabela_vacinas <- fread(paste0(input_folder,files[j]), 
                          colClasses = c("id" = "factor",
                                         "data" = "character", "vacina" = "integer","n" = "integer",
                                         "doses" = "factor","idade" = "integer"), encoding = "UTF-8")
  
  tabela_vacinas$data <- as.Date(tabela_vacinas$data)
  
  count_doses <- tabela_vacinas %>% group_by(id) %>% summarise(m = n())
  tabela_vacinas <- tabela_vacinas %>% left_join(count_doses, by = "id")
  
  pretabela <- tabela_vacinas %>% select(-n, -m) %>% #select(-id) %>% 
    filter(data > "2021-01-01") %>%
    filter(data <= data_base) %>%
    drop_na(vacina, idade, data, doses) %>%
    mutate(vacina = factor(vacina, levels = c(85,86,87,88), labels = c("AZ","Coronavac","Pfizer","Janssen")),
           agegroup = factor(cut(idade, 
                          breaks = c(0,5,12,18,seq(30,90,10),Inf),
                          #breaks = c(seq(0,90,10),Inf), 
                          include.lowest = T, 
                          right = F,
                          labels = F)))

  #rm(tabela_vacinas);gc()
  
  tabela <- pretabela %>% count(vacina, agegroup, data,doses, .drop = FALSE)
  filename = paste0(output_folder,"doses_aplicadas/doses_aplicadas_",estado,"_",j,".csv")
  print(filename)
  fwrite(tabela, file= filename)
  
  ### Preparar tabela em formato wide
  
  tabela_id_idade <- pretabela %>% select(id, agegroup) %>% distinct()
  print(259)
  
    cut_points <- as.integer(c(seq(1,  nrow(pretabela), 3*10^6), nrow(pretabela)))
  #cut_points <- as.integer(c(seq(1,  9*10^6, 3*10^6), nrow(pretabela)))
  
    ini = Sys.time()
    
    #tabela_wide_split <- foreach(i = 1:(length(cut_points)-1),.combine = bind_rows) %dopar% {
    tabela_wide_split = tibble()
    
    for(i in 1:(length(cut_points)-1)){
      
      print(c(i,j,cut_points[i]))
      
      tabela_wide_split_temp <- pretabela[cut_points[i]:(cut_points[i+1]-1),] %>%
        select(-agegroup, -idade) %>%
        pivot_wider(id_cols = id,
                    names_from = doses,
                    values_from = c(data, vacina),
                    values_fn = first,
                    values_fill = NA)
      
      tabela_wide_split <- bind_rows(tabela_wide_split, tabela_wide_split_temp)
    }
    
  tabela_wide_split = tabela_wide_split %>%
    right_join(tabela_id_idade, by = "id", na_matches = "never") %>% select(-id)
  
  fin = Sys.time()
  print(fin - ini)
  
  rm(pretabela);gc()
  
  filename = paste0(output_folder,"wide/wide_doses_aplicadas_",estado,"_",j,".csv")
  print(paste0("Salvando: ",filename))
  fwrite(tabela_wide_split, file = filename)
  
  reforco <- tabela_wide_split %>%
    filter(!is.na(data_D2) & is.na(data_R) & is.na(vacina_DU)) %>%
    count(data_D2, vacina_D2, agegroup)
  print(298)
  reforco <- reforco %>% mutate(dif = as.integer(as.Date(Sys.time()) - data_D2)) %>% select(-n)
  print(300)
  filename = paste0(output_folder,"reforco/tempo_d2_reforco_",estado,"_",j,".csv")
  print(paste0("Salvando: ",filename))
  fwrite(reforco, file= filename)
  
   } #j
    
  } else {
  
  tabela_vacinas <- fread(paste0(input_folder,estado, "_PNI_clean.csv"), 
                          colClasses = c("id" = "factor",
                                         "data" = "character", "vacina" = "integer","n" = "integer",
                                         "doses" = "factor","idade" = "integer"), encoding = "UTF-8")
    
  tabela_vacinas$data <- as.Date(tabela_vacinas$data)
    
  count_doses <- tabela_vacinas %>% group_by(id) %>% summarise(m = n())
  tabela_vacinas <- tabela_vacinas %>% left_join(count_doses, by = "id")
    
  pretabela <- tabela_vacinas %>% select(-n, -m) %>% #select(-id) %>% 
    filter(data > "2021-01-01") %>%
    filter(data <= data_base) %>%
    drop_na(vacina, idade, data, doses) %>%
    mutate(vacina = factor(vacina, levels = c(85,86,87,88), labels = c("AZ","Coronavac","Pfizer","Janssen")),
           agegroup = factor(cut(idade, 
                                 breaks = c(0,5,12,18,seq(30,90,10),Inf),
                                 #breaks = c(seq(0,90,10),Inf), 
                                 include.lowest = T, 
                                 right = F,
                                 labels = F)))
    
  rm(tabela_vacinas);gc()
    
  tabela <- pretabela %>% count(vacina, agegroup, data,doses, .drop = FALSE)
  
  filename = paste0(output_folder,"doses_aplicadas/doses_aplicadas_",estado,".csv")
  print(paste0("Salvando: ",filename))
  fwrite(tabela, file = filename)
    
  tabela_id_idade <- pretabela %>% select(id, agegroup) %>% distinct()
    
  tabela_wide = pretabela %>%
    select(-agegroup, -idade) %>%
    pivot_wider(id_cols = id,
                names_from = doses,
                values_from = c(data, vacina),
                values_fn = first,
                values_fill = NA)
  
  rm(pretabela);gc()
  
  filename = paste0(output_folder,"wide/wide_doses_aplicadas_",estado,".csv")
  print(paste0("Salvando: ",filename))
  fwrite(tabela_wide, file = filename)
  
  ### Histórico apenas dos que necessitam de dose de reforço
  
  reforco <- tabela_wide %>%
    filter(!is.na(data_D2) & is.na(data_R) & is.na(vacina_DU)) %>%
    count(data_D2, vacina_D2, agegroup) #%>%
  #count(data_D1, data_D2, data_R, data_DU, vacina_D1, vacina_D2, vacina_R, vacina_DU, agegroup) %>%
  #filter(vacina_DU == "Janssen" & !is.na(vacina_D2)) # Remove casos em que D1 é Janssen e D2 é outra marca
  
  reforco <- reforco %>% mutate(dif = as.integer(as.Date(Sys.time()) - data_D2)) %>% select(-n)
  
  filename = paste0(output_folder,"reforco/tempo_d2_reforco_",estado,".csv")
  print(paste0("Salvando: ",filename))
  fwrite(reforco, file = filename)
  }

}

#' join_historico: 
#' Faz a união das tabelas que foram separadas para estados com bancos de dados grandes
#' 
#' @estado: character. Sigla do estado do Brasil
#' @data_base: Date. Data da base de dados.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' 
#' @return: a função não retorna nenhum output para o environment. 
#' Porém, salvar três arquivos diferentes:
#' * doses_aplicadas_{estado}.csv: tabela com histórico de doses aplicadas por data, vacina, 
#' grupo etário e doses
#' * wide_doses_aplicadas_{estado}.csv: tabela com vacina, grupo etário e datas de aplicação
#' de cada dose por indivíduo (id)
#' * tempo_d2_reforco_{estado}.csv: tabela com vacina, grupo etário e datas de aplicação
#' de cada dose por indivíduo (id), apenas para aqueles que receberam a segunda dose mas
#' não receberam a dose de reforço 

join_historico <- function(estado = "SP", 
                           data_base = as.Date("2022-01-15"),
                           input_folder = "output/",
                           output_folder = "output/") {

  ### Create dataset with the following variables (long format table):
  ### vacina, agegroup, data, doses, n
  
  # Locate splited files
  splited_files <- grep("^doses_aplicadas_.+[1-9].csv", list.files(paste0(input_folder,"doses_aplicadas/")), value = T)
  files <- grep(estado, splited_files, value = T)
  
  # Aggregate data into single table
  doses_aplicadas <- tibble()
  for(j in files) {
    print(paste0("Reading: ",j))
    df = tibble(fread(paste0(input_folder,j)))
    df[is.na(df)] <- NA
    doses_aplicadas <- rbind(doses_aplicadas, df)
  }
  
  # Sum values according to groups
  tabela <- doses_aplicadas %>% 
    group_by(vacina, agegroup, data,doses, .drop = FALSE) %>%
    summarise(n = sum(n))
  
  # Save aggregated data
  filename = paste0(output_folder,"doses_aplicadas/doses_aplicadas_",estado,".csv")
  print(paste0("Saving: ",filename))
  fwrite(tabela, file = filename)
  
  ### Create dataset with the following variables (wide format table):
  ### data_D1, data_D2, data_R, data_DU, vacina_D1, vacina_D2, vacina_R, vacina_DU, agegroup
  
  # Locate splited files
  splited_files <- grep("wide_doses_aplicadas_.+[1-9].csv", list.files(paste0(input_folder,"wide/")), value = T)
  files <- grep(estado, splited_files, value = T)
  
  # Aggregate data into single table
  wide_doses <- tibble()
  for(j in files) {
    print(paste0("Reading: ",j))
    df = tibble(fread(paste0(input_folder,j)))
    df[is.na(df)] <- NA
    wide_doses <- rbind(wide_doses, df)
  }
  
  # Save aggregated data
  filename = paste0(output_folder,"wide/wide_doses_aplicadas_",estado,".csv")
  print(paste0("Saving: ",filename))
  fwrite(wide_doses, file = filename)
  
  ### Individuals who require booster shots table
  ### Create dataset with the following variables (wide format table):
  ### data_D2, vacina_D2, agegroup, n, dif  
  
  reforco <- wide_doses %>%
    filter(!is.na(data_D2) & is.na(data_R) & is.na(data_DU)) %>%
    count(data_D2, vacina_D2, agegroup)  %>% 
    mutate(dif = as.integer(as.Date(Sys.time()) - as.Date(data_D2)))
  
  filename = paste0(output_folder,"reforco/tempo_d2_reforco_",estado,".csv")
  print(paste0("Saving: ",filename))
  fwrite(reforco, file = filename)
  
  print(paste0(estado," done."))
}


#' plot_historico
#' Plota representação visual da diferença entre tempo em dias 
#' da primeira dose e a última atualização da base de dados.
#' 
#' @tabela_dose: data.frame. Tabela com dados para a visualização. Deve possuir quatro colunas:
#' vacina: character. sigla da vacina a ser plotada. Coronavac, AZ (Astrazeneca), ou Pfizer.
#' agegroup: integer. Valores **i** = {1,2,..,10}, código da faixa etária, representa, onde age_floor = (i-1)*10
#' tempo_doses: tempo em dias entre data da primeira dose e data do banco de dados.
#' n: quantidade de indivíduos da faixa etária **i** que recebeu a dose a **tempo_doses** dias para a **vacina**.
#' @vacina_nome: character. Nome da vacina a ser plotada.
#' @xlabel: Legenda do eixo x
#' @ylabel: Legenda do eixo y
#' 
#' @return: ggplot object

plot_historico <- function(tabela_dose, vacina_nome = "AZ",
                           xlabel = "\nTempo em relação a data da base (dias)",
                           ylabel = "Faixa etária\n") {
  
  if(vacina == "AZ") {nome_vacina <- "Astrazeneca"} else {nome_vacina <- vacina}
  print(vacina)
  age_labels <-  levels(cut(1:100, 
                            breaks = c(0,5,12,18,seq(30,90,10),Inf),
                          #  breaks = c(seq(0,90,10),Inf), 
                            include.lowest = T, right = F))
  
  max_time <- max(as.numeric(tabela_dose$tempo_doses))
  ggdose <- ggplot(tabela_dose %>% filter(vacina == vacina_nome), 
                   aes(y = agegroup, x = abs(max_time - as.numeric(tempo_doses)) - max_time, fill = n)) +
    geom_tile() + xlab(xlabel) +
    scale_fill_gradient2("",
                         low = "white", mid = "orange", high = "red", 
                         midpoint = max(as.numeric(tabela_dose$n))/2) +
    scale_y_discrete(ylabel, 1:10, age_labels) +
    ggtitle(nome_vacina)
  
  return(ggdose)
}

#' plot_proportion
#' Plota a proporção de indivíduos de cada faixa etária vacinados com as doses 1 e 2
#' em determinado estado ao longo do tempo. 
#' 
#' @estado: character. Sigla do estado
#' @distribuicao_etaria: data.frame. Tabela com faixas etárias e população em cada faixa para determinado estado.
#' @input_folder: character. Caminho para leitura de dados
#' @output_folder: character. Caminho para escrever os dados.
#' @write. logical. Se os gráficos devem ser salvos em formato .png
#' @return. logical. Se a função retorna objetos ggplot
#' @data_base. character. Data da base de dados, em formato "%Y-%m-%d"

plot_proportion <- function(estado, distribuicao_etaria, 
                            input_folder = "output/",
                            output_folder = "output/",
                            write = TRUE,
                            return = TRUE,
                            data_base = '2021-07-27'){
  
  tabela_vacinas <- fread(paste0(input_folder, estado, "_PNI_clean.csv"), 
                          colClasses = c("id" = "factor", "nasc" = "Date",
                                         "categoria" = "integer", "data" = "Date",
                                         "doses" = "factor"))
  print("Data succesfully loaded")
  
  ##########################
  
  D1 <- tabela_vacinas %>% filter(doses == "D1")
  D1 <- D1 %>% select(-id) %>% 
    #filter(categoria == 2) %>% 
    #filter(idade > 50 & idade < 90)
    filter(idade < 90)
  
  D1$age <- cut(D1$idade, breaks = seq(0,80,10), include.lowest = T, right = F)
  #D1$age <- cut(D1$idade, breaks = seq(0,90,5), include.lowest = T, right = F)
  D1$age <- droplevels(D1$age)
  
  print("Preparing table")
  table_dose_count = D1 %>% count(data, age) %>% drop_na(age) %>% 
    group_by(age) %>% mutate(sum = cumsum(n)) %>% arrange(age)
  
  num_string <- substr(as.character(table_dose_count$age),2,3)
  num_string <- gsub(",","",num_string)
  table_dose_count$age_code <- as.numeric(num_string)
  
  age_state = distribuicao_etaria[,c("Idade",estado)]
  age_state$Idade <- as.numeric(age_state$Idade)
  
  proportion_table = table_dose_count %>% 
    left_join(age_state, by = c("age_code" = "Idade")) %>% 
    mutate(p = sum/PR)
  
  print("Plotting")
  gprop_d1 = ggplot(proportion_table, aes(x = data, y = p, color = age)) +
    geom_line() + theme_minimal() + xlab("") + ylab("") + ylim(0,1) + 
    scale_x_date(date_breaks = "1 month", 
                 labels=date_format("%b"),
                 limits = as.Date(c('2021-01-15',data_base)))
  
  rm(D1);gc()
  ### Second dose
  
  D2 <- tabela_vacinas %>% filter(doses %in% c("D2","2ª Dose"))
  D2 <- D2 %>% select(-id) %>% 
    #filter(categoria == 2) %>% 
    #filter(idade > 50 & idade < 90)
    filter(idade < 90)
  
  D2$age <- cut(D2$idade, breaks = seq(0,80,10), include.lowest = T, right = F)
  #D2$age <- cut(D2$idade, breaks = seq(0,90,5), include.lowest = T, right = F)
  D2$age <- droplevels(D2$age)
  
  print("Preparing table")
  table_dose_count = D2 %>% count(data, age) %>% drop_na(age) %>% group_by(age) %>% mutate(sum = cumsum(n)) %>% arrange(age)
  
  num_string <- substr(as.character(table_dose_count$age),2,3)
  num_string <- gsub(",","",num_string)
  table_dose_count$age_code <- as.numeric(num_string)
  
  age_state = eta[,c("Idade",estado)]
  age_state$Idade <- as.numeric(age_state$Idade)
  
  proportion_table = table_dose_count %>% left_join(age_state, by = c("age_code" = "Idade")) %>% mutate(p = sum/SC)
  
  gprop_d2 = ggplot(proportion_table, aes(x = data, y = p, color = age)) +
    geom_line() + theme_minimal() + xlab("") + ylab("") + ylim(0,1)  + 
    scale_x_date(date_breaks = "1 month", 
                 labels=date_format("%b"),
                 limits = as.Date(c('2021-01-15',data_base)))
  
  if(write){
    ggsave(paste0(output_folder,"gprop_",estado,"_all_ages_d1.png"), plot = gprop_d1, dpi = 300, height = 6, width = 8)
    ggsave(paste0(output_folder,"gprop_",estado,"_all_ages_d2.png"), plot = gprop_d2, dpi = 300, height = 6, width = 8)
  }
  
  if(return){
    result.plots <- list(D1 = gprop_d1, D2 = gprop_d2)
    return(result.plots)
  }
}

################################################################################
## Parsing command line arguments
################################################################################
if (sys.nframe() == 0L) {
  option_list <- list(
    make_option("--command",
                help = ("Comando a ser feito: prepara_dado | prepara_cobertura"),
                metavar = "command"),
    make_option("--split", default = "FALSE",
                help = ("Booleano. Dados são quebrados em partes?"),
                metavar = "split"),
    make_option("--dataBase",
                help = ("Data da base de dados, formato 'yyyy-mm-dd'"),
                metavar = "dataBase"),
    make_option("--estado",
                help = ("Sigla do estado"),
                metavar = "estado"),
    make_option("--input_folder", default = "dados/",
                help = ("Pasta de dados de entrada"),
                metavar = "input_folder"),
    make_option("--output_folder", default = "output/",
                help = ("Pasta de dados de saída"),
                metavar = "output_folder")
  )
  parser_object <- OptionParser(usage = "Rscript %prog --command comando --estado UF --dataBase yyyy-mm-dd --split TRUE|FALSE \n",
                                option_list = option_list,
                                description = "Script para processar banco de dados de vacinção do SI-PNI.")

  ## TO TEST INTERACTIVELY the command-line arguments
  #input <- " --command prepara_dado --estado AC --dataBase 2022-01-26"
  #command.args <- strsplit(input, " ")[[1]]
  #opt <- parse_args(parser_object, args = command.args, positional_arguments = TRUE)
  ## SKIP opt line below

  opt <- parse_args(parser_object, args = commandArgs(trailingOnly = TRUE),
                    positional_arguments = TRUE)
  ## aliases
  command <- opt$options$command
  estado <- opt$options$estado
  split <- opt$options$split
  dataBase <- opt$options$dataBase
  input_folder <- opt$options$input_folder
  output_folder <- opt$options$output_folder

  if (length(command) == 0 || length(estado) == 0 || length(dataBase) == 0) {
    print("Argumento de entrada faltando! Saindo...")
    quit(save = "no", status = 1)
  }

  # quit on error when run non-interactively #small change because this is killing my local sessions T_T
  if (!interactive()) options(error = function() quit(save = "no", status = 1))

  ### roda comando
  if (command == "prepara_dado") {
    prepare_table(estado, data_base = dataBase, split = split,
                  input_folder = input_folder, output_folder = output_folder)
  } else if (command == "prepara_cobertura") {
    prepara_historico(estado, data_base = dataBase, split = split,
                  input_folder = output_folder, output_folder = output_folder)
    join_historico(estado, data_base = dataBase,
                  input_folder = output_folder, output_folder = output_folder)
  } else {
    print(paste("Comando", command, "não encontrado."))
  }
}

