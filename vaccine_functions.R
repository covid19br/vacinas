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

prepare_table <- function(estado, write.dose.types = TRUE, 
                          input_folder = "dados/",
                          output_folder = "output/",
                          data_base = "2021-07-15") {
  
  todas_vacinas <- fread(paste0(input_folder,data_base,"_",estado,".csv"), 
                         select = c("paciente_id", "paciente_datanascimento", 
                                    "vacina_categoria_codigo", "vacina_dataaplicacao", 
                                    "vacina_descricao_dose", "vacina_codigo"),
                         colClasses = c("paciente_id" = "factor",
                                        "paciente_datanascimento" = "Date",
                                        "vacina_categoria_codigo" = "integer",
                                        "vacina_dataaplicacao" = "Date",
                                        "vacina_descricao_dose" = "character",
                                        "vacina_codigo" = "integer"))
  
  print("Data succesfully loaded")
  print("Preparing data... 1")
  
  todas_vacinas <- todas_vacinas %>% 
    distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE) %>% 
    distinct(paciente_id,vacina_dataaplicacao,.keep_all = TRUE)
  
  if(write.dose.types) {
    write.csv(data.frame(table(todas_vacinas$vacina_descricao_dose)), 
              file = paste0(output_folder,estado,"_dose_types_log.csv"), quote = FALSE, row.names = FALSE)
  }
  
  # Substitute Dose names and transform into factor
  todas_vacinas$vacina_descricao_dose[grep(1,todas_vacinas$vacina_descricao_dose)] <- "D1"
  todas_vacinas$vacina_descricao_dose[grep(2,todas_vacinas$vacina_descricao_dose)] <- "D2"
  
  todas_vacinas$doses <- factor(todas_vacinas$vacina_descricao_dose)
  
  ##
  print("Preparing data... 2")
  
  ##########################
  
  tabela_vacinas <- todas_vacinas %>% filter(vacina_codigo %in% c(85,86,87)) %>% select(-vacina_descricao_dose) %>% as.data.frame()
  colnames(tabela_vacinas) <- c("id", "nasc", "categoria", "data","vacina","doses")
  
  nreg <- tabela_vacinas %>% select(id) %>% group_by(id) %>% count() # Contar frequencia registros_id
  tabela_vacinas <- tabela_vacinas %>% left_join(nreg, by = "id") %>% filter(n < 3) %>% mutate(id = factor(id)) # Cortar ids com mais de 2 registros
  
  rm(nreg);gc()
  
  # Filter if it has more than one D1 or D2 per ID
  remove_ids <- tabela_vacinas %>% group_by(id, doses, .drop = FALSE) %>% summarise(m = n()) %>% filter(m > 1)
  tabela_vacinas = tabela_vacinas %>% filter(!(id %in% remove_ids$id)) %>% mutate(id = droplevels(id))
  
  rm(remove_ids);gc()
  
  age <- tabela_vacinas %>%
    filter(doses == "D1") %>% mutate(idade = floor(time_length(data - nasc, "years"))) %>%
    select(id,idade)
  tabela_vacinas <- tabela_vacinas %>% left_join(age, by = "id") %>% select(-nasc)
  
  print("Saving data")
  
  write.csv(tabela_vacinas, file = paste0(output_folder, estado, "_PNI_clean.csv"), row.names = FALSE, quote = FALSE)
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

prepara_historico <- function(estado, 
                              data_base = as.Date("2021-07-15"),
                              input_folder = "output/",
                              output_folder = "output/") {
  
  if(class(data_base) == "character") data_base <- as.Date(data_base)
  
  tabela_vacinas <- fread(paste0(input_folder,estado, "_PNI_clean.csv"), 
                          colClasses = c("id" = "factor", "categoria" = "integer", 
                                         "data" = "Date", "vacina" = "integer", "n" = "integer",
                                         "doses" = "factor","idade" = "integer"))
  
  tabela_D1 <- tabela_vacinas %>% select(-id,-categoria) %>% 
    filter(doses == "D1" & n == 1) %>%
    filter(data > "2021-01-01") %>%
    drop_na(vacina, idade, data, doses) %>%
    mutate(tempo_doses = as.numeric(data_base - data),
           vacina = factor(vacina, levels = c(85,86,87), labels = c("AZ","Coronavac","Pfizer")),
           agegroup = cut(idade, 
                          breaks = c(seq(0,90,10),Inf), 
                          include.lowest = T, 
                          right = F, 
                          labels = F)) %>%
    mutate(agegroup = factor(agegroup),
           tempo_doses = factor(tempo_doses)) %>%
    count(vacina, agegroup,tempo_doses, .drop = FALSE)
  
  write.csv(tabela_D1, file= paste0(output_folder,"historico_D1_",estado,".csv"), row.names = FALSE, quote = FALSE)
  
  tabela_D2 <- tabela_vacinas %>% select(-id,-categoria) %>% 
    filter(doses == "D2") %>%
    filter(data > "2021-01-01") %>%
    drop_na(vacina, idade, data, doses) %>%
    mutate(vacina = factor(vacina, levels = c(85,86,87), labels = c("AZ","Coronavac","Pfizer")),
           agegroup = cut(idade, 
                          breaks = c(seq(0,90,10),Inf), 
                          include.lowest = T, 
                          right = F, 
                          labels = F)) %>%
    count(vacina, agegroup, .drop = FALSE)
  
  write.csv(tabela_D2, file= paste0(output_folder,"historico_D2_",estado,".csv"), row.names = FALSE, quote = FALSE)
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
  age_labels <-  levels(cut(1:100, breaks = c(seq(0,90,10),Inf), 
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
                            data_base = '2021-07-15'){
  
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
  
  D1$age <- cut(D1$idade, breaks = seq(0,90,5), include.lowest = T, right = F)
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
  
  D2 <- tabela_vacinas %>% filter(doses == "D2")
  D2 <- D2 %>% select(-id) %>% 
    #filter(categoria == 2) %>% 
    #filter(idade > 50 & idade < 90)
    filter(idade < 90)
  
  D2$age <- cut(D2$idade, breaks = seq(0,90,5), include.lowest = T, right = F)
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


