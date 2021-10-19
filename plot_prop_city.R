#setwd("C:/Users/morde/OneDrive/RWorkspace/Covid19/PNI")
source("vaccine_functions.R")

if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(ggcorrplot)){install.packages("ggcorrplot"); library(ggcorrplot)}
if(!require(RColorBrewer)){install.packages("RColorBrewer"); library(RColorBrewer)}
if(!require(gridExtra)){install.packages("gridExtra"); library(gridExtra)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(zoo)){install.packages("zoo"); library(zoo)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(scales)){install.packages("scales"); library(scales)}
if(!require(stringi)){install.packages("stringi"); library(stringi)}

#source("C:/Users/morde/OneDrive/RWorkspace/Covid19/pega_pop_datasus_fx_regiao (1).R")
#pop = tabnet_pop()
load("pop.Rdata")
print(17)
cidade = "São Paulo"
codigo_cidade = as.numeric(pop[which(pop$nome_regiao == cidade),"codreg"] %>% unlist())
print(20)
pop2 <- pop %>% select(-ano, -sexo) %>% 
  mutate(a0 = FX_0_a_4 + FX_5_a_9,
         a10 = FX_10_a_14 + FX_15_a_19) %>%
  filter(nome_regiao == cidade)
print(25)
pop3 <- pop2 %>% select(-codreg, -nome_regiao) %>% as.matrix() %>% t()
pop3 <- data.frame(pop3[c(13,14,5:11),])
colnames(pop3) = "pop"
rownames(pop3) = NULL
pop3$Idade <- seq(0,80,10)
#barplot(pop3$pop)
####
#eta <- read.csv("dados/Idade202_ibge.csv")
print(34)
prepare_table <- function(estado, write.dose.types = TRUE, 
                          input_folder = "dados/",
                          output_folder = "output/",
                          data_base = "2021-08-22",
                          codigo_cidade,
                          cidade, ...) {
  
  todas_vacinas <- fread(paste0(input_folder,"dados_",data_base,"_",estado,".csv"), 
                         select = c("paciente_id", "paciente_datanascimento", 
                                    "vacina_categoria_codigo", "vacina_dataaplicacao", 
                                    "vacina_descricao_dose", "vacina_codigo",
                                    "paciente_endereco_coibgemunicipio",
                                    "estabelecimento_municipio_codigo",
                                    "estabelecimento_municipio_nome"),
                         colClasses = c("paciente_id" = "factor",
                                        "paciente_datanascimento" = "Date",
                                        "vacina_categoria_codigo" = "integer",
                                        "vacina_dataaplicacao" = "Date",
                                        "vacina_descricao_dose" = "character",
                                        "vacina_codigo" = "integer"), 
                         encoding = "UTF-8")
  
  print("Data succesfully loaded")
  print("Preparing data... 1")
  
  todas_vacinas = todas_vacinas %>% filter(estabelecimento_municipio_codigo == codigo_cidade)
  
  #w = table(flo$paciente_endereco_coibgemunicipio) > 500
  #round(sort(table(flo$paciente_endereco_coibgemunicipio)[w])/length(flo$paciente_endereco_coibgemunicipio) * 100, 1)
  
  todas_vacinas$vacina_codigo[todas_vacinas$vacina_codigo == 89] <- 85
  
  todas_vacinas <- todas_vacinas %>% 
    distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE) %>% 
    distinct(paciente_id,vacina_dataaplicacao,.keep_all = TRUE)
  
  #if(write.dose.types) {
  #  write.csv(data.frame(table(todas_vacinas$vacina_descricao_dose)), 
  #            file = paste0(output_folder,estado,"_dose_types_log.csv"), quote = FALSE, row.names = FALSE)
  #}
  
  # Substitute Dose names and transform into factor
  #dose_types <- sort(unique(todas_vacinas$vacina_descricao_dose))
  load("nomes_doses.Rdata")
  todas_vacinas$vacina_descricao_dose[grep(nomes_doses[1],todas_vacinas$vacina_descricao_dose)] <- "D1"
  todas_vacinas$vacina_descricao_dose[grep(nomes_doses[2],todas_vacinas$vacina_descricao_dose)] <- "D2"
  
  todas_vacinas$doses <- factor(todas_vacinas$vacina_descricao_dose)
  
  todas_vacinas <- todas_vacinas %>% filter(vacina_dataaplicacao < as.Date(data_base) & vacina_dataaplicacao > as.Date("2021-01-17"))
  todas_vacinas <- todas_vacinas %>% drop_na(-vacina_categoria_codigo)
  todas_vacinas <- todas_vacinas %>% filter(paciente_datanascimento > "1900-01-01")
  
    ##
  print("Preparing data... 2")
  
  ##########################
  
  tabela_vacinas <- todas_vacinas %>% filter(vacina_codigo %in% c(85,86,87)) %>% 
    select(-vacina_descricao_dose, -paciente_endereco_coibgemunicipio,
           -estabelecimento_municipio_codigo
           ) %>% as.data.frame()
  colnames(tabela_vacinas) <- c("id", "nasc", "categoria", "data","vacina","municipio","doses")
  
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
  tabela_vacinas$complete <- tabela_vacinas$data + 14 < as.Date(data_base) & tabela_vacinas$doses == levels(tabela_vacinas$doses)[2]
  
  print("Saving data")
  cidade_nome = stri_trans_general(tolower(gsub(" ", "_", cidade)), id = "Latin-ASCII")
  write.csv(tabela_vacinas, file = paste0(output_folder, cidade_nome, "_PNI_clean.csv"), row.names = FALSE, quote = FALSE)
}

print(122)

prepare_table(estado = "SP", data_base = "2021-08-22", codigo_cidade = codigo_cidade, 
              cidade = "São Paulo")

plot_proportion_city <- function(cidade, distribuicao_etaria, 
                            input_folder = "output/",
                            output_folder = "output/",
                            write = TRUE,
                            return = TRUE,
                            data_base = '2021-08-22'){
  
  tabela_vacinas <- fread(paste0(input_folder,cidade, "_PNI_clean.csv"), 
                          colClasses = c("id" = "factor", "categoria" = "integer", 
                                         "data" = "Date", "vacina" = "integer", "n" = "integer",
                                         "doses" = "factor","idade" = "integer"))
  print("Data succesfully loaded")
  
  ##########################
  
  D1 <- tabela_vacinas %>% filter(doses == "D1")
  D1 <- D1 %>% select(-id) %>% 
    #filter(categoria == 2) %>% 
    #filter(idade > 50 & idade < 90)
    filter(idade < 120)
  seq_break <- seq(0,90,10)
  seq_break[10] <- 120
  D1$age <- cut(D1$idade, breaks = seq_break, include.lowest = T, right = F)
  #D1$age <- cut(D1$idade, breaks = seq(0,90,5), include.lowest = T, right = F)
  D1$age <- droplevels(D1$age)
  
  print("Preparing table")
  table_dose_count = D1 %>% count(data, age) %>% drop_na(age) %>% 
    group_by(age) %>% mutate(sum = cumsum(n)) %>% arrange(age)

  num_string <- substr(as.character(table_dose_count$age),2,3)
  num_string <- gsub(",","",num_string)
  table_dose_count$age_code <- as.numeric(num_string)
  
  age_state = distribuicao_etaria[,c("Idade","pop")]
  age_state$Idade <- as.numeric(age_state$Idade)
  colnames(age_state)[2] = "State"
  
  proportion_table = table_dose_count %>% 
    left_join(age_state, by = c("age_code" = "Idade")) %>% 
    mutate(p = sum/State)
  
  print("Plotting")
  gprop_d1 = ggplot(proportion_table, aes(x = data, y = p, color = age)) +
    geom_line() + theme_minimal() + xlab("") + ylab("") + ylim(0,1) + 
    scale_x_date(date_breaks = "1 month", 
                 labels=date_format("%b"),
                 limits = as.Date(c('2021-01-15',data_base))) +
    scale_color_viridis(discrete = T, direction = -1)
  
  rm(D1);gc()
  ### Second dose
  
  D2 <- tabela_vacinas %>% filter(doses == "D2")
  D2 <- D2 %>% select(-id) %>% 
    #filter(categoria == 2) %>% 
    #filter(idade > 50 & idade < 90)
    filter(idade < 120)
  
  D2$age <- cut(D2$idade, breaks = seq_break, include.lowest = T, right = F)
  #D2$age <- cut(D2$idade, breaks = seq(0,80,10), include.lowest = T, right = F)
  D2$age <- droplevels(D2$age)
  
  print("Preparing table")
  table_dose_count = D2 %>% count(data, age) %>% drop_na(age) %>% group_by(age) %>% mutate(sum = cumsum(n)) %>% arrange(age)
 # table_dose_count = D2 %>% count(age) %>% drop_na(age) %>% group_by(age) %>% mutate(sum = cumsum(n)) %>% arrange(age)
  num_string <- substr(as.character(table_dose_count$age),2,3)
  num_string <- gsub(",","",num_string)
  table_dose_count$age_code <- as.numeric(num_string)
  
  age_state = distribuicao_etaria[,c("Idade","pop")]
  age_state$Idade <- as.numeric(age_state$Idade)
  colnames(age_state)[2] = "State"
  proportion_table = table_dose_count %>% left_join(age_state, by = c("age_code" = "Idade")) %>% mutate(p = sum/State)
  
  
  #gd2 = ggplot(proportion_table, aes(x = age, y = p)) + geom_bar(stat = "identity", fill = "steelblue") + xlab("") + ylab("") +
  #  theme_minimal() + ggtitle("Florianópolis, SC")
  
  gprop_d2 = ggplot(proportion_table, aes(x = data, y = p, color = age)) +
    geom_line() + theme_minimal() + xlab("") + ylab("") + ylim(0,1)  + 
    scale_x_date(date_breaks = "1 month", 
                 labels=date_format("%b"),
                 limits = as.Date(c('2021-01-15',data_base))) +
    scale_color_viridis(discrete = T, direction = -1)
  
  gfin <- gprop_d1  + theme(legend.position = "none") + ggtitle(paste0(estado, ": D1"))| gprop_d2 + ggtitle("D2")
  
  #ggsave("d2_florianopolis.png", plot = gd2, dpi = 300, height = 6, width = 12)
  
  if(write){
    ggsave(paste0(output_folder,"gprop_",cidade,"_all_ages.png"), plot = gfin, dpi = 300, height = 6, width = 12)
  }
  
  if(return){
    result.plots <- gfin
    return(result.plots)
  }
}

plot_proportion_city <- function("sao_paulo", distribuicao_etaria = pop3)
print("Done.")