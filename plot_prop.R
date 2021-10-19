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



dt_base = "2021-08-22"

#eta <- read.csv("dados/DistrEtaria2020.csv")
eta <- read.csv("dados/Idade202_ibge.csv")

plot_proportion <- function(estado, distribuicao_etaria, 
                            input_folder = "output/",
                            output_folder = "output/",
                            write = TRUE,
                            return = TRUE,
                            data_base = '2021-08-03'){

  
  tabela_vacinas <- fread(paste0(input_folder,estado, "_PNI_clean.csv"), 
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
  
  age_state = distribuicao_etaria[,c("Idade",estado)]
  age_state$Idade <- as.numeric(age_state$Idade)
  colnames(age_state)[2] = "State"
  
  proportion_table = table_dose_count %>% 
    left_join(age_state, by = c("age_code" = "Idade")) %>% 
    mutate(p = sum/State)
  
  print("Plotting")
  gprop_d1 = ggplot(proportion_table, aes(x = data, y = p, color = age)) +
    geom_line() + theme_minimal() + xlab("") + ylab("") + ylim(0,1.5) + 
    scale_x_date(date_breaks = "1 month", 
                 labels=date_format("%b"),
                 limits = as.Date(c('2021-01-15',data_base))) +
  scale_color_viridis(discrete = T, direction = -1)
  #gprop_d1
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
  
  num_string <- substr(as.character(table_dose_count$age),2,3)
  num_string <- gsub(",","",num_string)
  table_dose_count$age_code <- as.numeric(num_string)
  
  age_state = eta[,c("Idade",estado)]
  age_state$Idade <- as.numeric(age_state$Idade)
  colnames(age_state)[2] = "State"
  proportion_table = table_dose_count %>% left_join(age_state, by = c("age_code" = "Idade")) %>% mutate(p = sum/State)
  
  gprop_d2 = ggplot(proportion_table, aes(x = data, y = p, color = age)) +
    geom_line() + theme_minimal() + xlab("") + ylab("") + ylim(0,1.5)  + 
    scale_x_date(date_breaks = "1 month", 
                 labels=date_format("%b"),
                 limits = as.Date(c('2021-01-15',data_base))) +
    scale_color_viridis(discrete = T, direction = -1)
  
  gfin <- gprop_d1  + theme(legend.position = "none") + ggtitle(paste0(estado, ": D1"))| gprop_d2 + ggtitle("D2")
  
  if(write){
    ggsave(paste0(output_folder,"gprop_",estado,"_all_ages.png"), plot = gfin, dpi = 300, height = 6, width = 12)
  }
  
  if(return){
    result.plots <- gfin
    return(result.plots)
  }
}

estados_br <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
for(est in estados_br){
plot_proportion(est, eta, input_folder = "./output/", output_folder = "./figuras/", return = F, data_base = dt_base)
}

print("Done.")