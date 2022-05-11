##############################
## Packages
##############################

if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}

##############################
### Auxiliary functions
##############################

beginning.of.month <- function(x) {
  substr(x,9,10) <- "01"
  return(x)
}

end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}

#
`%orNA%` <- function(A,B){
  A[is.na(A)] <- FALSE
  B[is.na(B)] <- FALSE
  C <- return(A < B)
}

#
`%leNAF%` <- function(A,B){
  ifelse(is.na(A) | is.na(B), FALSE, A <= B)
}

#
`%leNAT%` <- function(A,B){
  ifelse(is.na(A) | is.na(B), TRUE, A <= B)
}

`%geNAF%` <- function(A,B){
  ifelse(is.na(A) | is.na(B), FALSE, A >= B)
}

`%mNA%` <- function(A,B){
  A[is.na(A)] <- "5001-01-01"
  B[is.na(B)] <- "5001-01-01"
  C <- return(A < B)
}

#
which_min <- function(x) {
  y = c(x[2],x[3],x[4],x[5]) - x[1]
  return(order(y)[1])
}

which_order0 <- function(x) {
  y = c(x[1],x[2],x[3])
  return(substr(paste0(order(y),collapse = ""), start = 1, stop = 3 - sum(is.na(y))))
}

which_order <- function(x) {
  y = c(x[1],x[2],x[3],x[4],x[5])
  return(substr(paste0(order(y),collapse = ""), start = 1, stop = 5 - sum(is.na(y))))
}

which_order2 <- function(x) {
  y = c(x[1],x[2],x[3],x[4],x[5],x[6],x[7])
  return(substr(paste0(order(y),collapse = ""), start = 1, stop = 7 - sum(is.na(y))))
}

sort_rows <- function(x) {
  y = c(sort(x),rep(NA,sum(is.na(x))))
  return(y)
}

which_min <- function(x) {
  y = which(x == min(x, na.rm = T))[1]
  return(y)
}

find_duplicated_record <- function(x, threshold = 10) {
  
  if(sum(is.na(x))>=7) {
    
    return(FALSE)
    
    } else {
      
  y = c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8])
  y = y[!is.na(y)]
  
  difs = c()
  for(j in 1:(length(y)-1)) difs = c(difs, abs(y[j] - y[ (j+1):length(y) ]))
    
  return(any(difs<=threshold))
  
    }
}

###############
### Script
###############

#### Define objects
first.day <- as.Date("2021-01-01")
data_base <- list.files("dados/") %>% 
  grep("^dados_.*.csv", ., value = T) %>%
  substr(7,16) %>%
  as.Date() %>%
  max(na.rm = T)

#### Run script to compute dose coverage

ini = Sys.time()

files <- list.files("output/wide")[!grepl("[1-9].csv", list.files("output/wide"))]

#da_order <- data.frame()
#da_dupli <- data.frame()

da_month <- data.frame()
da_week <- data.frame()

for(i in files) {
  state = substr(i,22,23)
  print(state)
  
  # Load table for each state in wide format
  
  df <- data.frame(fread(paste0("output/wide/",i)))  %>%
    filter(agegroup != 1)
  
  if(!any(grepl("3",colnames(df)))) df$data_3 <- NA
  if(!any(grepl("4",colnames(df)))) df$data_4 <- NA
  if(!any(grepl("5",colnames(df)))) df$data_5 <- NA
  if(!any(grepl("DA",colnames(df)))) df$data_DA <- NA
  
  df <- df %>%
          mutate_at(vars(contains('data_')), ~as.Date(.))
  
  df[df==""] <- NA
  df$agegroup <- factor(df$agegroup, levels = c(1:11))
  
  # Compute time difference in days between each dose type to reference date
  df <- df %>% mutate(dif1 = as.numeric(data_D1 - first.day),
                      dif2 = as.numeric(data_D2 - first.day),
                       
                      difA = as.numeric(data_DA - first.day),
                      difR = as.numeric(data_R - first.day),
                      dif3 = as.numeric(data_3 - first.day),
                      dif4 = as.numeric(data_4 - first.day),
                      dif5 = as.numeric(data_5 - first.day),
                      
                      difU = as.numeric(data_D - first.day))
  
  df2 <- df[,c("dif1","dif2","difA","difR","dif3","dif4","dif5","difU")]
  
  # Identificação dos registros em que a primeira dose é Janssen
  
  min_dif <- apply(df2, 1, which_min)
  
  J <- factor((min_dif == 1 & df$vacina_D1 == "Janssen" |
               min_dif == 2 & df$vacina_D2 == "Janssen" |
               min_dif == 8 & df$vacina_D  == "Janssen" ),
               levels = c(T,F),
               labels = c("J","NJ")) # Janssen, Not Janssen
  
  # # Compute dose's date sequence
  # df$next_order = apply(df2, 1, which_order2)
  # 
  # # Find vaccination records with less than 15 days of difference
  # df$dupli = apply(df2, 1, find_duplicated_record)
  # 
  # da_dupli <- bind_rows(da_dupli, data.frame(dupli = sum(df$dupli), UF = state))
  # 
  # df <- df %>% filter(dupli) %>% select(-dupli)
  # 
  # # Compute the frequency of the dose's date sequence
  # da_order_uf <- df %>% count(agegroup, next_order) %>% mutate(UF = state)
  # da_order <- bind_rows(da_order, da_order_uf)
  
  ###
  #df2 = t(df[,c("data_D1","data_D2","data_DA","data_R","data_3","data_4","data_D")]) %>% data.frame()
  df3 = apply(df2,1,sort_rows) %>% t() %>% data.frame()
  
  D1 = data.frame(date = df3$X1 + as.Date("2021-01-01"), 
                  agegroup = df$agegroup, 
                  janssen = J) %>% 
          drop_na(date, agegroup, janssen) %>%
          count(date, agegroup, janssen) %>%
          complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"), agegroup, janssen,
                   fill = list(n = 0)) %>%
    mutate(dose = "a")
  
  
  D2 = data.frame(date = df3$X2 + as.Date("2021-01-01"), 
                  agegroup = df$agegroup, 
                  janssen = J) %>% 
    drop_na(date, agegroup, janssen) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "b")
  
  D3 = data.frame(date = df3$X3 + as.Date("2021-01-01"), 
                  agegroup = df$agegroup, 
                  janssen = J) %>% 
    drop_na(date, agegroup, janssen) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "c")
  
  D4 = data.frame(date = df3$X4 + as.Date("2021-01-01"), 
                  agegroup = df$agegroup, 
                  janssen = J) %>% 
    drop_na(date, agegroup, janssen) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "d")
  
  D5 = data.frame(date = df3$X5 + as.Date("2021-01-01"), 
                  agegroup = df$agegroup, 
                  janssen = J) %>% 
    drop_na(date, agegroup, janssen) %>%
    count(date, agegroup, janssen) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"), agegroup, janssen,
             fill = list(n = 0)) %>%
    mutate(dose = "e")
  
  df_doses <- bind_rows(D1,D2,D3,D4,D5) %>%
    mutate(dose = factor(dose))
  
  rm(df3, D1, D2, D3, D4, D5);gc()
  
  df_month <- df_doses %>%
    mutate(month = as.Date(beginning.of.month(as.character(date)))) %>%
    group_by(month, agegroup, dose, janssen) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(dose_vaccine = paste0(dose,janssen)) %>%
    select(-dose,-janssen) %>%
    spread(key = dose_vaccine, value = total) %>%
    complete(month = seq.Date(min(month), as.Date(beginning.of.month(as.character(data_base))), by="month"), agegroup,
             fill = list(aJ = 0, bJ = 0, cJ = 0, dJ = 0, eJ = 0,
                         aNJ = 0, bNJ = 0, cNJ = 0, dNJ = 0, eNJ = 0)) %>%
    mutate(month = as.Date(month)) %>%
    distinct() %>%
    mutate(aJ = aJ - bJ,
           bJ = bJ - cJ,
           cJ = cJ - dJ,
           dJ = dJ - eJ,
           
           aNJ = aNJ - bNJ,
           bNJ = bNJ - cNJ,
           cNJ = cNJ - dNJ,
           dNJ = dNJ - eNJ) %>%
    
    group_by(agegroup) %>%
    
    mutate(acNJ = cumsum(aNJ),
           bcNJ = cumsum(bNJ),
           ccNJ = cumsum(cNJ),
           dcNJ = cumsum(dNJ),
           ecNJ = cumsum(eNJ),
           
           acJ = cumsum(aJ),
           bcJ = cumsum(bJ),
           ccJ = cumsum(cJ),
           dcJ = cumsum(dJ),
           ecJ = cumsum(eJ)) %>%
    
    gather(key = "dose", value = "n", -month, -agegroup)  %>%
    mutate(first_dose = factor(!grepl("N", dose), 
                               levels = c(T,F), 
                               labels = c("Janssen","Other")),
           dose = gsub("J","",dose),
           dose = gsub("N","",dose),
           dose = factor(dose,
                         levels = c("a", "ac", "b", "bc", "c", "cc", "d", "dc", "e", "ec"),
                         labels = c("D1", "D1cum", "D2", "D2cum", "D3", "D3cum", "D4", "D4cum", "D5", "D5cum")),
           UF = state)

  ########

  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(date)) %>%
    group_by(week, agegroup, dose, janssen) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    mutate(dose_vaccine = paste0(dose,janssen)) %>%
    select(-dose,-janssen) %>%
    spread(key = dose_vaccine, value = total) %>%
    complete(week = seq.Date(min(week), end.of.epiweek(data_base), by="week"), agegroup,
             fill = list(aJ = 0, bJ = 0, cJ = 0, dJ = 0, eJ = 0,
                         aNJ = 0, bNJ = 0, cNJ = 0, dNJ = 0, eNJ = 0)) %>%
    mutate(week = as.Date(week)) %>%
    distinct() %>%
    mutate(aJ = aJ - bJ,
           bJ = bJ - cJ,
           cJ = cJ - dJ,
           dJ = dJ - eJ,
           
           aNJ = aNJ - bNJ,
           bNJ = bNJ - cNJ,
           cNJ = cNJ - dNJ,
           dNJ = dNJ - eNJ) %>%
    
    group_by(agegroup) %>%
    
    mutate(acNJ = cumsum(aNJ),
           bcNJ = cumsum(bNJ),
           ccNJ = cumsum(cNJ),
           dcNJ = cumsum(dNJ),
           ecNJ = cumsum(eNJ),
           
           acJ = cumsum(aJ),
           bcJ = cumsum(bJ),
           ccJ = cumsum(cJ),
           dcJ = cumsum(dJ),
           ecJ = cumsum(eJ)) %>%
    
    gather(key = "dose", value = "n", -week, -agegroup)  %>%
    mutate(first_dose = factor(!grepl("N", dose), 
                               levels = c(T,F), 
                               labels = c("Janssen","Other")),
           dose = gsub("J","",dose),
           dose = gsub("N","",dose),
           dose = factor(dose,
                         levels = c("a", "ac", "b", "bc", "c", "cc", "d", "dc", "e", "ec"),
                         labels = c("D1", "D1cum", "D2", "D2cum", "D3", "D3cum", "D4", "D4cum", "D5", "D5cum")),
           UF = state)
  
   rm(df_doses); gc()
  
  da_month <- bind_rows(da_month, df_month)  
  da_week <- bind_rows(da_week, df_week)  
}

da_month <- da_month %>%
  mutate(month = as.Date(beginning.of.month(as.character(month + 32)))) %>%
  drop_na(month, agegroup) %>%
  filter(agegroup != 1)

da_week <- da_week %>%
  drop_na(week, agegroup) %>%
  filter(agegroup != 1)

fwrite(da_month, file = "output/doses_cobertura_proporcao_mes_ordem.csv")
fwrite(da_week, file = "output/doses_cobertura_proporcao_semana_ordem.csv")

fin = Sys.time()
fin - ini

################
### Plots
###############

gage_month <- da_month %>% 
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 2:11,
                           labels = c("5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = month, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                    labels = c("5ª dose",
                               "4ª dose",
                               "3ª dose",
                               "2ª dose",
                               "1ª dose"))

  gage_week <- da_week %>% 
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 2:11,
                           labels = c("5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = week, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme_set(theme_gray(base_size = 30)) +
  xlab("")  + ylab("") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))
  
ggsave(gage_month, file = "figuras/aplicacao_doses_mes_ordem.png", width = 24, height = 12)
ggsave(gage_week, file = "figuras/aplicacao_doses_semana_ordem.png", width = 24, height = 12)

#
guf_month <- da_month %>%
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 2:11,
                           labels = c("5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = month, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme_set(theme_gray(base_size = 30)) +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))

guf_week <- da_week %>%
  filter(!dose %in% c("D1","D2","D3","D4","D5")) %>%
  mutate(dose = factor(dose,
                       levels = c("D5cum","D4cum","D3cum","D2cum","D1cum"),
                       ordered = T),
         agegroup = factor(agegroup, levels = 2:11,
                           labels = c("5 a 11",
                                      "12 a 17",
                                      "18 a 29",
                                      "30 a 39",
                                      "40 a 49",
                                      "50 a 59",
                                      "60 a 69",
                                      "70 a 79",
                                      "80 a 89",
                                      "90+"))) %>%
  ggplot(aes(x = week, y = n, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  theme_set(theme_gray(base_size = 30)) +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_viridis_d("Dose",
                       labels = c("5ª dose",
                                  "4ª dose",
                                  "3ª dose",
                                  "2ª dose",
                                  "1ª dose"))

ggsave(guf_month, file = "figuras/aplicacao_doses_uf_mes_ordem.png", width = 24, height = 12)
ggsave(guf_week, file = "figuras/aplicacao_doses_uf_semana_ordem.png", width = 24, height = 12)



