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

###############
### Script
###############

#### Define objects
pal = wes_palette(n = 5, name = "Zissou1", type = "discrete")
first.day <- as.Date("2021-01-01")

#### Run script to compute dose coverage

ini = Sys.time()
  
files <- list.files("output/wide")[!grepl("[1-9].csv", list.files("output/wide"))]

da_month <- data.frame()
da_week <- data.frame()
da_order <- data.frame()

for(i in files) {
  state = substr(i,22,23)
  print(state)
  
  # Load table for each state in wide format
  
  df <- data.frame(fread(paste0("output/wide/",i),
                         select = c("data_D1",
                                    "data_D2","data_D","agegroup"),
                         colClasses = c("data_D1" = "Date",
                                        "data_D2" = "Date",
                                        "data_D" = "Date",
                                        "data_DA" = "Date")))

  df[df==""] <- NA
  df$agegroup <- factor(df$agegroup, levels = c(1:11))
  
  # Compute time difference in days between each dose type to reference date
  df2 <- df %>% mutate(dif1 = as.numeric(data_D1 - first.day),
                       dif2 = as.numeric(data_D2 - first.day),
                       difU = as.numeric(data_D - first.day)) %>%
                filter(!(dif2 %leNAF% dif1))

  # Compute dose's date sequence
  df2$next_order = apply(df2[,c("dif1","dif2","difU")], 1, which_order0)

  # Compute the frequency of the dose's date sequence
  da_order_uf <- df2 %>% count(next_order) %>% mutate(UF = state)
  da_order <- bind_rows(da_order, da_order_uf)
  
  # Dates from D1
  df2$dose <- NA
  df2$dose[df2$next_order %in% c(1,12,123)] <- "D1"
  df2$dose[df2$next_order %in% c(3,31,32,312,321) ] <- "D"
  
  df2$data <- NA
  df2$data <- df2$data_D1
  df2$data[which(df2$dose == "D")] <- df2$data_D[which(df2$dose == "D")]
    
  df_d1 <- df2 %>% 
            drop_na(dose) %>% 
            count(data, agegroup, dose)
  
  # Dates from D2
  d2_code_a <- c(12,123)
  d2_code_b <- c(13, 132)
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% d2_code_a] <- df2$data_D2[df2$next_order %in% d2_code_a]
  df2$data[which(df2$next_order %in% d2_code_b)] <- df2$data_D[which(df2$next_order %in% d2_code_b)]
  
  df_d2 <- df2 %>% drop_na(data) %>% count(data, agegroup) %>% mutate(dose = "D2")
  
  # Dates from D2 (D1 not present in the data base)
  d2_code_c <- c(2,23)
  
  df2$data <- NA
  class(df2$data) <- "Date"
  df2$data[df2$next_order %in% d2_code_c] <- df2$data_D2[df2$next_order %in% d2_code_c]
  
  df_d3 <- df2 %>% 
            drop_na(data) %>% 
            count(data, agegroup) %>% 
            mutate(dose = "D2f")
  
  # Clear cache
  rm(df,df2);gc()
  
  # Merge all tables
  df_doses <- full_join(df_d1, 
                   df_d2, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never") %>%
         full_join(df_d3, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")    
  
  # Compute the population's dose coverage by month
  
  df_month <- df_doses %>%
              mutate(month = as.Date(beginning.of.month(as.character(data)))) %>%
              group_by(month, agegroup, dose) %>% 
              summarise(total = sum(n, na.rm = T)) %>%
              spread(key = dose, value = total) %>%
              complete(month = seq.Date(min(month), max(month), by="month"), agegroup,
                       fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0)) %>%
              distinct() %>%
              mutate(D1 = D1 - D2,
                     D2 = D2 + D2f) %>%
              group_by(agegroup) %>%
              mutate(D1cum = cumsum(D1),
                     D2cum = cumsum(D2),
                     Dcum = cumsum(D)) %>%
              gather(key = "dose", value = "n", -month, -agegroup)  %>%
              mutate(dose = factor(dose, levels = c("Dcum","D2cum","D1cum","D","D2f","D2","D1"), ordered = T)) %>%
              mutate(UF = state)

  # Compute the population's dose coverage by epidemiological week
  
  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(data)) %>%
    group_by(week, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    spread(key = dose, value = total) %>%
    complete(week = seq.Date(min(week), max(week), by= "week"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Dcum = cumsum(D)) %>% #,
    # DA = cumsum(DA),
    # R = cumsum(R)) %>%
    select(-D2f) %>%
    gather(key = "dose", value = "n", -week, -agegroup) %>%
    mutate(dose = factor(dose, levels = c("Dcum","D2cum","D1cum","D","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state) %>%
    arrange(week, dose, agegroup)
  
  # Bind rows
  
  da_month <- bind_rows(da_month, df_month)  
  da_week <- bind_rows(da_week, df_week)  
}

fin = Sys.time()
fin - ini

# Set month date to the first day of the next month and filter agegroup = 1

da_month <- da_month %>%
  mutate(month = beginning.of.month(as.character(month + 32))) %>%
  drop_na(month, agegroup) %>%
  filter(agegroup != 1)

# Filter agegroup = 1
da_week <- da_week %>%
  drop_na(week, agegroup) %>%
  filter(agegroup != 1)

# Save output

fwrite(da_month, file = "output/doses_cobertura_proporcao_mes.csv")
fwrite(da_week, file = "output/doses_cobertura_proporcao_semana.csv")
fwrite(da_order, file = "output/doses_ordem_uf.csv")

################
### Plots
###############

data = da_month %>%
  filter(dose %in% c("Dcum","D1cum","D2cum")) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, agegroup, dose) %>%
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","D2cum","D1cum"),
                       labels = c("D","D2","D1"), 
                       ordered = TRUE),
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
                                    "90+")))

g1 <- ggplot(data, aes(x = month, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[1],pal[4]))

ggsave(g1, file = "figuras/aplicacao_doses_prop_mes.png", width = 12, height = 8)

###
data2 = da_month %>% 
  filter(dose %in% c("Dcum","D1cum","D2cum")) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, UF, dose) %>% 
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","D2cum","D1cum"),
                       labels = c("D","D2","D1"), 
                       ordered = TRUE))

g2 <- ggplot(data2, aes(x = month, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[1],pal[4]))

ggsave(g2, file = "figuras/aplicacao_doses_uf_mes.png", width = 18, height = 12)

######## Weekly plot

data3 = da_week %>%
  filter(dose %in% c("Dcum","D1cum","D2cum")) %>%
  mutate(week = as.Date(week)) %>%
  group_by(week, agegroup, dose) %>% summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","D2cum","D1cum"),
                       labels = c("D","D2","D1"), 
                       ordered = TRUE),
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
                                      "90+")))

g3 <- ggplot(data3, aes(x = week, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup, ncol = 4, scale = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[1],pal[4]))

ggsave(g3, file = "figuras/aplicacao_doses_prop_week.png", width = 12, height = 8)

###
data4 = da_week %>%
  filter(dose %in% c("Dcum","D1cum","D2cum")) %>%
  mutate(week = as.Date(week)) %>%
  group_by(week, UF, dose) %>% 
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","D2cum","D1cum"),
                       labels = c("D","D2","D1"), 
                       ordered = TRUE))

g4 <- ggplot(data4, aes(x = week, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[1],pal[4]))

ggsave(g4, file = "figuras/aplicacao_doses_uf_week.png", width = 18, height = 12)

