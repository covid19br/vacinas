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
  y = c(x[1],x[2],x[3],x[4])
  return(substr(paste0(order(y),collapse = ""), start = 1, stop = 4 - sum(is.na(y))))
}

which_order2 <- function(x) {
  y = c(x[1],x[2],x[3],x[4],x[5],x[6],x[7])
  return(substr(paste0(order(y),collapse = ""), start = 1, stop = 7 - sum(is.na(y))))
}

###############
### Script
###############

#### Define objects
pal = c(wes_palette(n = 5, name = "Zissou1", type = "discrete"),
        wes_palette(n = 5, name = "Rushmore1", type = "discrete"))


first.day <- as.Date("2021-01-01")

data_base <- list.files("dados/") %>% 
                grep("^dados_.*.csv", ., value = T) %>%
                substr(7,16) %>%
                as.Date() %>%
                max(na.rm = T)

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
  
  df2 <- data.frame(fread(paste0("output/wide/",i),
                         select = c("data_D1",
                                    "data_D2",
                                    "data_R",
                                    "data_D",
                                    "agegroup"),
                         colClasses = c("data_D1" = "Date",
                                        "data_D2" = "Date",
                                        "data_R" = "Date",
                                        "data_D" = "Date")))

  
  df2[df2==""] <- NA
  df2$agegroup <- factor(df2$agegroup, levels = c(1:11))
  
  # Compute time difference in days between each dose type to reference date
  df2 <- df2 %>% mutate(dif1 = as.numeric(data_D1 - first.day),
                       dif2 = as.numeric(data_D2 - first.day),
                       difR = as.numeric(data_R - first.day),
                       difU = as.numeric(data_D - first.day)) %>%
                filter(!(difR %leNAF% dif2)) %>%
                filter(!(dif2 %leNAF% dif1))

  # Compute dose's date sequence
  df2$next_order = apply(df2[,c("dif1","dif2","difU","difR")], 1, which_order)

   # Compute the frequency of the dose's date sequence
  da_order_uf <- df2 %>% count(next_order) %>% mutate(UF = state)
  da_order <- bind_rows(da_order, da_order_uf)
  
  # Dates from D1
  df2$dose <- NA
  df2$dose[df2$next_order %in% c(1,12,13,14,
                                 123,124,132,134,142,143,
                                 1234,1243,1342,1324,1423,1432)] <- "D1"
  
  df2$dose[df2$next_order %in% c(3,31,32,34,
                                 312,314,321,324,341,342,
                                 3124,3142,3214,3241,3421) ] <- "D"
  
  df2$data <- NA
  df2$data <- df2$data_D1
  df2$data[which(df2$dose == "D")] <- df2$data_D[which(df2$dose == "D")]
    
  df_d1 <- df2 %>% 
            drop_na(dose) %>% 
            count(data, agegroup, dose) %>%
            rename(date = data) %>%
            complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
                     agegroup, dose,
                     fill = list(n = 0)) %>%
            rename(data = date)
  
  # Dates from D2
  d2_code_a <- c(12,123,124,1234,1243) # D2 na segunda posição
  d2_code_b <- c(13, 132, 134, 1342, 1324) # Segunda posição (ou dose) é D (Janssen), porém primeira não é Janssen
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% d2_code_a] <- df2$data_D2[df2$next_order %in% d2_code_a]
  df2$data[which(df2$next_order %in% d2_code_b)] <- df2$data_D[which(df2$next_order %in% d2_code_b)]
  
  df_d2a <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "D2") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Dates from D2 (D1 not present in the data base)
  d2_code_c <- c(2,23,234,24,243)
  
  df2$data <- NA
  class(df2$data) <- "Date"
  df2$data[df2$next_order %in% d2_code_c] <- df2$data_D2[df2$next_order %in% d2_code_c]
  
  df_d2b <- df2 %>% 
            drop_na(data) %>% 
            count(data, agegroup) %>% 
            mutate(dose = "D2f") %>%
            rename(date = data) %>%
            complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
                     agegroup, dose,
                     fill = list(n = 0)) %>%
            rename(data = date)
  
  ####
  # Dates from R
  dr_code_a <- c(124,1243) # R na terceira posição e D1 presente
  dr_code_b <- c(24, 243) # R na segunda posição e D1 ausente
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% dr_code_a] <- df2$data_R[df2$next_order %in% dr_code_a]
  df2$data[which(df2$next_order %in% dr_code_b)] <- df2$data_R[which(df2$next_order %in% dr_code_b)]
  
  df_Ra <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Dates from R
  dr_code_c <- c(123,1234) # D na terceira posição como reforço e D1 presente #143 (D1-R-D) e 1432 (D1-R-D-D2) ignorados
  dr_code_d <- c(23,234)  # D na segunda posição como reforço e D1 ausente
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% dr_code_c] <- df2$data_D[df2$next_order %in% dr_code_c]
  df2$data[which(df2$next_order %in% dr_code_d)] <- df2$data_D[which(df2$next_order %in% dr_code_d)]
  
  df_Rb <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Clear cache
  rm(df2);gc()
  
  # Merge all tables
  df_doses <- full_join(df_d1, 
                   df_d2a, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never") %>%
              full_join(df_d2b, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")  %>%
              full_join(df_Ra, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")  %>%
              full_join(df_Rb, 
                   by = c("data","agegroup","dose","n"), 
                   na_matches = "never")
            
  # Compute the population's dose coverage by month
  
  df_month <- df_doses %>%
              mutate(month = as.Date(beginning.of.month(as.character(data)))) %>%
              group_by(month, agegroup, dose) %>% 
              summarise(total = sum(n, na.rm = T)) %>%
              ungroup() %>%
              spread(key = dose, value = total) %>%
              complete(month = seq.Date(as.Date("2021-01-01"), as.Date(beginning.of.month(as.character(data_base))), by="month"), agegroup,
                       fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
              distinct() %>%
              mutate(D1 = D1 - D2,
                     D2 = D2 + D2f - R) %>%
              group_by(agegroup) %>%
              mutate(D1cum = cumsum(D1),
                     D2cum = cumsum(D2),
                     Rcum = cumsum(R),
                     Dcum = cumsum(D)) %>%
              gather(key = "dose", value = "n", -month, -agegroup)  %>%
              mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
              mutate(UF = state)
  # Compute the population's dose coverage by epidemiological week
  
  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(data)) %>%
    group_by(week, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    spread(key = dose, value = total) %>%
    complete(week = seq.Date(end.of.epiweek(as.Date("2021-01-17")), end.of.epiweek(data_base), by= "week"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f - R) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Rcum = cumsum(R),
           Dcum = cumsum(D)) %>% #,
    # DA = cumsum(DA),
    # R = cumsum(R)) %>%
    select(-D2f) %>%
    gather(key = "dose", value = "n", -week, -agegroup) %>%
    mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state) %>%
    arrange(week, dose, agegroup)
  
  # Bind rows
  
  da_month <- bind_rows(da_month, df_month)  
  da_week <- bind_rows(da_week, df_week)  
  
  fwrite(da_month, file = "output/doses_cobertura_proporcao_mes.csv")
  fwrite(da_week, file = "output/doses_cobertura_proporcao_semana.csv")
}

### Calcular cobertura para SP (estado com split)

files <- list.files("output/wide")[grepl("[1-9].csv", list.files("output/wide"))]

df_month_split <- data.frame()
df_week_split <- data.frame()

for(i in files) {

  state = substr(i,22,23)
  print(substr(i,22,25))
  
  # Load table for each state in wide format
  
  df2 <- data.frame(fread(paste0("output/wide/",i),
                          select = c("data_D1",
                                     "data_D2",
                                     "data_R",
                                     "data_D",
                                     "agegroup"),
                          colClasses = c("data_D1" = "Date",
                                         "data_D2" = "Date",
                                         "data_R" = "Date",
                                         "data_D" = "Date")))

  df2[df2==""] <- NA
  df2$agegroup <- factor(df2$agegroup, levels = c(1:11))

  # Compute time difference in days between each dose type to reference date
  df2 <- df2 %>% mutate(dif1 = as.numeric(data_D1 - first.day),
                        dif2 = as.numeric(data_D2 - first.day),
                        difR = as.numeric(data_R - first.day),
                        difU = as.numeric(data_D - first.day)) %>%
    filter(!(difR %leNAF% dif2)) %>%
    filter(!(dif2 %leNAF% dif1))

  # Compute dose's date sequence
  
  cuts <- seq(1,nrow(df2),length.out = 10)
  cuts[1] <- 0
  
  next_order = c()
  for(j in 1:(length(cuts)-1)) {
    
    limits = as.integer(cuts[j:(j+1)]+c(1,0))
    df_temp <- df2[limits[1]:limits[2],]
    next_temp <- apply(df_temp[,c("dif1","dif2","difU","difR")], 1, which_order)
    next_order = c(next_order, next_temp)
    rm(next_temp, df_temp)
  }
  
  df2$next_order = next_order

  # Dates from D1
  df2$dose <- NA
  df2$dose[df2$next_order %in% c(1,12,13,14,
                                 123,124,132,134,142,143,
                                 1234,1243,1342,1324,1423,1432)] <- "D1"
  
  df2$dose[df2$next_order %in% c(3,31,32,34,
                                 312,314,321,324,341,342,
                                 3124,3142,3214,3241,3421) ] <- "D"

  df2$data <- NA
  df2$data <- df2$data_D1
  df2$data[which(df2$dose == "D")] <- df2$data_D[which(df2$dose == "D")]

  df_d1 <- df2 %>% 
    drop_na(dose) %>% 
    count(data, agegroup, dose) %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Dates from D2
  d2_code_a <- c(12,123,124,1234,1243) # D2 na segunda posição
  d2_code_b <- c(13, 132, 134, 1342, 1324) # Segunda posição (ou dose) é D (Janssen), porém primeira não é Janssen
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% d2_code_a] <- df2$data_D2[df2$next_order %in% d2_code_a]
  df2$data[which(df2$next_order %in% d2_code_b)] <- df2$data_D[which(df2$next_order %in% d2_code_b)]
  
  df_d2a <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "D2") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Dates from D2 (D1 not present in the data base)
  d2_code_c <- c(2,23,234,24,243)
  
  df2$data <- NA
  class(df2$data) <- "Date"
  df2$data[df2$next_order %in% d2_code_c] <- df2$data_D2[df2$next_order %in% d2_code_c]
  
  df_d2b <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% 
    mutate(dose = "D2f") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  ####
  # Dates from R
  dr_code_a <- c(124,1243) # R na terceira posição e D1 presente
  dr_code_b <- c(24, 243) # R na segunda posição e D1 ausente
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% dr_code_a] <- df2$data_R[df2$next_order %in% dr_code_a]
  df2$data[which(df2$next_order %in% dr_code_b)] <- df2$data_R[which(df2$next_order %in% dr_code_b)]
  
  df_Ra <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)

  # Dates from R
  dr_code_c <- c(123,1234) # D na terceira posição como reforço e D1 presente #143 (D1-R-D) e 1432 (D1-R-D-D2) ignorados
  dr_code_d <- c(23,234)  # D na terceira posição como reforço e D1 ausente
  
  df2$data <- NA
  class(df2$data) <- "Date"
  
  df2$data[df2$next_order %in% dr_code_c] <- df2$data_D[df2$next_order %in% dr_code_c]
  df2$data[which(df2$next_order %in% dr_code_d)] <- df2$data_D[which(df2$next_order %in% dr_code_d)]
  
  df_Rb <- df2 %>% 
    drop_na(data) %>% 
    count(data, agegroup) %>% mutate(dose = "R") %>%
    rename(date = data) %>%
    complete(date = seq.Date(as.Date("2021-01-17"), data_base, by="day"),
             agegroup, dose,
             fill = list(n = 0)) %>%
    rename(data = date)
  
  # Clear cache
  rm(df2);gc()
  
  # Merge all tables
  df_doses <- full_join(df_d1, 
                        df_d2a, 
                        by = c("data","agegroup","dose","n"), 
                        na_matches = "never") %>%
    full_join(df_d2b, 
              by = c("data","agegroup","dose","n"), 
              na_matches = "never")  %>%
    full_join(df_Ra, 
              by = c("data","agegroup","dose","n"), 
              na_matches = "never")  %>%
    full_join(df_Rb, 
              by = c("data","agegroup","dose","n"), 
              na_matches = "never")
  
  # Compute the population's dose coverage by month

  df_month <- df_doses %>%
    mutate(month = as.Date(beginning.of.month(as.character(data)))) %>%
    group_by(month, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    spread(key = dose, value = total) %>%
    complete(month = seq.Date(as.Date("2021-01-01"), as.Date(beginning.of.month(as.character(data_base))), by="month"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f - R) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Rcum = cumsum(R),
           Dcum = cumsum(D)) %>%
    gather(key = "dose", value = "n", -month, -agegroup)  %>%
    mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state)
  
  # Compute the population's dose coverage by epidemiological week

  df_week <- df_doses %>%
    mutate(week = end.of.epiweek(data)) %>%
    group_by(week, agegroup, dose) %>% 
    summarise(total = sum(n, na.rm = T)) %>%
    ungroup() %>%
    spread(key = dose, value = total) %>%
    complete(week = seq.Date(end.of.epiweek(as.Date("2021-01-17")), end.of.epiweek(data_base), by= "week"), agegroup,
             fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0)) %>%
    distinct() %>%
    mutate(D1 = D1 - D2,
           D2 = D2 + D2f - R) %>%
    group_by(agegroup) %>%
    mutate(D1cum = cumsum(D1),
           D2cum = cumsum(D2),
           Rcum = cumsum(R),
           Dcum = cumsum(D)) %>%
    select(-D2f) %>%
    gather(key = "dose", value = "n", -week, -agegroup) %>%
    mutate(dose = factor(dose, levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"), ordered = T)) %>%
    mutate(UF = state) %>%
    arrange(week, dose, agegroup)
  
  # Bind rows
  df_month_split <- bind_rows(df_month_split, df_month)  
  df_week_split <- bind_rows(df_week_split, df_week)  
  
}

df_month <- df_month_split %>%
  group_by(month, agegroup, dose,UF) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  complete(month = seq.Date(as.Date("2021-01-01"), as.Date(beginning.of.month(as.character(data_base))), by="month"), 
           agegroup, dose, UF,
           fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0, D1cum = 0, D2cum = 0, Rcum = 0, Dcum = 0))

df_week <- df_week_split %>%
  group_by(week, agegroup, dose,UF) %>%
  summarise(n = sum(n, na.rm = T)) %>%
  complete(month = seq.Date(end.of.epiweek(as.Date("2021-01-17")), as.Date(beginning.of.month(as.character(data_base))), by="month"), 
         agegroup, dose, UF,
         fill = list(D1 = 0, D2 = 0, D = 0, D2f = 0, R = 0, D1cum = 0, D2cum = 0, Rcum = 0, Dcum = 0))

##### Unir todas as bases (estados com e sem split)

da_month <- bind_rows(da_month, df_month)
da_week <- bind_rows(da_week, df_week)

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

######

fin = Sys.time()
fin - ini

################
### Plots
###############

data = da_month %>%
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, agegroup, dose) %>%
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
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
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g1, file = "figuras/aplicacao_doses_mes.png", width = 12, height = 8)

###
data2 = da_month %>% 
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(month = as.Date(month)) %>%
  group_by(month, UF, dose) %>% 
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
                       ordered = TRUE))

g2 <- ggplot(data2, aes(x = month, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g2, file = "figuras/aplicacao_doses_uf_mes.png", width = 18, height = 12)

######## Weekly plot

data3 = da_week %>%
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(week = as.Date(week)) %>%
  group_by(week, agegroup, dose) %>% summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
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
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g3, file = "figuras/aplicacao_doses_semana.png", width = 12, height = 8)

###
data4 = da_week %>%
  filter(dose %in% c("Dcum","Rcum","D1cum","D2cum")) %>%
  mutate(week = as.Date(week)) %>%
  group_by(week, UF, dose) %>% 
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(dose = factor(dose,
                       levels = c("Dcum","Rcum","D2cum","D1cum"),
                       labels = c("D","R","D2","D1"), 
                       ordered = TRUE))

g4 <- ggplot(data4, aes(x = week, y = m, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y") +
  xlab("")  + ylab("") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual("Dose", 
                    labels = c("D",
                               "R",
                               "D2",
                               "D1"),
                    values = c(pal[5],pal[8],pal[1],pal[4]))

ggsave(g4, file = "figuras/aplicacao_doses_uf_semana.png", width = 18, height = 12)

