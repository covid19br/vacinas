suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
})

#' Este script classifica todas as combinações de aplicações de doses encontradas no SI-PNI para um mesmo id
#' Em seguida, encontra a data de aplicação das doses para os seguintes grupos:
#' D1: id's de indivíDos que tomaram APENAS a primeira dose, para vacina que não seja Janssen
#' D: id's de indivíDos que tomaram Janssen como a primeira dose, independente de tomarem outras 
#'     doses ou reforços posteriormente
#' D2: id's de todos os outros casos. Inclui indivíDos que tomaram a Dose 2, com ou sem reforço, 
#'     desde que a D1 não seja da marca Janssen.

### Functions

#' less than operator for dates and NA. If one of the values is NA, it is defined as greater than the other value
#' Examples:
#' 
#' "2021-01-15 %mNA% 2021-06-15 ~ TRUE
#' "2021-01-15 %mNA% 2020-06-15 ~ FALSE
#' "2021-01-15 %mNA% NA         ~ TRUE
#'          NA %mNA% 2021-01-15 ~ FALSE
#' 

`%mNA%` <- function(A,B){
  A[is.na(A)] <- "5001-01-01"
  B[is.na(B)] <- "5001-01-01"
  C <- return(A < B)
}

###########################
end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}

###### Script

ini = Sys.time()
files <- list.files("output/wide")[!grepl("[1-9].csv", list.files("output/wide"))]

da <- data.frame()
da2 <- data.frame()

for(i in files) {
  state = substr(i,22,23)
  print(state)
  
  df <- data.frame(fread(paste0("output/wide/",i),
              select = c("data_D1","data_R","data_D2","data_D","agegroup"),
              colClasses = c("data_D1" = "Date",
                             "data_R" = "Date",
                             "data_D2" = "Date",
                             "data_D" = "Date")))
                         
        
  df[df==""] <- NA
  df$agegroup <- factor(df$agegroup, levels = c(1:11))
  
  df$dose <- ""
  
  df$dose[!is.na(df$data_R)] <- "R"
  df$dose[!is.na(df$data_D)] <- "D"
  df$dose[!is.na(df$data_R) &  !is.na(df$data_D)] <- "R+D"
  df$dose[ is.na(df$data_D1) &  is.na(df$data_D2) &  is.na(df$data_R) & !is.na(df$data_D)] <- "D only"
  df$dose[!is.na(df$data_D1) & !is.na(df$data_D2) &  is.na(df$data_R) &  is.na(df$data_D)] <- "(D1+D2) only"
  df$dose[!is.na(df$data_D1) &  is.na(df$data_D2) & !is.na(df$data_R) &  is.na(df$data_D)] <- "(D1+R) only"
  df$dose[ is.na(df$data_D1) & !is.na(df$data_D2) & !is.na(df$data_R) &  is.na(df$data_D)] <- "(D2+R) only"
  df$dose[ is.na(df$data_D1) &  is.na(df$data_D2) & !is.na(df$data_R) & !is.na(df$data_D)] <- "(R+D) only"
  
  df$dose[!is.na(df$data_D1) & !is.na(df$data_D2) &  is.na(df$data_R) & !is.na(df$data_D)] <- "(D1+D2+D) only"
  df$dose[!is.na(df$data_D1) &  is.na(df$data_D2) &  is.na(df$data_R) & !is.na(df$data_D)] <- "(D1+D) only"
  df$dose[ is.na(df$data_D1) & !is.na(df$data_D2) &  is.na(df$data_R) & !is.na(df$data_D)] <- "(D2+D) only"
  
  df$dose[!is.na(df$data_D1) & !is.na(df$data_D2) & !is.na(df$data_R) &  is.na(df$data_D)] <- "(D1+D2+R) only"
  df$dose[!is.na(df$data_D1) &  is.na(df$data_D2) &  is.na(df$data_R) &  is.na(df$data_D)] <- "D1 only"
  df$dose[ is.na(df$data_D1) & !is.na(df$data_D2) &  is.na(df$data_R) &  is.na(df$data_D)] <- "D2 only"
  df$dose[ is.na(df$data_D1) &  is.na(df$data_D2) & !is.na(df$data_R) &  is.na(df$data_D)] <- "DR only"
  
  df$dose[ is.na(df$data_D1) & !is.na(df$data_D2) &  !is.na(df$data_R) & !is.na(df$data_D)] <- "(D2+R+D) only"
  
  df$dose[!is.na(df$data_D1) & !is.na(df$data_D2) & !is.na(df$data_R) & !is.na(df$data_D)] <- "D1+D2+D+R"
  
  df$min_date <- (df$data_D %mNA% df$data_D1) & (df$data_D %mNA% df$data_D2) & (df$data_D %mNA% df$data_R)
  
  df$status <- "D2"
  df$status[grepl("D",df$dose) & df$min_date] <- "D"
  df$status[grepl("D1 only",df$dose)] <- "D1"
  
  df$data <- NA
  df <- as.data.frame(df)
  df$data[df$status == "D2"] <- df[df$status == "D2","data_D2"]
  df$data[df$status == "D1"] <- df[df$status == "D1","data_D1"]
  df$data[df$status == "D"] <- df[df$status == "D","data_D"]
  df$data <- as.Date(df$data)
  
  df$agegroup <- factor(df$agegroup)
    
  df2 = df %>%
    dplyr::count(data, status, agegroup, .drop = FALSE) %>%
    drop_na(data, status, agegroup) %>%
    arrange(data, status, agegroup) %>%
    mutate(UF = state)
  
  da <- rbind(da, df2)

  # Reciclando o nome do objeto (df2) para consumir menos memória ram
  
  df2 = df %>%
    dplyr::count(dose, agegroup, .drop = FALSE) %>%
    drop_na(dose, agegroup) %>%
    arrange(dose, agegroup) %>%
    mutate(UF = state)  

  da2 <- rbind(da2, df2)  
}

fin = Sys.time()
fin - ini

fwrite(da, file = "output/doses_serie_temporal.csv")
fwrite(da2, file = "output/doses_por_estado.csv")

#### Plot

g1 <- da %>% 
  mutate(data = end.of.epiweek(data)) %>%
  dplyr::group_by(data, status) %>% dplyr:: summarise(m = sum(n)) %>%
  ggplot(aes(x = as.Date(data), y = m, fill = status)) + 
  xlab("") + ylab("") + 
  scale_fill_discrete("", labels = c("Dose 1 apenas","Dose 2","Dose Única"))
  geom_col() +
  theme_minimal() +
  labs(title = "Data de aplicação da dose\n")

ggsave(g1, file = "figuras/aplicacao_doses.png", width = 12, height = 8)
