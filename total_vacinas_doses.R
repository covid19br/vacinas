suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
})

###
ini = Sys.time()
files <- list.files("output/wide")[!grepl("[1-9].csv", list.files("output/wide"))]

da <- data.frame()
for(i in files) {
  state = substr(i,22,23) #
  print(state)
  df <- read.csv(paste0("output/wide/",i))
  df[df==""] <- NA
  df$dose <- ""
  df$dose[!is.na(df$data_D1) & is.na(df$data_D2) & is.na(df$data_R)] <- "D1"
  df$dose[!is.na(df$data_D2) & is.na(df$data_R)] <- "D2"
  df$dose[is.na(df$data_DU) & !is.na(df$data_R)] <- "R" #
  df$dose[!is.na(df$data_DU)] <- "DU"
  
  df2 = df %>%
    mutate(agegroup = factor(agegroup)) %>%
    count(vacina_D1, dose, agegroup, .drop = FALSE) %>%
    drop_na(vacina_D1, dose, agegroup) %>%
    arrange(vacina_D1, dose, agegroup) %>%
    filter(dose != "DU") %>%
    mutate(UF = state)

  da <- rbind(da, df2)
}

da <- da %>% filter(vacina_D1 != "Janssen")

fwrite(da, file = "output/total_vacinas.csv")

fin = Sys.time()
fin - ini