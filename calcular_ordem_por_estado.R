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
first.day <- as.Date("2021-01-01")

#### Run script to compute dose coverage

ini = Sys.time()

files <- list.files("output/wide")[!grepl("[1-9].csv", list.files("output/wide"))]

da_order <- data.frame()

for(i in files) {
  state = substr(i,22,23)
  print(state)
  
  # Load table for each state in wide format
  
  df <- data.frame(fread(paste0("output/wide/",i))) 
  
  if(!any(grepl("3",colnames(df)))) df$data_3 <- NA
  if(!any(grepl("4",colnames(df)))) df$data_4 <- NA
  if(!any(grepl("DA",colnames(df)))) df$data_DA <- NA
  
  df <- df %>%
          mutate_at(vars(contains('data_')), ~as.Date(.)) %>% str()

  
  df[df==""] <- NA
  df$agegroup <- factor(df$agegroup, levels = c(1:11))
  
  # Compute time difference in days between each dose type to reference date
  df2 <- df %>% mutate(difU = as.numeric(data_D - first.day),
                       dif1 = as.numeric(data_D1 - first.day),
                       dif2 = as.numeric(data_D2 - first.day),
                       
                       difA = as.numeric(data_DA - first.day),
                       difR = as.numeric(data_R - first.day),
                       dif3 = as.numeric(data_3 - first.day),
                       dif4 = as.numeric(data_4 - first.day))# %>%
    #filter(!(dif2 %leNAF% dif1))
  
  # Compute dose's date sequence
  df2$next_order = apply(df2[,c("difU","dif1","dif2","difA","difR","dif3","dif4")], 1, which_order2)
  
  # Compute the frequency of the dose's date sequence
  da_order_uf <- df2 %>% count(agegroup, next_order) %>% mutate(UF = state)
  da_order <- bind_rows(da_order, da_order_uf)
  
}

fin = Sys.time()
fin - ini

fwrite(da_order, file = "output/doses_ordem_uf.csv")


