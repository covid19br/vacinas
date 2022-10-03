suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(viridis)){install.packages("viridis"); library(viridis)}
  if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(scales)){install.packages("scales"); library(scales)}
  if(!require(optparse)){install.packages("scales"); library(optparse)}
})

# Functions

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
files <- grep("_PNI_clean", list.files("output"), value = T)

vac_infantil <- data.frame()

for(f in files) {

  state = substr(f, 1,2)
  
  print(paste0("Reading: ", state ))
  
  UF = fread(paste0("output/",f))

  UFinfantil = UF %>% 
    filter(idade >= 5 & idade <= 12) %>%
    filter(vacina %in% c(86,87)) %>% 
    mutate(id = as.numeric(factor(id))) %>%
    mutate(data = as.Date(data)) %>%
    filter(data >= "2022-01-17") %>%
    group_by(id) %>%
    mutate(ordem = 1:n(),
           idade = min(idade, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(idade >= 5 & idade <= 11) %>%
    arrange(idade, id, data) %>%
    mutate(week = end.of.epiweek(data))
  
   UFinfantil_count <- UFinfantil %>%
    mutate(idade = factor(idade, levels = 5:11),
           ordem = factor(ordem),
           vacina = factor(vacina, levels = c(86,87), labels = c("Coronavac","Pfizer"))) %>%
    count(week, idade, vacina, ordem) %>%
    complete(week = seq.Date(end.of.epiweek(as.Date("2022-01-17")), 
                             end.of.epiweek(as.Date("2022-08-15")), 
                             by="week"), idade, vacina, ordem,
             fill = list(n = 0)) %>%
    mutate(state = state)
  
  vac_infantil <- bind_rows(vac_infantil, UFinfantil_count)
}

vac_infantil <- fread("output/vac_infantil.csv")

vac_infantil_final <- vac_infantil %>%
  group_by(week, idade, vacina, ordem, state) %>%
  summarise(n = sum(n, na.rm = TRUE))

fwrite(vac_infantil_final, file = "output/vac_infantil.csv")

## Plotar resultados

#vac = fread("output/vac_infantil.csv")

# vac %>%
#   mutate(idade = factor(idade),
#          vacina = factor(vacina),
#          state = factor(state)) %>%
#   ggplot(aes(x = week, y = n, color = idade)) +
#     geom_line(aes(linetype = vacina), size = 1) +
#     facet_wrap(~state, scales = "free_y") +
#   scale_color_viridis_d() +
#   theme_minimal() +
#   xlab("") + ylab("")

# 
# vac %>%
#   group_by(week, vacina, state) %>%
#   summarise(n = sum(n)) %>%
#   ungroup() %>%
#   group_by(vacina, state) %>%
#   mutate(m = cumsum(n)) %>%
#   ungroup() %>%
#   mutate(vacina = factor(vacina),
#          state = factor(state)) %>%
#   ggplot(aes(x = week, y = m, color = vacina)) +
#     geom_line(size = 1) +
#     facet_wrap(~state, scales = "free_y") +
#   #scale_color_viridis_d() +
#   theme_minimal() +
#   xlab("") + ylab("")

