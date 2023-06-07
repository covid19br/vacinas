suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(viridis)){install.packages("viridis"); library(viridis)}
  if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(scales)){install.packages("scales"); library(scales)}
  if(!require(optparse)){install.packages("scales"); library(optparse)}
  if(!require(stringr)){install.packages("stringr"); library(stringr)}
  if(!require(crunch)){install.packages("crunch"); library(crunch)}
})


print("Iniciando a união das tabelas")

files <- list.files("output/municipios")
#files <- list.files("output/yale")
files_pac <- grep("pac_yale", files, value = T)
files_apl <- grep("apli_yale", files, value = T)


# Salvar por DOSE

DOSES = c("D1","D2","D3","D4","D5")

for(DOSE in DOSES){

df_pac <- data.frame()
for(i in files_pac) {
  print(paste0(i,"_",DOSE))
  pac <- fread(paste0("output/municipios/",i), 
               colClasses = c("muni_pac" = "character",
                              "doses" = "factor",
                              "agegroup" = "factor",
                              "month" = "Date",
                              "vacina" = "integer",
                              "n" = "integer")) %>% data.frame() %>%
    filter(doses = DOSE)
  
  df_pac <- bind_rows(df_pac, pac)
  rm(pac);gc()
}

# Corrigir código de cidades satélite em Brasília
df_pac$muni_pac[grepl("^53",df_pac$muni_pac)] <- "530010"

# Agrupar resultados para todos os municípios
final_pac <- df_pac %>% 
  mutate(muni_pac = factor(muni_pac)) %>% 
  group_by(muni_pac, agegroup, month, vacina) %>% 
  summarise(n = sum(n, na.rm =T)) %>%
  ungroup() %>%
  mutate(vacina = factor(vacina, levels = 85:88, labels = c("AZ","CV","PF","JS")))

# Salvar
filename <- paste0("output/sipni_muni_residencia_yale_",DOSE,".csv.gz")
print(paste0("Salvando: ", filename))
write.csv.gz(final_pac, file = filename)

rm(final_pac, df_pac);gc()
}


###
for(DOSE in DOSES){
  
df_apl <- data.frame()

for(i in files_apl) {
  
  print(paste0(i,"_",DOSE))
  
  apl <- fread(paste0("output/municipios/",i), 
               colClasses = c("muni_pac" = "character",
                              "doses" = "factor",
                              "agegroup" = "factor",
                              "month" = "Date",
                              "vacina" = "integer",
                              "n" = "integer")) %>% data.frame() %>%
    filter(doses = DOSE)
  
  df_apl <- bind_rows(df_apl, apl)
  rm(apl);gc()
}

# Corrigir código de cidades satélite em Brasília
df_apl$muni_apli[grepl("^53",df_apl$muni_apli)] <- "530010"

# Agrupar resultados para todos os municípios
final_apl <- df_apl %>% 
  mutate(muni_apli = factor(muni_apli)) %>% 
  group_by(muni_apli, doses, agegroup, month, vacina) %>% 
  summarise(n = sum(n, na.rm =T)) %>%
  ungroup() %>%
  mutate(vacina = factor(vacina, levels = 85:88, labels = c("AZ","CV","PF","JS")))

filename <- paste0("output/sipni_muni_aplicacao_yale_",DOSE,".csv.gz")
print(paste0("Salvando: ", filename))
write.csv.gz(final_apl, file = filename)

rm(final_apl, df_apl);gc()
}

print("Finalizada a extração de dados para Yale University.")
###