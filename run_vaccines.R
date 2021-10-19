source("vaccine_functions.R")

estados_br <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
#estados_br <- c("TO")
estados_br = c("SP","TO")

ini = Sys.time()

base = "2021-09-28"

time = ini
for (estado_n in estados_br){
print(estado_n)
prepare_table(estado = estado_n, data_base = base)
#time <- c(time, Sys.time())
gc()
}


estados_br <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","TO")

estados_br <- c("SP","TO")
time = ini
for (estado_n in estados_br){
print(estado_n)
prepara_historico(estado = estado_n, data_base = base)
time <- c(time, Sys.time())
gc()
}
  
#save(time, file = "time.RData")
#prepara_historico("SP")
#eta <- read.csv("dados/DistrEtaria2020.csv")
fin = Sys.time()
fin - ini
print("Done.")
