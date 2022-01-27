source("vaccine_functions.R")

args = commandArgs(trailingOnly=TRUE)
estado = args[1]

prepara_historico(estado,data_base = "2022-01-15", split = TRUE)
#prepara_historico(estado,data_base = Sys.Date())

join_historico(estado, data_base = "2022-01-15")
