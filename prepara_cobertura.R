source("vaccine_functions.R")

args = commandArgs(trailingOnly=TRUE)
estado = args[1]
prepara_historico(estado,data_base = Sys.Date())