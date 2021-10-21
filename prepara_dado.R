source("vaccine_functions.R")

args = commandArgs(trailingOnly=TRUE)
estado = args[1]
prepare_table(estado,data_base = Sys.Date())