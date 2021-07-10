library(dplyr)
library(tidyr)
library(flexsurv)
library(survminer)
library(ggplot2)
dados_floripa <- read.csv("dados_floripa.csv")
dados <- select(dados_floripa, c("paciente_id","paciente_enumsexobiologico",
                                 "paciente_racacor_valor","estabelecimento_municipio_nome",
                                 "vacina_dataaplicacao","vacina_descricao_dose","vacina_nome"))
dados[(dados$vacina_nome == "Vacina Covid-19 - Covishield"),]$vacina_nome <- "AZD1222"
dados[(dados$vacina_nome == "Covid-19-Coronavac-Sinovac/Butantan"),]$vacina_nome <- "CoronaVac"
dados[(dados$vacina_nome == "Vacina covid-19 - BNT162b2 - BioNTech/Fosun Pharma/Pfizer"),]$vacina_nome <- "BNT162b2"
dados$vacina_dataaplicacao <- as.Date(dados$vacina_dataaplicacao)
dados <- dados %>% distinct(paciente_id,vacina_descricao_dose,.keep_all = TRUE)
dados <- dados %>% distinct(paciente_id,vacina_dataaplicacao,.keep_all = TRUE)
doses <- unique(dados$vacina_descricao_dose)
dados[dados$vacina_descricao_dose == doses[1],"vacina_descricao_dose"] <- 1
dados[dados$vacina_descricao_dose == doses[2],"vacina_descricao_dose"] <- 2
dados <- dados %>% pivot_wider(names_from = vacina_descricao_dose,values_from = c(vacina_nome,vacina_dataaplicacao))

# dados <- dados[(!(dados$vacina_dataaplicacao_2 < dados$vacina_dataaplicacao_1 & !is.na(dados$vacina_dataaplicacao_2)) | is.na(dados$vacina_dataaplicacao_2)),]
##corrige essa linha depois

# if(length((!is.na(dados$vacina_dataaplicacao_2) & is.na(dados$vacina_dataaplicacao_1))) > 0) {
# dados[(!is.na(dados$vacina_dataaplicacao_2) & is.na(dados$vacina_dataaplicacao_1)),c("vacina_dataaplicacao_1","vacina_dataaplicacao_2")] <-
#   data.frame(dados[(!is.na(dados$vacina_dataaplicacao_2) & is.na(dados$vacina_dataaplicacao_1)),"vacina_dataaplicacao_2"],NA)
# }
data_base <- as.Date("2021-06-24")
min_coronavac <- 21
dados <- dados %>% mutate(tempo_doses = ifelse(!is.na(vacina_dataaplicacao_2), vacina_dataaplicacao_2 - vacina_dataaplicacao_1,
                                        ifelse(data_base - vacina_dataaplicacao_1 > min_coronavac,data_base - vacina_dataaplicacao_1,NA)),
                          cens = ifelse(!is.na(vacina_dataaplicacao_2),1,0))
dados$cens <- as.numeric(dados$cens)
dados_coronavac <- dados %>% filter(vacina_nome_1 == "CoronaVac" & tempo_doses >= 21 & !is.na(tempo_doses))
dados_coronavac$tempo_doses = as.numeric(dados_coronavac$tempo_doses - min_coronavac+1)
dados_coronavac <- as.data.frame(dados_coronavac)
g <- ggplot(dados_coronavac,aes(x=tempo_doses,group = as.factor(cens),color = as.factor(cens))) + geom_histogram(binwidth = 1, fill="white")+geom_vline(xintercept = c(21,28))
#   geom_vline(xintercept = mean(dados_coronavac$tempo_doses,na.rm=TRUE),color="blue")+
#   geom_vline(xintercept = 21, color = "red")+
#   geom_vline(xintercept = 28,color = "red")+
#   xlim(NA,50)
# 
# km <- survfit(Surv(tempo_doses,cens)~1,data = dados_coronavac)
# plot(km)


colnames(dados_coronavac)[2] <- "Sex"
f1 <- flexsurvreg(Surv(tempo_doses,cens)~Sex,data = dados_coronavac,dist="llogi")

gplot <- ggsurvplot(f1, data = dados_coronavac,conf.int = TRUE,risk.table = TRUE,
                    # surv.median.line = "hv",  # add the median survival pointer.
                    legend.labs =
                      c( "Female","Male"),
                    break.time.by=14)
gplot$table <- gplot$table + labs(title = "People receiving second dose")
gplot$plot <- gplot$plot + geom_hline(yintercept = 0.1)
gplot

