library(dplyr)
library(tidyr)
library(flexsurv)
library(survminer)
library(ggplot2)
library(data.table)
dados <- fread("dados_SP_cidade.csv", select = c("paciente_id","paciente_enumsexobiologico","paciente_idade",
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
dados <- dados %>% filter(vacina_dataaplicacao_1 > as.Date("2020-12-31"))
dados <- dados %>% mutate(agegroup = cut(paciente_idade, breaks = c(0,50,60,70,80,90,Inf), right = F))
data_base <- as.Date("2021-06-24")
min_coronavac <- 21
dados <- dados %>% mutate(tempo_doses = ifelse(!is.na(vacina_dataaplicacao_2), vacina_dataaplicacao_2 - vacina_dataaplicacao_1,
                                               ifelse(data_base - vacina_dataaplicacao_1 > min_coronavac,data_base - vacina_dataaplicacao_1,NA)),
                          cens = ifelse(!is.na(vacina_dataaplicacao_2),1,0))
dados$cens <- as.numeric(dados$cens)
dados_coronavac <- dados %>% filter(vacina_nome_1 == "CoronaVac" & tempo_doses >= min_coronavac & !is.na(tempo_doses))
dados_coronavac$tempo_doses = as.numeric(dados_coronavac$tempo_doses - min_coronavac+1)
dados_coronavac <- as.data.frame(dados_coronavac)
g <- ggplot(dados_coronavac,aes(x=tempo_doses,group = as.factor(cens),color = as.factor(cens))) + geom_histogram(binwidth = 1, fill="white")+geom_vline(xintercept = c(56,84))


colnames(dados_coronavac)[2] <- "Sex"
dados_coronavac <- dados_coronavac %>% filter(Sex != "I") %>% filter(cens == 1)
f1 <- flexsurvreg(Surv(tempo_doses,cens)~agegroup,data = dados_coronavac,dist="llogi")

gplot <- ggsurvplot(f1, data = dados_coronavac,conf.int = TRUE,
                    # risk.table = TRUE,
                    # # surv.median.line = "hv",  # add the median survival pointer.
                    # legend.labs =
                    #   c( "Female","Male"),
                    break.time.by=14)
gplot$table <- gplot$table + labs(title = "People receiving second dose")
gplot$plot <- gplot$plot + geom_hline(yintercept = 0.1)+scale_colour_viridis_d(option = "viridis")
gplot


