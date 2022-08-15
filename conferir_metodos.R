suppressPackageStartupMessages({
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(viridis)){install.packages("viridis"); library(viridis)}
  if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(scales)){install.packages("scales"); library(scales)}
  if(!require(optparse)){install.packages("scales"); library(optparse)}
})

### AUXILIARY FUNCTIONS

as_d <- function(x) {x <- substr(x, 1, 1); return(x)}
as_dcum <- function(x) {x <- paste0(substr(x, 1, 1), substr(x, 3, 5)); return(x)}

### LOAD FILES

dados = fread("doses_cobertura_proporcao_semana.csv") %>% data.frame()
dados_o = fread("doses_cobertura_proporcao_semana_ordem.csv") %>% data.frame()

### PREPARE DATA

sp = dados #%>% filter(UF == "SP")
spo = dados_o #%>% filter(UF == "SP")

s1 = spo %>% 
  mutate(dose = ifelse(first_dose == "Janssen" & nchar(dose) == 2, as_d(dose), dose)) %>%
  mutate(dose = ifelse(first_dose == "Janssen" & nchar(dose) > 2, as_dcum(dose), dose)) %>%
  group_by(week,agegroup,dose) %>%
  summarise(m = sum(n)) %>%
  mutate(type = "ordem") %>%
  ungroup()

s2 = sp %>% 
  group_by(week,agegroup,dose) %>%
  summarise(m = sum(n)) %>%
  mutate(dose = factor(dose, 
                levels = c("Dcum","Rcum","D2cum","D1cum","D","R","D2f","D2","D1"),
                labels = c("Dcum","D3cum","D2cum","D1cum","D","D3","D2","D2","D1")),
              type = "original") %>%
  ungroup()

s3 = bind_rows(s1, s2) %>% mutate(dose = factor(dose)) %>%
  mutate(agegroup = factor(agegroup, levels = 2:11,
                       labels = c("5-11 anos",
                                  "12-17 anos",
                                  "18-29 anos",
                                  "30-39 anos",
                                  "40-49 anos",
                                  "50-59 anos",
                                  "60-69 anos",
                                  "70-79 anos",
                                  "80-89 anos",
                                  "90+ anos")))

#### PLOT

dif1 = s3 %>% filter(dose == "D1cum") %>%
  ggplot(aes(x= week, y = m, color = type)) +
  geom_line() +
  facet_wrap(~agegroup, scale = "free_y") +
  xlab("") +ylab("Cobertura de doses") +
  theme_minimal() +
  scale_color_discrete("Método de classificação",
                       labels  = c("Ordem de aplicação","Descrição da dose")) +
  labs(title = "Comparação de classificação por método: D1 vs 1a dose (Não-Janssen)")+
  labs(caption = "Fonte: OPENDATASUS/SI-PNI, extração realizada em 12 de julho de 2022") +
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0),
        plot.background = element_rect(fill = 'white'))

dif2 = s3 %>% filter(dose == "D2cum") %>%
  ggplot(aes(x= week, y = m, color = type)) +
  geom_line() +
  facet_wrap(~agegroup, scale = "free_y") +
  xlab("") +ylab("Cobertura de doses") +
  theme_minimal() +
  scale_color_discrete("Método de classificação",
                       labels  = c("Ordem de aplicação","Descrição da dose")) +
  labs(title = "Comparação de classificação por método: D2 vs 2a dose (Não-Janssen)")+
  labs(caption = "Fonte: OPENDATASUS/SI-PNI, extração realizada em 12 de julho de 2022") +
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0),
        plot.background = element_rect(fill = 'white'))

dif3 = s3 %>% filter(dose == "D1cum") %>%
  ggplot(aes(x= week, y = m, color = type)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "grey")+
  facet_wrap(~agegroup, scale = "free_y") +
  xlab("") +ylab("Cobertura de doses") +
  theme_minimal() +
  scale_color_discrete("Método de classificação",
                       labels  = c("Ordem de aplicação","Descrição da dose")) +
  labs(title = "Comparação de classificação por método: Reforço vs 3a dose (Não-Janssen)") +
  labs(caption = "Fonte: OPENDATASUS/SI-PNI, extração realizada em 12 de julho de 2022") +
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0),
        plot.background = element_rect(fill = 'white'))

difu = s3 %>% filter(dose == "Dcum") %>%
  ggplot(aes(x= week, y = m, color = type)) +
  geom_line() +
  facet_wrap(~agegroup, scale = "free_y") +
  xlab("") +ylab("Cobertura de doses") +
  theme_minimal() +
  scale_color_discrete("Método de classificação",
                       labels  = c("Ordem de aplicação","Descrição da dose")) +
  labs(title = "Comparação de classificação por método: Janssen como primeira dose") +
  labs(caption = "Fonte: OPENDATASUS/SI-PNI, extração realizada em 12 de julho de 2022") +
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0),
        plot.background = element_rect(fill = 'white'))

ggsave(dif1, file = "figuras/metodo/d1.png", dpi = 300, width = 12, height = 8)
ggsave(dif2, file = "figuras/metodo/d2.png", dpi = 300, width = 12, height = 8)
ggsave(dif3, file = "figuras/metodo/d3.png", dpi = 300, width = 12, height = 8)
ggsave(difu, file = "figuras/metodo/du.png", dpi = 300, width = 12, height = 8)


dif_all = s3 %>% filter(dose %in% c("D1cum","D2cum","D3cum","Dcum")) %>%
  group_by(week, dose, type) %>%
  summarise(m = sum(m)) %>%
  mutate(dose = factor(dose, levels = c("D1cum","D2cum","D3cum","Dcum"),
                      labels = c("D1 vs 1a dose",
                                 "D2 vs 2a dose",
                                 "Reforço vs 3a dose",
                                 "Dose única vs Janssen como 1a dose"))) %>%
  ggplot(aes(x= week, y = m, color = type)) +
  geom_line() +
  facet_wrap(~dose, scale = "free_y") +
  xlab("") +ylab("Cobertura de doses") +
  theme_minimal() +
  scale_color_discrete("Método de classificação",
                       labels  = c("Ordem de aplicação","Descrição da dose")) +
  labs(title = "Comparação de classificação por método") +
  labs(caption = "Fonte: OPENDATASUS/SI-PNI, extração realizada em 12 de julho de 2022") +
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 0),
        plot.background = element_rect(fill = 'white', color = "white"))

ggsave(dif_all, file = "figuras/metodo/todas_doses_brasil.png", dpi = 300, width = 12, height = 8)

#################################################
######## Conferir cobertura para dados por município
#################################################

ibge <- read_csv2("../vacinas/dados/municipios_codigos.csv")

ibge2 <- ibge %>% select(`Código Município Completo`, Nome_UF) %>%
  rename(codigo = `Código Município Completo`,
         UF = Nome_UF) %>%
  mutate(codigo = factor(substr(codigo,1,6)))

source("C:/Users/morde/OneDrive/RWorkspace/Covid19/pega_pop_datasus_fx_regiao (1).R")

pop = tabnet_pop()
pop2 = pop %>% 
  select(-ano, -sexo, -Total) %>%
  gather(key = "AG", value = "n", -codreg, -nome_regiao) %>%
  group_by(AG) %>% dplyr::summarise(t = sum(n))

pop3 = pop2[c(1,7,2:6,8:11),]

pop4 = pop3 %>% mutate(agegroup = factor(c(1,1,2,2,3:9))) %>%
  group_by(agegroup) %>%
  summarise(total =sum(t))

mun = fread("municipios/sipni_muni_aplicacao_long.csv.gz")

cobertura <- mun %>% 
              mutate(agegroup = gsub(10,9,agegroup)) %>%
              filter(SE == max(mun$SE, na.rm = T)) %>%
              group_by(dose, agegroup) %>%
              summarise(m = sum(n)) %>%
              ungroup() %>%
              mutate(agegroup = factor(agegroup)) %>%
              left_join(pop4, by = c("agegroup")) %>%
              mutate(p = m/total*100,
                     dose = factor(dose, levels = 1:3,
                                   labels = c("1a dose",
                                              "2a dose",
                                              "3a dose")),
                     agegroup = factor(agegroup, levels = 1:9,
                                       labels = c("0-9",
                                                  "10-19",
                                                  "20-29",
                                                  "30-39",
                                                  "40-49",
                                                  "50-59",
                                                  "60-69",
                                                  "70-79",
                                                  "80+")))

gcov <- ggplot(cobertura, aes(x = agegroup, y= p, fill = dose)) +
  geom_col(position = "identity") +
  scale_fill_viridis_d("Dose") +
 # facet_wrap(~dose) +
  ylab("\nCobertura estimada") + xlab("\nGrupo etário") +
  scale_color_discrete("Grupo Etário") + #, labels = c("0-9","10-19","20-59","60+")) +
  theme_minimal()

ggsave(gcov, file = "figuras/cobertura_idades.png", dpi = 300, width = 12, height = 8)

#################################################
###### Conferir cobertura por UF (dados por município)
#################################################

pop_uf = tabnet_pop(qnivel = "uf")

pop_uf2 = pop_uf %>% 
  select(-ano, -sexo, -Total) %>%
  gather(key = "AG", value = "n", -codreg, -nome_regiao) %>%
  mutate(agegroup = factor(AG,
                           levels = c("FX_0_a_4","FX_5_a_9","FX_10_a_14","FX_15_a_19",
                                      "FX_20_a_29","FX_30_a_39","FX_40_a_49","FX_50_a_59",
                                      "FX_60_a_69","FX_70_a_79","FX_80_e_mais"),
                           labels = c(1,1,2,2,3:9),
                           ordered = TRUE)) %>%
  group_by(agegroup, nome_regiao) %>% 
  summarise(t = sum(n)) %>%
  ungroup()

cobertura_uf <- mun %>% 
  mutate(agegroup = gsub(10,9,agegroup)) %>%
  filter(SE == 77) %>%
  mutate(codigo_municipio = factor(codigo_municipio)) %>%
  left_join(ibge2, by = c("codigo_municipio" = "codigo"), na_matches = "never") %>%
  group_by(dose, agegroup, UF) %>%
  summarise(m = sum(n)) %>%
  ungroup() %>%
  mutate(agegroup = factor(agegroup, ordered = T)) %>%
  left_join(pop_uf2, by = c("agegroup", "UF" = "nome_regiao")) %>%
  mutate(p = m/t*100,
         dose = factor(dose, levels = 1:3,
                       labels = c("1a dose",
                                  "2a dose",
                                  "3a dose")),
         agegroup = factor(agegroup, levels = 1:9,
                           labels = c("0-9",
                                      "10-19",
                                      "20-29",
                                      "30-39",
                                      "40-49",
                                      "50-59",
                                      "60-69",
                                      "70-79",
                                      "80+")))

gcov_uf <- ggplot(cobertura_uf, aes(x = agegroup, y= p, fill = dose)) +
  geom_hline(yintercept = 100) +
  geom_col(position = "identity") +
  scale_fill_viridis_d("Dose") +
  facet_wrap(~UF) +
  ylab("\nCobertura estimada") + xlab("\nGrupo etário") +
  scale_color_discrete("Grupo Etário") + #, labels = c("0-9","10-19","20-59","60+")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = 'white', color = "white"))

ggsave(gcov_uf, file = "figuras/cobertura_idades_uf.png", dpi = 300, width = 12, height = 8)

#########################
######### CONFERIR DIFERENCA ENTRE BASES DE DATAS DIFERENTES PELA COBERTURA

cov_jun <- fread("doses_cobertura_proporcao_semana.csv") %>% data.frame()
cov_mar <- fread("doses_cobertura_proporcao_semana_marco.csv") %>% data.frame()

cov_mar %>%
  filter(dose %in% c("D1cum", "D2cum", "Dcum")) %>%
  group_by(week, agegroup, dose) %>%
  summarise(m = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = week, y = m)) +
  geom_line(aes(linetype = dose)) +
  facet_wrap(~agegroup)

cov <- cov_jun %>% mutate(banco = "zjunho") %>%
  mutate(dose = ifelse(dose == "Rcum", "D2cum", dose)) %>%
  bind_rows(cov_mar %>% mutate(banco = "marco")) %>%
  filter(dose %in% c("D1cum", "D2cum", "Dcum")) %>%
  group_by(week, agegroup, dose, banco) %>%
  summarise(m = sum(n)) %>%
  ungroup() %>%
  mutate(agegroup = factor(agegroup, levels = 1:11,
                           labels = c("0-4",
                                      "5-11",
                                      "12-17",
                                      "18-29",
                                      "30-39",
                                      "40-49",
                                      "50-59",
                                      "60-69",
                                      "70-79",
                                      "80-89",
                                      "90+")))

cov %>%
  ggplot(aes(x = week, y = m, color = dose)) +
  geom_line(aes(linetype = banco)) +
  # geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "grey")+
  facet_wrap(~agegroup, scale = "free_y") +
  xlab("") + ylab("Cobertura de doses") +
  scale_color_discrete("Dose", labels = c("D1","D2+","Janssen+")) +
  scale_linetype_discrete("Data de extração", labels = c("8-março","20-junho")) +
  scale_x_date(date_labels = "%b", date_breaks = "3 months") +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = 'white', color = "white"))


cov_jun %>% mutate(banco = "zjunho") %>%
  mutate(dose = ifelse(dose == "Rcum", "D2cum", dose)) %>%
  bind_rows(cov_mar %>% mutate(banco = "marco")) %>%
  filter(dose %in% c("D1cum", "D2cum", "Dcum")) %>%
  filter(agegroup > 8) %>%
  group_by(week, UF, dose, banco) %>%
  summarise(m = sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = week, y = m, color = dose)) +
  geom_line(aes(linetype = banco)) +
  facet_wrap(~UF, scale = "free_y")

###
setwd("C:/Users/morde/Documents/GitHub/vacinas/output/log")

log <- read.csv("log_2022_07_05.csv")

log %>% 
  select(before_rem_dupli, after_remove_id, state) %>%
  mutate(dif = (before_rem_dupli - after_remove_id)/before_rem_dupli *100) %>%
  ggplot(aes(x = state, y = dif)) + geom_col() +
  ylim(0,50)
########## last

siglas <- read.csv("siglas_estados.txt", sep =",")

pop_uf3 <- pop_uf2 %>% 
  left_join(siglas, by = c("nome_regiao" = "NOME")) %>%
  mutate(agegroup = as.numeric(agegroup)) %>%
  mutate(SIGLA = substr(SIGLA, 2,3)) %>%
  select(agegroup, SIGLA, t)


dados_o %>%
  filter(dose %in% c("D1cum","D2cum","D3cum","D4cum","D5cum")) %>%
  group_by(week, n, dose, UF, agegroup) %>%
  summarise(m = sum(n, na.rm = T)) %>%
  # mutate(agegroup = factor(agegroup)) %>%
  left_join(pop_uf3, by = c("UF" = "SIGLA", "agegroup" = "agegroup")) %>%
  mutate(p = m/t) %>%
  #filter(dose == "D3cum") %>%
  filter(agegroup == 7) %>%
  group_by(week, dose, UF) %>%
  summarise(p = sum(p)) %>%
  ggplot(aes(x = week, y = p, fill = dose)) +
  geom_col() +
  facet_wrap(~UF, scale = "free_y")


dados_o %>%
  filter(dose %in% c("D1cum","D2cum","D3cum","D4cum","D5cum")) %>%
  group_by(week, n, dose, agegroup) %>%
  summarise(m = sum(n, na.rm = T)) %>%
  mutate(agegroup = factor(agegroup)) %>%
  left_join(pop4, by = "agegroup") %>%
  mutate(p = m/total) %>%
  ggplot(aes(x = week, y = p, fill = dose)) +
  geom_col() +
  facet_wrap(~agegroup)

