# Load packages
  if(!require(data.table)){install.packages("data.table"); library(data.table)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(viridis)){install.packages("viridis"); library(viridis)}
  if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
  if(!require(scales)){install.packages("scales"); library(scales)}
  if(!require(optparse)){install.packages("scales"); library(optparse)}


# Load data
dados = fread("doses_cobertura_proporcao_semana.csv") %>% data.frame() %>% mutate(week = as.Date(week))
dados_o = fread("doses_cobertura_proporcao_semana_ordem.csv") %>% data.frame()

source("C:/Users/morde/OneDrive/RWorkspace/Covid19/pega_pop_datasus_fx_regiao (1).R")
pop_uf = tabnet_pop(qnivel = "uf")
siglas <- read.csv("siglas_estados.txt", sep =",")

# Prepare data

pop_new <- pop_uf %>% 
  select(-codreg, -ano, -sexo, -Total) %>%
  mutate(i1 =  FX_0_a_4,
         i2 =  FX_5_a_9 + (FX_10_a_14*2/5),
         i3 = (FX_10_a_14*3/5) + (FX_15_a_19*3/5),
         i4 = (FX_15_a_19*2/5) + FX_20_a_29,
         i5 =  FX_30_a_39,
         i6 =  FX_40_a_49,
         i7 =  FX_50_a_59,
         i8 =  FX_60_a_69,
         i9 =  FX_70_a_79,
         i10 = FX_80_e_mais) %>%
  select(nome_regiao, starts_with("i")) %>%
  gather(key = "idade", value = "n", - nome_regiao) %>%
  mutate(idade = as.numeric(substr(idade,2,3))) %>%
  mutate(n = round(n)) %>%
  filter(idade != 1) %>%
  left_join(siglas, by = c("nome_regiao" = "NOME")) %>%
  mutate(SIGLA = substr(SIGLA, 2,3))  %>%
  select(SIGLA, idade, n) %>%
  rename(total = n)
  
dados2 <- dados %>%
  mutate(agegroup = ifelse(agegroup == 11, 10, agegroup)) %>%
  group_by(week, agegroup, dose, UF) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(pop_new, by = c("UF" = "SIGLA", "agegroup" = "idade")) %>%
  mutate(p = round(n/total*100,2)) %>%
  filter(week == max(week)) %>%
  filter(dose %in% c("D1cum","D2cum","Rcum","Dcum")) %>%
  mutate(dose = factor(dose, 
                       levels = c("D1cum","D2cum","Rcum","Dcum"), 
                       ordered = TRUE)) %>%
  mutate(agegroup = factor(agegroup, levels = 1:10,
                labels = c("0-4",
                           "5-11",
                           "12-17",
                           "18-29",
                           "30-39",
                           "40-49",
                           "50-59",
                           "60-69",
                           "70-79",
                           "80+")))

dados3 <- dados_o %>%
  mutate(agegroup = ifelse(agegroup == 11, 10, agegroup)) %>%
  group_by(week, agegroup, dose, UF) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(pop_new, by = c("UF" = "SIGLA", "agegroup" = "idade")) %>%
  filter(agegroup != 1) %>%
  mutate(p = round(n/total*100,2)) %>%
  filter(week == max(week)) %>%
  filter(dose %in% c("D1cum","D2cum","D3cum","D4cum","D5cum")) %>%
  mutate(dose = factor(dose, 
                       levels = c("D1cum","D2cum","D3cum","D4cum","D5cum"), 
                       ordered = TRUE)) %>%
  mutate(agegroup = factor(agegroup, levels = 1:10,
                labels = c("0-4",
                           "5-11",
                           "12-17",
                           "18-29",
                           "30-39",
                           "40-49",
                           "50-59",
                           "60-69",
                           "70-79",
                           "80+")))

# Plot data

gufc <- dados2 %>%
  ggplot(aes(x = agegroup, y = p, fill = dose)) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed") +
  geom_col() +
  scale_fill_viridis_d("Dose", labels = c("D1","D2","R","D")) +
  ylab("Cobertura\n") + xlab("\nGrupo etário") +
  ylim(0,125) +
  facet_wrap(~UF) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = 'white', color = "white")) +
  labs(title = "Cobertura por descrição da dose de aplicação\n")

gufo <- dados3 %>%
  ggplot(aes(x = factor(agegroup), y = p, fill = dose)) +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed") +
  geom_col() +
  scale_fill_viridis_d("Dose", labels = c("D1","D2","D3","D4","D5")) +
  ylab("Cobertura\n") + xlab("\nGrupo etário") +
  ylim(0,125) +
  facet_wrap(~UF) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect(fill = 'white', color = "white")) +
  labs(title = "Cobertura por ordem de aplicação\n")

# Save files

ggsave(gufc, file = "figuras/metodo/cobertura_uf.png", dpi = 300, width = 12, height = 8)
ggsave(gufo, file = "figuras/metodo/cobertura_uf_ordem.png", dpi = 300, width = 12, height = 8)
  