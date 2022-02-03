require(tidyverse)
require(geofacet)


###########################
end.of.epiweek <- function(x, end = 6) {
  offset <- (end - 4) %% 7
  num.x <- as.numeric(x)
  return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
}

###########################

data_corte = as.Date("2021-09-01")
data_base = as.Date("2022-02-01")
data_base_texto <- format(data_base, format = "%d de %B de %Y")

distribuicao_etaria <- read.csv("dados/DistrEtaria2020.csv")
colnames(distribuicao_etaria)[1] <- "Idade"
distribuicao_etaria <- distribuicao_etaria[-1,]
rownames(distribuicao_etaria) <- NULL

distribuicao_etaria[2,-1] <- as.integer(distribuicao_etaria[2,-1] + distribuicao_etaria[3,-1]*2/5) # 5-11
distribuicao_etaria[3,-1] <- as.integer(distribuicao_etaria[3,-1]*3/5 + distribuicao_etaria[4,-1]*3/5) # 12-17
distribuicao_etaria[4,-1] <- as.integer(distribuicao_etaria[4,-1]*2/5) # 18-19

distribuicao_etaria$agegroup <- factor(c(1,2,3,4,4,4,rep(5:10, each = 2), 11))

de2 <- distribuicao_etaria %>% 
  select(-Idade) %>% 
  group_by(agegroup) %>% 
  summarise(across(everything(), sum)) %>% 
  gather(key = "UF", value = "total", -agegroup)

###
files <- list.files("output/doses_aplicadas")[!grepl("[1-9].csv", list.files("output/doses_aplicadas"))]

da <- data.frame()
for(i in files) {
  state = substr(i,17,18)
  print(state)
  df <- data.frame(read.csv(paste0("output/doses_aplicadas/",i)), UF = state)
  da <- rbind(da, df)
}

da$agegroup <- factor(da$agegroup)
da$doses <- factor(da$doses)
da$UF <- factor(da$UF)
da$data <- as.Date(da$data)

da2 <- da %>% left_join(de2, by = c("agegroup", "UF"))

### Total por Estado, vacina e dose

final = da %>%
  as_tibble() %>%
  drop_na(agegroup) %>%
  group_by(UF, vacina, doses, agegroup) %>%
  #group_by(agegroup, vacina, doses, n, UF) %>%
  summarise(nn = sum(n, na.rm = T)) %>%
  arrange(UF, vacina, doses, agegroup)

write_csv(final, file = "total_vacinas.csv")

### DOSE 1

d1 = da2 %>%
  as_tibble() %>%
  filter(doses == "D1") %>%
  drop_na(agegroup) %>%
  group_by(data, agegroup, n, UF) %>%
  summarise(nn = sum(n, na.rm = T)/total) %>%
  group_by(agegroup, UF) %>%
  mutate(m = cumsum(nn)*100) %>%
  arrange(UF, agegroup, data)

gd1 <- d1 %>%
  ggplot(aes(x = data, y = m, color = agegroup)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  facet_wrap(~UF,  scale = "free_y") +
 # facet_geo(~UF, grid = "br_states_grid1", scale = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Cobertura (%)") +
  scale_color_discrete("Faixa etária",
    labels = c("0-4","5-11","12-17","18-29",
               "30-39","40-49","50-59",
               "60-69","70-79","80-89","90+")) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  labs(title = "Cobertura de Dose 1",
       caption = paste0("Fonte: SI-PNI em ", data_base_texto, ".\nEstimativas populacionais das faixas etárias de 5 a 29 anos são aproximadas"))
#gd1
ggsave(gd1, file = "figuras/dose_1.pdf", width = 15, height = 10)
ggsave(gd1, file = "figuras/dose_1.png", width = 15, height = 10, dpi = 200)

###
gd1b <- d1 %>%
  filter(data >= data_corte) %>%
  mutate(week = end.of.epiweek(data)) %>%
  ggplot(aes(x = week, y = n)) + #, color = agegroup)) + #, fill = agegroup)) +
  geom_col() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  facet_wrap(~UF,  scale = "free_y") +
  # facet_geo(~UF, grid = "br_states_grid1", scale = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Cobertura (%)") +
  #scale_fill_discrete("Faixa etária",
   #                    labels = c("0-4","5-11","12-17","18-29",
  #                                "30-39","40-49","50-59",
   #                               "60-69","70-79","80-89","90+")) +
 # scale_color_discrete(show.legend = FALSE) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  labs(title = "Novas aplicações de Dose 1 (semanal)",
       caption = paste0("Fonte: SI-PNI em ", data_base_texto))
#gd1
ggsave(gd1b, file = "figuras/dose_1_diario.pdf", width = 15, height = 10)
ggsave(gd1b, file = "figuras/dose_1_diario.png", width = 15, height = 10, dpi = 200)


### DOSE 2

d2 = da2 %>%
  as_tibble() %>%
  filter(doses == "D2") %>%
  drop_na(agegroup) %>%
  group_by(data, agegroup, n, UF) %>%
  summarise(nn = sum(n, na.rm = T)/total) %>%
  group_by(agegroup, UF) %>%
  mutate(m = cumsum(nn)*100) %>%
  arrange(UF, agegroup, data)

gd2 <- d2 %>%
  ggplot(aes(x = data, y = m, color = agegroup)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  facet_wrap(~UF,  scale = "free_y") +
  # facet_geo(~UF, grid = "br_states_grid1", scale = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Cobertura (%)") +
  scale_color_discrete("Faixa etária",
                       labels = c("0-4","5-11","12-17","18-29",
                                  "30-39","40-49","50-59",
                                  "60-69","70-79","80-89","90+")) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  labs(title = "Cobertura de Dose 2",
       caption = paste0("Fonte: SI-PNI em ", data_base_texto, ".\nEstimativas populacionais das faixas etárias de 5 a 29 anos são aproximadas"))
#gd2
ggsave(gd2, file = "figuras/dose_2.pdf", width = 15, height = 10)
ggsave(gd2, file = "figuras/dose_2.png", width = 15, height = 10, dpi = 200)

###
gd2b <- d2 %>%
  filter(data >= data_corte) %>%
  mutate(week = end.of.epiweek(data)) %>%
  ggplot(aes(x = week, y = n)) + #, color = agegroup)) + #, fill = agegroup)) +
  geom_col() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  facet_wrap(~UF,  scale = "free_y") +
  # facet_geo(~UF, grid = "br_states_grid1", scale = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Cobertura (%)") +
  #scale_fill_discrete("Faixa etária",
  #                    labels = c("0-4","5-11","12-17","18-29",
  #                                "30-39","40-49","50-59",
  #                               "60-69","70-79","80-89","90+")) +
  # scale_color_discrete(show.legend = FALSE) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  labs(title = "Novas aplicações de Dose 2 (semanal)",
       caption = paste0("Fonte: SI-PNI em ", data_base_texto))
#gd1
ggsave(gd2b, file = "figuras/dose_2_diario.pdf", width = 15, height = 10)
ggsave(gd2b, file = "figuras/dose_2_diario.png", width = 15, height = 10, dpi = 200)

### DOSE REFORÇO

dr = da2 %>%
  as_tibble() %>%
  filter(doses == "R") %>%
  drop_na(agegroup) %>%
  group_by(data, agegroup, n, UF) %>%
  summarise(nn = sum(n, na.rm = T)/total) %>%
  group_by(agegroup, UF) %>%
  mutate(m = cumsum(nn)*100) %>%
  arrange(UF, agegroup, data)

gdr <- dr %>%
  ggplot(aes(x = data, y = m, color = agegroup)) +
  geom_line() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  facet_wrap(~UF,  scale = "free_y") +
  # facet_geo(~UF, grid = "br_states_grid1", scale = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Cobertura (%)") +
  scale_color_discrete("Faixa etária",
                       labels = c("0-4","5-11","12-17","18-29",
                                  "30-39","40-49","50-59",
                                  "60-69","70-79","80-89","90+")) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  labs(title = "Cobertura de doses de reforço",
       caption = paste0("Fonte: SI-PNI em ", data_base_texto))
#gdr
ggsave(gdr, file = "figuras/dose_reforco.pdf", width = 15, height = 10)
ggsave(gdr, file = "figuras/dose_reforco.png", width = 15, height = 10, dpi = 200)


###
grb <- dr %>%
  filter(data >= data_corte) %>%
  mutate(week = end.of.epiweek(data)) %>%
  ggplot(aes(x = week, y = n)) + #, color = agegroup)) + #, fill = agegroup)) +
  geom_col() +
  geom_hline(yintercept = 100, linetype = "dashed") +
  facet_wrap(~UF,  scale = "free_y") +
  # facet_geo(~UF, grid = "br_states_grid1", scale = "free_y") +
  theme_minimal() +
  xlab("") + ylab("Cobertura (%)") +
  #scale_fill_discrete("Faixa etária",
  #                    labels = c("0-4","5-11","12-17","18-29",
  #                                "30-39","40-49","50-59",
  #                               "60-69","70-79","80-89","90+")) +
  # scale_color_discrete(show.legend = FALSE) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
  labs(title = "Novas aplicações de doses de reforço (semanal)",
       caption = paste0("Fonte: SI-PNI em ", data_base_texto, ".\nEstimativas populacionais das faixas etárias de 5 a 29 anos são aproximadas"))
#gd1
ggsave(grb, file = "figuras/dose_reforco_diario.pdf", width = 15, height = 10)
ggsave(grb, file = "figuras/dose_reforci_diario.png", width = 15, height = 10, dpi = 200)
