setwd("C:/Users/morde/Documents/GitHub/vacinas")

require(tidyverse)
require(geofacet)

distribuicao_etaria <- read.csv("dados/DistrEtaria2020.csv")
colnames(distribuicao_etaria)[1] <- "Idade"
distribuicao_etaria <- distribuicao_etaria[-1,]
distribuicao_etaria$agegroup <- factor(c(1,2,3,4,4,4,rep(5:10, each = 2), 11))
de2 <- distribuicao_etaria %>% 
  select(-Idade) %>% 
  group_by(agegroup) %>% 
  summarise(across(everything(), sum)) %>% 
  gather(key = "UF", value = "total", -agegroup)

###
files <- list.files("dados_output/doses_aplicadas")[!grepl("[1-9].csv", list.files("dados_output/doses_aplicadas"))]

da <- data.frame()
for(i in files) {
  state = substr(i,17,18)
  print(state)
  df <- data.frame(read.csv(paste0("dados_output/doses_aplicadas/",i)), UF = state)
  da <- rbind(da, df)
}

da$agegroup <- factor(da$agegroup)
da$doses <- factor(da$doses)
da$UF <- factor(da$UF)
da$data <- as.Date(da$data)

da2 <- da %>% left_join(de2, by = c("agegroup", "UF"))

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
       caption = "Fonte: SI-PNI em 15 de janeiro de 2022.\nEstimativas populacionais das faixas etárias de 5 a 29 anos são aproximadas")
#gd1
ggsave(gd1, file = "dose_1.pdf", width = 15, height = 10)

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
       caption = "Fonte: SI-PNI em 15 de janeiro de 2022.\nEstimativas populacionais das faixas etárias de 5 a 29 anos são aproximadas")
#gd2
ggsave(gd2, file = "dose_2.pdf", width = 15, height = 10)


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
       caption = "Fonte: SI-PNI em 15 de janeiro de 2022.\nEstimativas populacionais das faixas etárias de 5 a 29 anos são aproximadas")
#gdr
ggsave(gdr, file = "dose_reforco.pdf", width = 15, height = 10)
