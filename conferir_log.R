require(tidyverse)

log2 <- read.csv("../vacinas/output/log/log_cobertura_2022_07_19.csv")

loga <- read.csv("../vacinas/output/log/log_2022_07_19.csv")

logordem <- read.csv("../vacinas/output/log/log_ordem_2022_07_20.csv")

logmun <- read.csv("../vacinas/output/log/log_municipios_2022_07_19.csv")


unique(logb$state) == unique(loga$state)

logb <- loga %>% 
  select(-X, -nvac, -vac_codes) %>%
  group_by(state) %>% 
  summarise(across(everything(), ~ sum(., is.na(.), 0)))

logc = logb %>%
  select(-state) %>%
  summarise(across(everything(), ~ round((.-before_rem_dupli)/before_rem_dupli * 100,2) )) %>%
  mutate(state = unique(logb$state))

logc %>% ggplot(aes(x = state, y = after_remove_id)) +
  geom_col(fill = 'steelblue') +
  xlab("") + ylab("% registros deletados\n") +
  theme_minimal()

log_cover <- data.frame(n = round(((log2$before_filter_date -  log2$after_filter_date)/log2$before_filter_date)*100,2),
           state = log2$state)
