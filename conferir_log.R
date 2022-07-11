require(tidyverse)

log2 <- read.csv("output/log/log_cobertura_2022_07_05.csv")
log2 <- log[,31:ncol(log2)]

loga <- read.csv("output/log/log_2022_07_05.csv")
loga <- loga[,33:ncol(loga)]

unique(logb$state) == unique(loga$state)

logb <- loga %>% 
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
