#setwd("C:/Users/morde/OneDrive/RWorkspace/Covid19/PNI")

if(!require(data.table)){install.packages("data.table"); library(data.table)}
if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(wesanderson)){install.packages("wesanderson"); library(wesanderson)}
if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}

force_bind = function(df1, df2, df3) {
  colnames(df2) = colnames(df1)
  colnames(df3) = colnames(df1)
  bind_rows(df1, df2, df3)
}

count.na <- function(x) sum(is.na(x))

doses.report <- function(data, plot = TRUE, pal = wes_palette("Zissou1", 5, type = "discrete")[c(3,1,5)]){
  
  summary_vector <- c(sum(!is.na(data$D1)&is.na(data$D2)),
                      sum(!is.na(data$D1)&!is.na(data$D2)),
                      sum(is.na(data$D1)&!is.na(data$D2)))
  
  df <- data.frame(group = c("D1 only", "D1 + D2", "D2 only"),
                   label = paste0(round(summary_vector/nrow(data),2)*100,"%"),
                   value = summary_vector)
  
  print(paste("Total de registros:",nrow(data)))
  print(paste0("Individuos registrados com apenas uma dose: ",summary_vector[1], " (",df$label[1],")"))
  print(paste0("Individuos registrados com as duas doses: ", summary_vector[2], " (",df$label[2],")"))
  print(paste0("Individuos registrados com apenas a segunda dose: ", summary_vector[3], " (",df$label[3],")"))
  
  if(plot){
    
    df <- df %>% 
      arrange(desc(group)) %>%
      mutate(ypos = cumsum(value)- 0.5*value )
    
    gg <- ggplot(df, aes(x="", y=value, fill=group)) +
      geom_bar(stat="identity", width=1, color = "white") +
      coord_polar("y", start=0) +
      xlab("") + theme_void() + 
      scale_fill_manual("", values = (pal)) +
      geom_text(aes(y = ypos, label = label), color = "black", size=3) 
    
    return(gg)
  }
}

#### Load data

ini = Sys.time()
dados <- fread("dados/part-00000-6fdca81d-a8ba-4752-b407-2c1f6174c29d-c000.csv")
print("Data succesfully loaded")
estado = "SP"
dados <- dados %>% filter(estabelecimento_uf == estado)

print("Preparing data... 1")

if(estado == "SP") {
  dados$doses <- factor(dados$vacina_descricao_dose, levels = unique(dados$vacina_descricao_dose), label = c("D2","D1","U1","U2","U3"))
} else {
dados$doses <- factor(dados$vacina_descricao_dose, levels = unique(dados$vacina_descricao_dose), label = c("D1","D2","U1","U2"))
}

todas_vacinas <- dados %>% select("paciente_id", "paciente_datanascimento", "vacina_categoria_codigo", "vacina_dataaplicacao", "doses",
                                  "vacina_codigo") 
todas_vacinas <- todas_vacinas %>% mutate(paciente_datanascimento = as.Date(paciente_datanascimento),
                                          vacina_dataaplicacao = as.Date(vacina_dataaplicacao))
rm(dados);gc()

##########################
## Definir vacina aqui

#86: coronavac
#85: astrazeneca
print("Preparing data... 2")

codigo = 85
vacina <- ifelse(codigo == 86, "coronavac","astrazeneca")
minimal_interval <- ifelse(codigo == 86, 21, 84)
##########################

AZ <- todas_vacinas %>% filter(vacina_codigo == codigo) %>% select(-vacina_codigo) %>% as.data.frame()
colnames(AZ) <- c("id", "nasc", "categoria", "data","doses")

nreg <- AZ %>% select(id) %>% group_by(id) %>% count() # Contar frequencia registros_id
AZ <- AZ %>% left_join(nreg, by = "id") %>% filter(n < 3) %>% mutate(id = factor(id)) # Cortar ids com mais de 2 registros

rm(nreg);gc()

# Filter if it has more than one D1 or D2 per ID
rem <- AZ %>% group_by(id, doses, .drop = FALSE) %>% summarise(m = n()) %>% filter(m > 1) 
AZ = AZ %>% filter(!(id %in% rem$id)) %>% mutate(id = droplevels(id))

rm(rem);gc()

age <- AZ %>% 
  filter(doses == "D1") %>% mutate(idade = floor(time_length(data - nasc, "years"))) %>%
  select(id,idade)
AZ <- AZ %>% left_join(age, by = "id")

final = AZ %>% select(-nasc, -n, -categoria) %>% pivot_wider(names_from = doses, values_from = data) %>% select(-id)
final$age <- cut(final$idade, breaks = seq(0,110,10), include.lowest = T, right = F)
final$difdoses <- as.numeric(final$D2 - final$D1)
final$difbase <- as.numeric(data_base - final$D1)

print("Preparing data... ok")

##### Plotar distribuição das doses

d1 = final %>% group_by(D1) %>% summarise(n = n()) %>% drop_na()
d2 = final %>% group_by(D2) %>% summarise(n = n()) %>% drop_na()

allD <- force_bind(data.frame(cbind(d1, type = "D1")),
                   data.frame(cbind(d2, type = "D2")),
                   data.frame(cbind(d1 %>% mutate(D1 = D1 + 84), type = "D1 expected")))

pal <- wes_palette("Zissou1", 5, type = "discrete")[c(1,3,5)]
data_base <- as.Date("2021-07-09")

gdoses <- ggplot(allD, aes(x = D1, y = n, fill = type)) +
  geom_bar(stat = "identity", alpha = 0.9, width = 1) +
  scale_fill_manual("", values = pal) +
  geom_vline(xintercept = data_base) +
  theme_minimal() +
  xlab("")+ ylab("") +
  xlim(as.Date("2021-01-01"), as.Date("2021-11-01"))

filename = paste0(estado, "_", vacina, "_doses.png")
ggsave(filename, plot = gdoses, dpi = 300, height = 10, width = 15)

print("First plot, ok.")

#### Plotar delay

final.dif <- final %>% filter(!(age %in% c("[0,10)","[10,20)"))) %>% drop_na(age, difdoses) %>% filter(difdoses >= minimal_interval)
age_total <- final.dif %>% count(age) %>% rename(n_age = n)
final.dif %>% count(age, difdoses) %>%  group_by(age) %>% summarise(x = sum(n))

cumulative <- final.dif %>% group_by(age,difdoses) %>% summarise(n = n()) %>% 
  group_by(age) %>% mutate(sum = cumsum(n)) %>% 
  left_join(age_total, by = "age") %>% mutate(sum2 = sum/n_age)

gdelay <- ggplot(cumulative, aes(x = difdoses - minimal_interval, y = sum2, color = age)) +
  geom_line() + 
  geom_point() + theme_minimal() +
  xlab("Days after minimal interval") + ylab("Frequency") +
  xlim(0,30) +
  geom_vline(xintercept = c(7,14,21))

filename = paste0(estado, "_", vacina, "_delay.png")
ggsave(filename, plot = gdelay, dpi = 300, height = 10, width = 15)

print("Second plot, ok.")

fin = Sys.time()
fin - ini



