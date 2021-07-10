library(dplyr)
library(data.table)

dados <- fread("vacinas_SP.csv",sep = ";", select = c("paciente_id","paciente_enumsexobiologico","paciente_idade",
                                                      "paciente_racacor_valor","estabelecimento_municipio_nome",
                                                      "vacina_dataaplicacao","vacina_descricao_dose","vacina_nome")
               ) %>% filter(estabelecimento_municipio_nome == "SAO PAULO")
# dados <- dados 
write.csv(dados,file = "dados_SP_cidade.csv")
