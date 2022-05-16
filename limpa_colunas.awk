#!/usr/bin/awk -f 
BEGIN{
  FS = ";"
  OFS = ","
}
#colunas a printar "paciente_id", "paciente_datanascimento", "vacina_dataaplicacao", "vacina_descricao_dose", "vacina_codigo"
#{
#  {print $2, $4, $28, $29, $30}   
#}

#colunas a printar "paciente_id", "paciente_enumSexoBiologico" "paciente_endereco_coIbgeMunicipio" "estabelecimento_municipio_codigo", "paciente_datanascimento", "vacina_dataaplicacao", "vacina_descricao_dose", "vacina_codigo"
{
  {print $2, $4, $5, $8, $18, $28, $29, $30}   
}

END{

}