 #!/bin/bash

## CONFIGURAÇÂO E PASTAS

# script assume que o meta-repo tem a estrutura usual
dir=$(readlink -f $(dirname $0))
cd $dir

METAREPO=`readlink -f ..`
DADOS=$METAREPO/dados/vacinas


# pega data mais recente
    lastdate=`ls dados/dados_*.csv | grep -oP "20\d{2}-\d{2}-\d{2}" | sort -r | head -n1`
    
    echo "== Limpando base SI-PNI de ${lastdate} =="
    
    #######limpa base
    pushd dados/
    echo "cleaning columns in $PWD folder"
    for i in dados_${lastdate}_SP.csv; do
        awk -f ../limpa_colunas.awk "$i" > limpo_"$i"
    done
    echo "done, returning to $dir"
    popd
	
	
  for estado in "${estados_split[@]}"; do
        pushd dados/
        echo "sorting state ${estado} in $PWD folder"
        ./sort_files.sh limpo_dados_${lastdate}_${estado}.csv
        echo "done"
    
        echo "spliting state ${estado} in $PWD folder"
        ./split_file.sh sorted_limpo_dados_${lastdate}_${estado}.csv 4 &&
          #rm sorted_limpo_dados_${lastdate}_${estado}.csv
        echo "done"
        popd
    
        echo "generating number of doses for state ${estado} in $PWD folder"
        Rscript vaccine_functions.R --command prepara_dado --split TRUE --estado $estado --dataBase $lastdate &&
            rm output/${estado}_PNI_clean.csv
        echo "done"
    
        echo "cleaning data for state ${estado} in $PWD folder"
        Rscript vaccine_functions.R --command prepara_cobertura_split --split TRUE --estado $estado --dataBase $lastdate &&
          rm dados/split_sorted_limpo_dados_${lastdate}_${estado}_*.csv
        echo "done"
    
	done