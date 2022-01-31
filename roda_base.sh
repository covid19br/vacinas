#!/usr/bin/bash

## CONFIGURAÇÂO E PASTAS

# script assume que o meta-repo tem a estrutura usual
dir=$(readlink -f $(dirname $0))
cd $dir

METAREPO=`readlink -f ..`
DADOS=$METAREPO/dados/vacinas

if [ -z $GIT_UPDATE ]; then
    GIT_UPDATE=1
fi

#######baixar base atualizada #####pode demorar
python3 sipni_downloader2.py todas

if [ $? -ne 0 ]; then
    echo "erro ao baixar bases SI-PNI"
    exit 1
fi

####### pega data mais recente
lastdate=`ls dados/dados_*.csv | grep -oP "20\d{2}-\d{2}-\d{2}" | sort -r | head -n1`

echo "Processando base SI-PNI de $lastdate"

#######limpa base
pushd dados/
echo "cleaning columns in $PWD folder"
for i in dados_${lastdate}*.csv; do
    awk -f ../limpa_colunas.awk "$i" > limpo_"$i"
done
echo "done, returning to $dir"
popd


#######estados que não splitam
estados=("AC" "AL" "AM" "AP" "BA" "CE" "DF" "ES" "GO" "MA" "MG" "MS" "MT" "PA" "PB" "PE" "PI" "PR" "RJ" "RN" "RO" "RR" "RS" "SC" "SE" "TO")
echo "preparing data for states that doesn't split"
for estado in "${estados[@]}"; do
    Rscript vaccine_functions.R --command prepara_dado --estado $estado --dataBase $lastdate &&
      Rscript vaccine_functions.R --command prepara_cobertura --estado $estado --dataBase $lastdate &&
      rm output/${estado}_PNI_clean.csv
    echo "state ${estado} done"
done


######estados que splitam
estados_split=("SP")

for estado in "${estados_split[@]}"; do
    pushd dados/
    echo "sorting state ${estado} in $PWD folder"
    ./sort_files.sh limpo_dados_${lastdate}_${estado}.csv
    echo "done"

    echo "spliting state ${estado} in $PWD folder"
    ./split_file.sh sorted_limpo_dados_${lastdate}_${estado}.csv 4 &&
      rm sorted_limpo_dados_${lastdate}_${estado}.csv
    echo "done"
    popd

    #pushd output/
    #echo "aggregating data in single file for state ${estado} in $PWD folder"
    #head -n 1 "$estado"_1_PNI_clean.csv > "$estado"_PNI_clean.csv
    #for i in 1 2 3 4; do
    #    tail -n +2 "$estado"_"${i}"_PNI_clean.csv >> "$estado"_PNI_clean.csv
    #    rm "$estado"_"$i"_PNI_clean.csv
    #done
    #echo "done"
    #popd

    echo "generating number of doses for state ${estado} in $PWD folder"
    Rscript vaccine_functions.R --command prepara_cobertura --estado $estado --dataBase $lastdate &&
        rm output/${estado}_PNI_clean.csv
    echo "done"

    echo "cleaning data for state ${estado} in $PWD folder"
    Rscript vaccine_functions.R --command prepara_dado --split TRUE --estado $estado --dataBase $lastdate &&
      rm dados/split_sorted_limpo_dados_${lastdate}_${estado}_*.csv
    echo "done"
done

echo "Processing finished"

echo "Cleaning up"
#rm -v dados/dados_*.csv dados/limpo_dados_*.csv

if [ $GIT_UPDATE -eq 1 ]; then
    for estado in "${estados[@]}" "${estados_split[@]}"; do
        cp "output/doses_aplicadas/doses_aplicadas_${estado}.csv" $DADOS/doses_estados/
        pushd $DADOS
        git add "doses_estados/doses_aplicadas_${estado}.csv"
        popd
    done
    cd $DADOS
    git commit -m ":robot: atualizando dados processados SI-PNI ${lastdate}" &&
        git push
fi

