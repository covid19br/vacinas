#!/usr/bin/bash

#######baixar base atualizada #####pode demorar
python3 sipni_downloader.py todas

#######limpa base
dir=$(pwd)
cd dados/ || exit
echo "cleaning columns in $PWD folder"
for i in dados_*.csv; do
    awk -f ../limpa_colunas.awk "$i" > limpo_"$i"
done
echo "done, returning to $dir"
cd "$dir" || exit


# #######estados que não splitam
estados=("AC" "AL" "AM" "AP" "BA" "CE" "DF" "ES" "GO" "MA" "MG" "MS" "MT" "PA" "PB" "PE" "PI" "PR" "RJ" "RN" "RO" "RR" "RS" "SC" "SE" "TO")
echo "preparing data for states that doesn't split"
for estado in "${estados[@]}"; do
    Rscript prepara_dado.R "$estado"
    Rscript prepara_cobertura.R "$estado"
    echo "state ${estado} done"
done


######estados que splitam
estados_split=("SP")

for estado in "${estados_split[@]}"; do
    cd dados/ || exit
    echo "sorting state ${estado} in $PWD folder"
    ./sort_files.sh limpo_dados*"$estado".csv
    echo "done"
    echo "spliting state ${estado} in $PWD folder"
    ./split_file.sh sorted_limpo_dados*"$estado".csv 4
    rm sorted_limpo_dados*"$estado".csv
    echo "done"
    cd "$dir" || exit
    echo "cleaning data for state ${estado} in $PWD folder"
    Rscript prepara_dado_split.R "$estado"
    rm split_sorted_limpo_dados*"$estado"_*.csv
    echo "done"
    cd output/ || exit
    echo "agregating data in single file for state ${estado} in $PWD folder"
    head -n 1 "$estado"_1_PNI_clean.csv > "$estado"_PNI_clean.csv
    for i in 1 2 3 4; do
        tail -n +2 "$estado"_"${i}"_PNI_clean.csv >> "$estado"_PNI_clean.csv
        rm "$estado"_"$i"_PNI_clean.csv
    done
    echo "done"
    cd "$dir" || exit
    echo "generating number of doses for state ${estado} in $PWD folder"
    Rscript prepara_cobertura.R "$estado"
    echo "done"
done
cd "$dir" || exit
echo "Script finished"

