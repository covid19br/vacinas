#!/bin/bash

## CONFIGURAÇÂO E PASTAS

# script assume que o meta-repo tem a estrutura usual
dir=$(readlink -f $(dirname $0))
cd $dir

METAREPO=`readlink -f ..`
DADOS=$METAREPO/dados/vacinas

## PROCESSAMENTO DE OPÇÕES DE LINHA DE COMANDO

usage(){
    echo "USO: $0 [-hdocpga]"
    echo -e "\nBaixa dados do SI-PNI, limpa, processa, e atualiza o repositório de dados processados.\n"
    echo -e "  Opções:\n"
    echo -e "    -h help"
    echo -e "    -d faça download dos dados"
    echo -e "    -o sobrescreva dados anteriores, mesmo que sejam da mesma data"
    echo -e "    -c limpe os dados"
    echo -e "    -p processe os dados"
    echo -e "    -g atualize o repositório com dados processados"
    echo -e "    -r remove os arquivos de dados baixados e/ou limpos"
    echo -e "    -a faça todas as tarefas anteriores (exceto -o e -r, ie sem sobrescrever"
    echo -e "       dados ou removê-los) [equivalente a -dcpg]\n"
}

if [ "$#" -eq 0 ]; then
    usage
    exit 0
fi

# command-line options
OPTIND=1         # Reset in case getopts has been used previously in the shell.
download=
overwrite=
clean=
process=
gitupdate=
remove=
while getopts "hdocpga" opt; do
    case "$opt" in
    h)
        usage
        exit 0
        ;;
    d)  download=1
        ;;
    o)  overwrite=1
        ;;
    c)  clean=1
        ;;
    p)  process=1
        ;;
    g)  gitupdate=1
        ;;
    r)  remove=1
        ;;
    a)  download=1
        clean=1
        process=1
        gitupdate=1
        ;;
    *)
        usage
        exit 1
    esac
done
shift $((OPTIND-1))
[ "${1:-}" = "--" ] && shift

if [ $download ]; then
    ## pega data mais recente
    olddate=`ls dados/dados_*.csv | grep -oP "20\d{2}-\d{2}-\d{2}" | sort -r | head -n1`

    echo "== Tentando baixar base SI-PNI =="

    if [ $overwrite ]; then
        python3 sipni_downloader2.py todas
    else
        python3 sipni_downloader2.py todas -d"$olddate"
    fi
    err_download=$?

    if [ $err_download -eq 2 ]; then
        echo "base SI-PNI ainda não foi atualizada"
        exit 0
    elif [ $err_download -ne 0 ]; then
        echo "erro ao baixar bases SI-PNI"
        exit 1
    fi
fi

if [ $clean ]; then
    # pega data mais recente
    lastdate=`ls dados/dados_*.csv | grep -oP "20\d{2}-\d{2}-\d{2}" | sort -r | head -n1`
    
    echo "== Limpando base SI-PNI de ${lastdate} =="
    
    #######limpa base
    pushd dados/
    echo "cleaning columns in $PWD folder"
    for i in dados_${lastdate}*.csv; do
        awk -f ../limpa_colunas.awk "$i" > limpo_"$i"
    done
    echo "done, returning to $dir"
    popd
fi

if [ $process ]; then
    echo "== Processando base SI-PNI de ${lastdate} =="
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
fi

if [ $remove ]; then
    echo "== Removendo arquivos baixados e limpos"
    rm -v dados/dados_${lastdate}.csv dados/limpo_dados_${lastdate}.csv
fi

if [ $gitupdate ]; then
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

