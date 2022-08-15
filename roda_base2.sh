#!/bin/bash

## CONFIGURAÇÂO E PASTAS

# script assume que o meta-repo tem a estrutura usual
dir=$(readlink -f $(dirname $0))
cd $dir

METAREPO=`readlink -f ..`
DADOS=$METAREPO/dados/vacinas

# estados que splitam
estados_split=("SP")
NSPLIT=4

## PROCESSAMENTO DE OPÇÕES DE LINHA DE COMANDO

usage(){
    echo "USO: $0 [-hdocpgram]"
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
    echo -e "       dados ou removê-los) [equivalente a -dcpg]"
    echo -e "    -m avise por e-mail quando a tarefa estiver completa\n"
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
mail=
while getopts "hdocpgram" opt; do
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
    m)  mail=1
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

	## Fazer download de dados do opendatasus
    if [[ $overwrite || -z $olddate ]]; then
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
    # Pega data mais recente
    lastdate=`ls dados/dados_*.csv | grep -oP "20\d{2}-\d{2}-\d{2}" | sort -r | head -n1`
    
    echo "== Limpando base SI-PNI de ${lastdate} =="
    
    #######limpa base (seleciona apenas as colunas de interesse)
	
    pushd dados/
    echo "cleaning columns in $PWD folder"
    for i in dados_${lastdate}*.csv; do
        awk -f ../limpa_colunas.awk "$i" > limpo_"$i"
    done
    echo "done, returning to $dir"
    popd
fi

if [ $process ]; then
    lastdate=`ls dados/dados_*.csv | grep -oP "20\d{2}-\d{2}-\d{2}" | sort -r | head -n1`

    echo "== Processando base SI-PNI de ${lastdate} =="

    ####### Estados cujos bancos são analisados sem separação em diferentes arquivos
    estados=("AC" "AL" "AM" "AP" "BA" "CE" "DF" "ES" "GO" "MA" "MG" "MS" "MT" "PA" "PB" "PE" "PI" "PR" "RJ" "RN" "RO" "RR" "RS" "SC" "SE" "TO")

    echo "preparing data for states that doesn't split"
    for estado in "${estados[@]}"; do
          Rscript vaccine_functions.R --command prepara_dado --estado $estado --dataBase $lastdate &&
          Rscript vaccine_functions.R --command prepara_cobertura --estado $estado --dataBase $lastdate &&
         # rm output/${estado}_PNI_clean.csv
        echo "state ${estado} done"
    done
    
	####### Estado cujos banco é analisado após separação em diferentes arquivos
	
    for estado in "${estados_split[@]}"; do
        pushd dados/
		
		## Os registros são reordenados por id_individuo, de modo que os mesmos ids fiquem nos mesmos arquivos
        echo "sorting state ${estado} in $PWD folder"
        ./sort_files.sh limpo_dados_${lastdate}_${estado}.csv
        echo "done"
    
		## Separação do banco de dados em diferentes arquivos
        echo "spliting state ${estado} in $PWD folder"
        ./split_file.sh sorted_limpo_dados_${lastdate}_${estado}.csv $NSPLIT &&
          rm -v sorted_limpo_dados_${lastdate}_${estado}.csv
        echo "done"
        popd
    
		## Processamento dos dados (prepare_table())
        echo "generating number of doses for state ${estado} in $PWD folder"
        Rscript vaccine_functions.R --command prepara_dado --split TRUE --estado $estado --dataBase $lastdate &&
            rm output/${estado}_PNI_clean.csv
        echo "done"
    
		## Processamento dos dados (prepara_historico() e join_historico())
        echo "cleaning data for state ${estado} in $PWD folder"
        Rscript vaccine_functions.R --command prepara_cobertura_split --split TRUE --estado $estado --dataBase $lastdate &&
         # rm dados/split_sorted_limpo_dados_${lastdate}_${estado}_*.csv
        echo "done"
    
	done

    echo "counting doses for all states"
    Rscript calcular_cobertura_por_estado.R
	Rscript calcular_ordem_por_estado_v2.R
	Rscript analise_municipios.R
    echo "done"

    # limpando arquivos quebrados
    for estado in "${estados_split[@]}"; do
        if [ -s "output/doses_aplicadas/doses_aplicadas_${estado}.csv" ] &&
           [ "output/doses_aplicadas/doses_aplicadas_${estado}.csv" -nt "output/doses_aplicadas/doses_aplicadas_${estado}_${NSPLIT}.csv" ] ; then
            rm -v output/doses_aplicadas/doses_aplicadas_${estado}_*.csv
        fi
    done
fi

if [ $remove ]; then
    echo "== Removendo arquivos baixados e limpos"
	#rm -v output/*_PNI_clean.csv
    rm -v dados/dados_${lastdate}_*.csv dados/limpo_dados_${lastdate}_*.csv
    rm -v dados/split_sorted_limpo_dados_${lastdate}_*_*.csv
fi

if [ $gitupdate ]; then
    for estado in "${estados[@]}" "${estados_split[@]}"; do
        cp "output/doses_aplicadas/doses_aplicadas_${estado}.csv" $DADOS/doses_estados/
        pushd $DADOS
        git add "doses_estados/doses_aplicadas_${estado}.csv"
        popd
    done

	cp "output/doses_ordem_uf.csv" "output/doses_cobertura_proporcao_semana.csv" "output/doses_cobertura_proporcao_mes.csv" $DADOS/
	cp "output/doses_cobertura_proporcao_semana_ordem.csv" "output/doses_cobertura_proporcao_mes_ordem.csv" $DADOS/
	
    pushd $DADOS
    git add "doses_ordem_uf.csv" "doses_cobertura_proporcao_semana.csv" "doses_cobertura_proporcao_mes.csv"
	git add "doses_cobertura_proporcao_semana_ordem.csv" "doses_cobertura_proporcao_mes_ordem.csv"
    popd
	
	cp "figuras/aplicacao_doses_uf_semana.png" "figuras/aplicacao_doses_semana.png" "figuras/aplicacao_doses_uf_mes.png" "figuras/aplicacao_doses_mes.png" $DADOS/figuras/
	cp "figuras/aplicacao_doses_uf_semana_ordem.png" "figuras/aplicacao_doses_semana_ordem.png" "figuras/aplicacao_doses_uf_mes_ordem.png" "figuras/aplicacao_doses_mes_ordem.png" $DADOS/figuras/
	
    pushd $DADOS
    git add "figuras/aplicacao_doses_uf_semana.png" "figuras/aplicacao_doses_semana.png" "figuras/aplicacao_doses_uf_mes.png" "figuras/aplicacao_doses_mes.png"
	git add "figuras/aplicacao_doses_uf_semana_ordem.png" "figuras/aplicacao_doses_semana_ordem.png" "figuras/aplicacao_doses_uf_mes_ordem.png" "figuras/aplicacao_doses_mes_ordem.png"
    popd
	
	cp "output/sipni_muni_residencia.csv.gz" "output/sipni_muni_residencia_agrupado.csv.gz" "output/sipni_muni_aplicacao.csv.gz" "output/sipni_muni_aplicacao_agrupado.csv.gz" "output/sipni_muni_residencia_long.csv.gz" "output/sipni_muni_aplicacao_long.csv.gz" $DADOS/municipios
	
	pushd $DADOS
	git add "municipios/sipni_muni_residencia.csv.gz" "municipios/sipni_muni_residencia_agrupado.csv.gz" "municipios/sipni_muni_aplicacao.csv.gz" "municipios/sipni_muni_aplicacao_agrupado.csv.gz" "municipios/sipni_muni_residencia_long.csv.gz" "municipios/sipni_muni_aplicacao_long.csv.gz"
    popd
	
    pushd $DADOS
    git commit -m ":robot: atualizando dados processados SI-PNI ${lastdate}" &&
        git push
    popd
fi

if [ $mail ]; then
    ( echo "Nova base do SI-PNI de ${lastdate} processada.

Os dados agregados estão disponíveis no repositório https://github.com/covid19br/dados-vacinas.

Atenciosamente,
Robot mailer" ) |
    s-nail -s "nova base SI-PNI de ${lastdate}" `< emails.txt`
fi
