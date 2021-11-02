#!/bin/bash

# loop sobre arquivos da linha de comando
for i "$@"; do
    (head -n 1 "$i" && tail -n +2 "$i" | sort -k 1 -t"," --parallel=4 -u ) > "sorted_${i}.csv"
    echo "${i}"
    rm "${i}"
done
