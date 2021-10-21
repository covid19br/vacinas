#!/bin/bash
for i in limpo_dados*.csv; do
    (head -n 1 "${i}" && tail -n +2 "${i}" | sort -k 1 -t"," --parallel=4 -u ) > sorted_"${i}"
    echo "${i}"
    rm "${i}"
done
