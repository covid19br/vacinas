#!/bin/bash

for i in dados*.csv; do
    ./limpa_bruto.awk "$i" > "limpo_$i"
done
