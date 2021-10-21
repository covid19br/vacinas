#!/usr/bin/bash

f=$1
n=$2
suffix=".csv"
foo=${f%"$suffix"}
echo "$foo"
