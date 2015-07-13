#! /usr/bin/bash
./getCatalog.sh
./getDependencies.sh
echo '"conjunto_de_datos","fecha_modif","num_catalogos","num_bases"' >> ../../data/inventory_test.csv
for i in $(cat ../../data/dependencies.txt)
do
    name_dataset=$(curl -ls $i | jq -c '.["title"]')
    date_modified=$(curl -ls $i | jq -c '.["modified"]')
    number_catalog=$(curl -ls $i | jq -c '.["dataset"] | length')
    number_base=$(curl -ls $i | jq -c '.["dataset"][]["distribution"] | length'| awk '{s+=$1} END {print s}')
    echo "$name_dataset,$date_modified,$number_catalog,$number_base" >> ../../data/inventory_test.csv
done


