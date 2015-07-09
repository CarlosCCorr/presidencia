#! /usr/bin/bash
./getCatalog.sh
./getDependencies.sh
for i in $(cat ../../data/dependencies.txt)
do
    curl -ls $i | jq -c '{catalogo:.["title"],modified:.["modified"]}'
done


