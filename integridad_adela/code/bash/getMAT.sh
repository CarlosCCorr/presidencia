#!/usr/bin/bash
echo "institution,base_name,type,size" >> ../../data/MAT_test.csv
for i in $(cat ../../data/dependencies.txt)
do
    inst=$(echo $i | awk -F '/' '{print $2}')
    title=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["title"]' )
    url=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["downloadURL"]' )
    type=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["mediaType"]' )
    size=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["byteSize"]' )
    #    data=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | {title:.["title"], url:.["downloadURL"], type:.["mediaType"], size:.["byteSize"]}' )
    length=$(echo "$title" | wc -l)
	for j in  `seq 1 $length`
	do
	    title_j=$(echo "$title" | sed -n  "${j}p")
	    url_j=$(echo "$url"     | sed -n  "${j}p")
	    type_j=$(echo "$type"   | sed -n  "${j}p")
	    size_j=$(echo "$size"   | sed -n  "${j}p")
	    echo "\"$inst\",$title_j,$type_j,$size_j" >> ../../data/MAT_test.csv
	done
done
