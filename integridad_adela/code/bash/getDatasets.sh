#! /usr/bin/bash

 for i in $(cat ../../data/dependencies.txt); do curl -sL "$i" | jq '.' >> ../../data/datasets/data$i ; done

