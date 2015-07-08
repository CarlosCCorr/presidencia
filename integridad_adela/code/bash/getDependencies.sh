#!/usr/bin/bash

for i in $(cat ../../data/catalogs.json | sed 's;/; ;g' | awk '{print $3}' ); do echo "adela.datos.gob.mx/$i/catalogo.json" >> ../../data/dependencies.txt ;done

