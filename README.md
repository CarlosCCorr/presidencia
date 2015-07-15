# presidencia

Este repositorio contiene diversas rutinas utilizadas en el proceso de limpieza, estructuración, revisión y análisis de datos.

## Requerimientos

- [R (≥ 3.1.0)](http://www.r-project.org/)
- [jq (≥ 1.4)](http://stedolan.github.io/jq/download/)

************

## integridad_adela

Como el nombre lo menciona, aquí se almacenan las diferentes rutinas que sirven para checar la calidad de los datos así como la disponibilidad de los mismos en la página de [adela](adela.datos.gob.mx).
Las rutinas están implementadas tanto en R como en bash.

### R

#### integridad_servidores.R

Contiene las funciones necesarias para extraer las ligas y verificar que estas funcionan.

#### linkCheck.R

Toma una muestra de las ligas contenidas en MAT (integridad_adela/data/MAT.csv) y revisa si estas funcionan, 
imprime los resultados al *standard output*.

Para ejcutarlo es necesario instalar los paquetes [httr](http://cran.r-project.org/web/packages/httr/index.html) y [plyr](http://cran.r-project.org/web/packages/plyr/index.html) después de esto puedes ejecutarlo normalmente sobre la línea de comandos.

 <pre><code>./linkCheck.R</code></pre>
  
#### integridad_datos.R

Contiene las funciones necesarias para identificar ciertos tipos de datos en las columnas de las bases de datos dentro de [adela](adela.datos.gob.mx) y verifica que estas se encuentren en un formato adecuado.

### Bash

Para ejecutar las rutinas de esta sección es necesario contar con la utilería de la línea de comandos [jq](stedolan.github.io/jq/). En arch linux
  
<pre><code>yaourt -S jq</code></pre>

#### getCatalog.sh

Obtiene las ligas a todas las nuevas versiones de los catálogos dentro de [adela](adela.datos.gob.mx)

#### getDependencies.sh

Obtiene las ligas a la base de datos de cada una de las dependencias dentro de catálogos.

#### getDatasets.sh

Obtiene datos relevantes sobre los catálogos como fecha de última modificación, número de bases, número de conjuntos de datos, etc.

#### getMAT.sh

Obtiene datos relevantes sobre cada una de las bases de datos de cada uno de los catálogos.

##### Ejemplo

Para obtener la matriz MAT_test.csv, las rutinas se deben ejecutar como sigue:

<pre><code>./getDataset.sh</code></pre>
<pre><code>./getMat.sh</code></pre>

## angeles_verdes

Rutinas para anlizar, predecir y optimizar el tiempo de respuesta a accidentes. Sigue en desarrollo.

## tweets

Rutinas para anlizar y explotar el stream de tweets. Sigue en desarrollo.
