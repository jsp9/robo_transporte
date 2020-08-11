# **Readme**
#### Estructura de carpetas y archivos necesarios
La estructura de este proyecto asume la siguiente estructura de carpetas y archivos
```
├── README.md
├── data
├───────carpetas-de-investigacion-pgj-cdmx.json (disponible en la página de datos abiertos de la ADIP)
├───────rutas-y-corredores-del-transporte-publico-concesionado (disponible en la página de datos abiertos de la ADIP)
├── out
│   Todo lo contenido en esta carpeta se genera ejecutando el código
└── source (correr en secuencia o correr script 00_master que ejecuta los siguientes scripts)
    ├── 00_master.R
    ├── 01_lee_limplia_json.R
    ├── 02_procesa_lineas.R
    ├── 03_analisis_exploratorio.R
    ├── 04_clusters_interseccion_con_rutas.R
    └── 05_interactivos_mapas_cluster.R
```

#### Dependencias
```
jsonlite
rgdal
lubridate
ggmap
rgeos
dbscan
ggplot2
viridis
gridExtra
data
dplyr
gridExtra
ggpmisc
plotly
```
---
# **Robo a bordo transporte público: ¿a quiénes afecta y dónde atenderlo?**

Versión impresa publicada en: [https://redaccion.nexos.com.mx/?p=9910](https://redaccion.nexos.com.mx/?p=9910)

Pedro salió de su casa un miércoles de diciembre, 2018. Se subió a la ruta 124 como lo hacía todos los días para ir a la universidad. Era estudiante de la UACM. Pedro no sabía que a la altura de Avenida Cuatro, en la delegación Iztapalapa, ese día iba a perder la vida. En este punto, subieron dos hombres para asaltar el camión en el que viajaba. Pedro, con miedo a perder sus pertenencias, intentó abandonar la unidad. Los asaltantes advirtieron esto y dispararon contra él. Los usuarios del transporte público están expuestos a este tipo de situaciones de manera cotidiana.

Hasta ahora, no podíamos saber con suficiente detalle la naturaleza del fenómeno. Los habitantes de la ciudad nos encontrábamos a ciegas para poder entender cómo y dónde suceden estos hechos. Sin un entendimiento del problema, es muy difícil atenderlo.

Durante enero de 2019, la Agencia Digital de Innovación Pública de la Ciudad de México publicó datos desagregados a nivel calle de carpetas de investigación recabadas por la PGJ de la ciudad. Este es un avance importante para enriquecer el acervo de datos disponibles, los cuales permiten hacer análisis con mayor granularidad. El siguiente análisis es un ejemplo de qué se podría hacer con la integración de datos que está construyendo la Agencia.

  

La Agencia registró 2,175 robos a bordo de peseros con o sin violencia y 3,305 robos a pasajero a bordo de transporte público. Esto da un total de 5,480 eventos de 2016 a 2018 sobre los cuales basaremos el análisis.
## ¿Qué viajeros corren en más peligro?
El robo en transporte público tuvo su peor año en el 2018. En 2016, se registraron 1,410 carpetas de investigación totales. En 2017, hubo 1,778; mientras que en 2018 hubo 2,292.
**![](https://github.com/jsp9/robo_transporte/blob/master/out/serie_total.png?raw=true)**
Existen dos posibilidades: hubo un aumento en la frecuencia de estos delitos, o las agencias comenzaron a abrir más carpetas de investigación para los crímenes que ya ocurrían.
Desagregado por alcaldía, las que tienen un aumento mayor durante el 2018 son Iztapalapa y Gustavo A. Madero.
![](https://github.com/jsp9/robo_transporte/blob/master/out/serie_delegacion.png?raw=true)
La gráfica anterior y las subsecuentes que desagregan el fenómeno por alcaldía, nos dejan ver la tendencia temporal. Sin embargo, están influidas por la población total de la alcaldía. Un análisis posterior debería contemplar la tasa de ocurrencias por 100,000 habitantes. Sólo así, se podría afirmar con precisión la prevalencia del delito por alcaldía.

El momento en el que más robos se reportan es a finales de año. 10% de los registros publicados sucedieron en octubre; otro 10% en noviembre. Diciembre es el mes con mayor concentración con 11% de los registros.
![](https://github.com/jsp9/robo_transporte/blob/master/out/mes_ao.png?raw=true)
![](https://github.com/jsp9/robo_transporte/blob/master/out/dia_hora.png?raw=true)
Los crímenes suceden mayormente en las primeras horas de la mañana, en días laborales. **Es decir, la gente que acude a sus actividades diarias en transporte público es especialmente vulnerable**. Aquellos que se trasladan en transporte público desde muy temprano, por vivir lejos de los centros de trabajo o alguna otra razón, corren más peligro.

De hecho, 32% de los registros encontrados en la tabla ocurrieron entre las 5 y las 8 de la mañana. Entre las 11 y las 12 del día, se cometieron 13% de los delitos. Podemos ver un pico menor alrededor del mediodía.
![](https://github.com/jsp9/robo_transporte/blob/master/out/hora_delegacion.png?raw=true)
Otra forma de verlo es a través de la siguiente animación. El color es proporcional al número de robos en transporte público registrados en la alcaldía.
![](https://github.com/jsp9/robo_transporte/blob/master/out/gif_conteos/gif_conteo.gif?raw=true)
En la siguiente animación se muestra la intensidad de puntos en cada hora del día. Un color más saturado representa una mayor cantidad de delitos en la hora indicada. La forma de la mancha nos marca dónde sucedieron esos delitos en cada hora específica.
![](https://github.com/jsp9/robo_transporte/blob/master/out/gif_densidades/densidad.gif?raw=true)
Otra de las novedades que publicó la Agencia son las geometrías de líneas que representan las vías de transporte público que están concesionadas en la ciudad. Con estas líneas, extrajimos cuántos eventos de robo en transporte público ocurrieron en cada una. El siguiente mapa muestra las rutas de transporte público concesionado junto con el número de robos en cada tramo de un kilómetro. Entre más saturado sea el color, más robos hubo en este tramo de la ruta.

![](https://github.com/jsp9/robo_transporte/blob/master/out/mapa_rutas.png?raw=true)
*Líneas de transporte público. color proporcional al número de robos en el tramo*

*Tramos con más robos por alcaldía*

| Ruta                                                          | A la altura de :                  | Robos |
| --------------------- | ------------------------------------------------------------- | --------------------------------- | ----- |
| Iztapalapa            | R-53 Viaducto-Cabeza de Juárez                                | Avenida Universidad               | 77    |
| Gustavo A. Madero     | Ruta 18.13                                                    | Avenida Centenario                | 46    |
| Iztacalco             | R-108 Penitenciaria - Periférico                              | Francisco del Paso y Troncoso     | 38    |
| Miguel Hidalgo        | R-115 Metro Cuatro Caminos - Metro Garibaldi por Flores Magón | Calzada México Tacuba             | 23    |
| Coyoacán              | R-33 Fuego nuevo-Metro Taxqueña                               | Avenida Taxqueña                  | 18    |
| Tlahuac               | R-44 Taxqueña-Tulyehualco                                     | Avenida Tlahuac                   | 17    |
| Venustiano Carranza   | R-11 Eje 5 y Eje 6 sur -Metro Pino Suarez                     | Av. Francisco del Paso y Troncoso | 16    |
| Xochimilco            | R-112, Ramal 57 Santa Catarina - Metro Universidad            | Anillo Periférico                 | 15    |
| Azcapotzalco          | R-2 Azcapotzalco - La Raza                                    | Pino                              | 13    |
| Tlalpan               | R-95 Metro C.U. -Paradero Estadio Azteca                      | Calzada de Tlalpan                | 13    |
| Álvaro Obregón        | R-5 Tacubaya - Cilantro                                       | Camino Real a Toluca              | 12    |
| Cuauhtémoc            | R-1 Peón rca Víctor Cuitláhuac                                | Manuel González                   | 10    |
| Benito Juárez         | R-112 Ramal 46 Oasis - Metro Observatorio                     | Eje 6 sur Playa pie de la cuesta  | 7     |
| Magdalena Contreras   | R-111 Metro Taxqueña - Contreras Anzaldo                      | Camino a Santa Teresa             | 6     |
| Cuajimalpa de morelos | Ruta 4 004 Tacubaya Santiago Yancuitlalpan                    | Carretera federal México Toluca   | 4     |
| Milpa Alta            | Ruta 50                                                       | Morelos                           | 2     |

## 29 lugares concentran 45% de los robos en transporte público de la Ciudad
Los robos en transporte público están concentradas en ciertas zonas. Mediante un algoritmo de aprendizaje automático, se ubicaron 29 lugares de riesgo de este tipo de delitos. Dentro de las fronteras de estos conglomerados, tenemos poco más de 45% de los registros totales reportados por la Agencia de Innovación.
Conglomera
![](https://github.com/jsp9/robo_transporte/blob/master/out/metodo_clusters.png?raw=true)
El conglomerado con más robos se encuentra al norte de la ciudad. En este lugar, la colonia donde más suceden los robos es Martín Carrera, la ruta más castigada es la R-64 Indios Verdes-Loma Prepa. El día y la hora más robos es el miércoles a las seis de la mañana.

Las salidas de la ciudad concentran varios conglomerados detectados. El ejemplo más claro es el de la Calzada Ignacio Zaragoza, hacia la salida a Puebla. Esta área concentra cinco claros clusters, desde la colonia Agrícola, hasta Ermita Zaragoza.

Los robos ocurren en horas más tempranas entre más cerca se encuentren de las salidas de la ciudad. Por ejemplo, contrario a la tendencia general, el cluster que se encuentra en la colonia Morelos, calle Comonfort, la hora en la que más registros hay es las tres de la tarde. En cambio, sobre la calle Francisco VIlla en la colonia General Felipe Berriozabal, la hora más común de los asaltos es las 5:00 AM.
En la siguiente animación se pueden consultar las características definitorias de todos los clusters.
![](https://github.com/jsp9/robo_transporte/blob/master/out/gif_mapa/gif_mapa.gif?raw=true)

La información está disponible en formato interactivo descargando este html: **[https://drive.google.com/open?id=1fGepYvgstd5AN4NYGy3NPc59uh-90DKn](https://drive.google.com/open?id=1fGepYvgstd5AN4NYGy3NPc59uh-90DKn)**

La ZMVM es especialmente vulnerable al fenómeno, por el grado de movilidad que existe en sus salidas. La gente que se traslada a trabajar y a la escuela todos los días desde estados conurbados es enorme. Por ello, es importante procurar su seguridad y, por lo menos, entender el fenómeno.

Este problema tiene por lo menos dos aristas de política pública. Por un lado, se cuenta con información sobre los lugares con más riesgo, por lo que la seguridad pública dedicada a este problema debería dedicarse a estos puntos. Por otro lado, la estrategia de movilidad debería de contemplar este problema. Una mayor capacidad de servicios de transporte más seguros en zonas de riesgo podrían hacer que miles de habitantes tuvieran un trayecto más seguro desde y hacia sus hogares.

---
Todo el código para la replicación de este análisis está disponible en [https://github.com/jsp9/robo_transporte/](https://github.com/jsp9/robo_transporte/). Cualquier aportación es bienvenida mediante un pull request