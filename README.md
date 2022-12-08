## Proyecto Elección Pública III

### José A. Torrens Hernández (175021)

### Argumento y enfoque

En el presente trabajo retomo los resultados de la última Elección General al Parlamento de Suecia (Sveriges Riksdag) y el modelo de formación de gobiernos desarrollado por Michael Laver y Kenneth A. Shepsle en *Coalitions and cabinet government* (Lever & Shepsle, 1990). Así pues, buscaré modelar los potenciales equilibrios en la distribución de carteras ministeriales en Suecia. Como apuntan Laver y Shepsle, para tal análisis es necesario contar con el número de asientos controlados por cada partido político; el umbral de votos necesarios para aprobar una moción de censura; las posturas de cada partido en cada dimensión de política pública a analizar; y la cartera ministerial con jurisdicción sobre dicha dimensión de política pública (Lever & Shepsle, 1996, 125–47).

En este sentido, a lo largo del trabajo presentaré los resultados de las elecciones al Parlamento, una lista de posibles coaliciones ganadoras dado el umbral para formar gobierno, y, también, retomaré las discusiones en el debate público durante el periodo de campañas. Las dimensiones de política pública con las que modelaré las posibles coaliciones las identificaré a partir de las declaraciones y preocupaciones de cada partido. Asimismo, los indicadores sobre la postura de cada partido en cada dimensión de política pública los tomaré de estimaciones realizadas por encuestas a expertos, en particular la *2019 Chapel Hill Expert Survey (CHES)*.

### Elección General al Parlamento de Suecia (2022)

#### Principales temas de campaña

Durante la campaña, Jimmie Åkesson, líder del *Sweden Democrats*, introdujo a la conversación pública temas como "la criminalidad, los problemas de integración de parte de la población inmigrante y la espiral inflacionaria, principalmente en los precios de la energía" (Torralba, 2022a.). Sobre la criminalidad, fue particularmente visible la discusión en torno a la a creciente prevalencia de los delitos con armas, con lo que ciertos sectores, como el de Åkesson, argumentaron sobre la relación de la criminalidad con la inmigración, los problemas de integración y los problemas económicos. Así pues, si bien en la campaña se abordaron muchas problemáticas, en todas estuvo presente la discusión sobre asuntos relativos a la inmigración y la criminalidad (Torralba, 2022b.).

Otro tema que cobró relevancia en la conversación fue el aumento del costo de vida en el país. Ante el aumento de la inflación, la P.M. Magdalena Andersson, líder del *Swedish Social Democratic Party*, prometió aumentar las prestaciones sociales, instituir un impuesto a la renta más alto, y apoyar a los afectados por la subida de los precios de la energía; por lo que, la discusión sobre asistencia social fue prioritaria (European Movement, 2022 ; Anderson & Kwai, 2022). Sobre este tema, el European Movement señala que "la campaña del *Swedish Social Democratic Party* se centró en 'recuperar el sistema de bienestar'; mientras que el *Moderate Party* esbozó sus planes para hacer frente a la dependencia de las contribuciones del Estado, y los *Sweden Democrats* abogaron por un 'sistema de bienestar sueco para los ciudadanos suecos', argumentando que los inmigrantes no deberían tener derecho a las ayudas del Estado hasta que estén empleados (European Movement, 2022).

También tuvieron su lugar en la conversación la creciente crisis energética provocada por el conflicto ruso–ucraniano, la transición ecológica y los precios de la energía y combustibles. (European Movement, 2022), pero "quedaron prácticamente excluidos" la adhesión del país a la OTAN y el rompimiento del no–alineamiento militar, así como la estrategia sanitaria durante la pandemia (Torralba, 2022b.).

- *Agregar OTAN / Rusia / Política Europea*
- De esta forma, la discusión se dio principalmente en lo relacionado a temas de *política de bienestar – redistributiva* y *política migratoria – criminalidad*, por lo que estas dos dimensiones serán las que consideraré para el modelo de distribución de carteras. Del conjunto de carteras ministeriales que conforman al gobierno sueco (Ministry of Culture; Defence; Education and Research; Employment; Enterprise, Energy and Communications; Environment; Finance; Foreign Affairs; Health and Social Affairs; Justice; Rural Affairs), tomo para el análisis la cartera de Justicia (Minsitry of Justice) y la cartera de Finanzas (Ministro of Finance). En la cartera de Justicia se elabora la política migratoria y de asilo; y en la cartera de Finanzas se elabora la política económica, fiscal, presupuestaria, financiera, etc. (Regeringskansliet, 2014). Es decir, estas dos carteras definirán el espacio de análisis para el modelo que planteo, pues tienen jurisdicción sobre las dimensiones planteadas anteriormente.
- Interesante como los populistas han conseguido ganar el debate sobre la inmigración, hasta los moderados abogan por un estado de bienestar con derechos de admisión. "la discusión se dio principalmente en lo relacionado a temas de política de bienestar--redistributiva y política migratoria – criminalidad" ten presente que podría haber otra(s) dimensión que revele algo interesante del conflicto, aunque no haya estado en primer plano de las campañas. Plantea un par de candidatas.

#### Resultados electorales

Con las Elecciones Generales, el *Swedish Social Democratic Party* (SSDP) alcanzó el 30.33% de los votos, aproximadamente un tercio de los asientos en el Parlamente, pero el bloque que respaldaba su gobierno (*Centre Party, Green Party, y Left Party*) no alcanzó la mayoría necesaria para constituir gobierno. Por su parte, el partido *Sweden Democrats* (SD) se consolidó como la segunda fuerza política, con aproximadamente 20.54% de los votos, y la suma de sus asientos con los del *Moderate Party*, el *Liberal Party* y el *Christian Democratic Party* supera los 173 asientos del bloque gobernante y los 175 votos necesarios para constituir gobierno. 

Considerando un *umbral de 175 votos para formar gobierno* (según el capítulo sexto, artículo 4 del *Regeringsformen*, no se requieren 175 votos a favor del nuevo Primer Ministro, sino que no se sumen 175 votos en contra), se advierte que ningún partido tiene mayoría para constituir un nuevo gobierno, por lo que es necesaria la formación de una coalición legislativa. Los resultados que anticiparían las posibles coaliciones serían los siguientes:

##### Tabla 1. Resultados Elecciones Generales (Suecia 2022)

|                               **Partido** | Abrev. | European Parliament Group                         | Porcentaje elecciones 2022 | Parlamento  2022 | Diferencial 2018 |
| ----------------------------------------: | ------ | :------------------------------------------------ | :------------------------: | :--------------: | :--------------: |
| Swedish Social Democratic Party $(x_{0})$ | SSDP   | Progressive  Alliance of Socialists and Democrats |           30.33%           |       107        |       + 7        |
|                          Sweden Democrats | SD     | European Conservatives and Reformists  Group      |           20.54%           |        73        |       + 11       |
|                            Moderate Party | MP     | European People’s Party                           |           19.10%           |        68        |       – 2        |
|                                Left Party | LP     | The Left Group in the European Parliament         |           6.75%            |        24        |       – 4        |
|                              Centre Party | CP     | Renew Europe Group                                |           6.71%            |        24        |       – 7        |
|                Christian Democratic Party | CDP    | European People’s Party                           |           5.34%            |        19        |       – 3        |
|                               Green Party | G      | Group of the Greens/European Free–Alliance        |           5.08%            |        18        |       + 2        |
|                             Liberal Party | LIB    | Renew Europe Group                                |           4.61%            |        16        |       – 4        |
|                             Other parties | -      | -                                                 |           1.54%            |        0         |        0         |
|                                           |        | ***Total***                                       |            100%            |       349        |        -         |

#### Partidos que serán considerados para el análisis

Para simplificar el modelo y el análisis espacial, agruparé al Moderate Party (MP) y al Christian Democratic Party (CDP) en un mismo partido, asignando los 19 asientos del CDP a los 68 del MP (la posición de política pública ideal será la correspondiente al partido al que se asigna, en este caso al MP). Por tanto, en el modelo el *Moderate Party* pasará a tener 87 asientos. Además, los asientos correspondientes al Green Party (G) y al Centre Party (CP) los asigno al Left Party (LP), de modo que el LP pasaría a tener 66 asientos. Me quedaré con el partido SSDP y SD, y el LIB como partidos únicos, es decir, a los que no se les asignaron los asientos de ningún otro partido.

- El criterio para agrupar a los partidos es que intuitivamente mantienen una proximidad ideológica, por ejemplo, el MP y el CDP forman parte del mismo European Parliament Group, y el G, CP y LP eran los aliados que sostenían el *status quo* $(x_{0})$, de modo que anticipo sería esperable se agrupasen para formar una posible coalición ganadora
- 
Para simplificar el análisis espacial renombraré a los partidos como A, B, C, D, E. Al partido con más asientos en el parlamento, según esta simplificación, le corresponderá A, y así sucesivamente. En este sentido, el SSDP pasará a nombrarse A, la suma del MP y el CDP pasará a llamarse B, etc. La siguiente tabla contiene la nueva asignación de asientos en el parlamento:

##### Tabla 2. Partidos que serán considerados para el análisis

|                                               **Partidos** |          Abrev.          | Modelo - Parlamento 2022 |
| ---------------------------------------------------------: | :----------------------: | :----------------------- |
|          Swedish Social Democratic Party  (SSDP) $(x_{0})$ |            A             | 107                      |
| Moderate Party (MP) / **Christian Democratic Party (CDP)** |            B             | 87                       |
|                                      Sweden Democrats (SD) |            C             | 73                       |
|  Green Party (G) / Centre Party (CP) / **Left Party (LP)** |            D             | 66                       |
|                                        Liberal Party (LIB) |            E             | 16                       |
|                                                            |       ***Total***        | 349                      |
|                                                            | ***Umbral mayoritario*** | 175                      |

De lo anterior, y dados los asientos de cada partido, se obtiene la siguiente lista de posibles coaliciones ganadoras {winSet $W(x_{0})$} al *status quo* $(x_{0})$ -- SSDP

##### Tabla 3. Posibles coaliciones ganadoras

| Coaliciones  Ganadoras | Asientos | Coaliciones Ganadoras | Asientos |
| ---------------------: | -------- | --------------------: | -------- |
|                  A + B | 194      |             B + C + D | 226      |
|                  A + C | 180      |             B + C + E | 176      |
|              A + B + C | 267      |         A + B + C + D | 333      |
|              A + B + D | 260      |         A + B + C + E | 283      |
|              A + B + E | 210      |         A + B + D + E | 276      |
|              A + C + D | 246      |         A + C + D + E | 262      |
|              A + C + E | 196      |         B + C + D + E | 242      |
|              A + D + E | 189      |     A + B + C + D + E | 349      |

#### Política pública ideal: migración y redistribución

Como afirman Laver y Shepsle, "una vez identificado el conjunto de coaliciones legislativas ganadoras, el siguiente paso es identificar las posiciones de los partidos en aquellas dimensiones de la política que suponemos les motivan a la hora de negociar la formación del gobierno", así como el punto ideal de cada partido para cada dimensión de política pública en el horizonte izquierda–derecha. (Lever & Shepsle, 1996, 127). Para establecer la posición de política pública de cada partido tomo como base la *2019 Chapel Hill Expert Survey (CHES)*, que recoge la percepción de 421 politólogos especializados en política partidista e integración europea. En los siguientes diagramas se representan los puntos ideales de cada partido (A, B, C, D y E) en cada dimensión de política pública identificada previamente.

#####  Política Pública ideal para cada partido político en el horizonte izquierda – derecha

| Diagrama 1. Política de apertura a la migración v. política restrictiva frente a la migración -- Cartera de Justicia |
| :----------------------------------------------------------: |
| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/migracion.png?raw=true) |
| **Diagrama 2. Relevancia de la retórica antiislámica para la dirección del partido -- Cartera de Justicia** |
| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/anti_islam.png?raw=true) |
| **Diagrama 3. Redistribución de la riqueza de 'ricos hacia pobres' -- Cartera de Finanzas** |
| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/redistribution.png?raw=true) |
| **Diagrama 4. Posición del partido sobre política europea y de seguridad europea -- Cartera de Relaciones Exteriores** |
| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/eu_foreign.png?raw=true) |

- La *política migratoria* es capturada en la cartera Justicia; la dimensión de *política económica*, capturado en la cartera de Finanzas; y la dimensión de *política europea* es capturada por la cartera de Relaciones Exteriores.

### Análisis espacial -- winSet Calculator

| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/anti_islam-redistribution.png?raw=true) | ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/immigrate_policy-redistribution.PNG?raw=true) |
| :----------------------------------------------------------: | :----------------------------------------------------------: |
| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/anti_islam-eu%20_foreign.png?raw=true) | ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/redistribution-eu%20foreign.png?raw=true) |
| ![alt text](https://github.com/pptrrns/final_ep3/blob/main/winsetCalc/plots/immigrate_policy-eu_foreignPNG.png?raw=true) |                                                              |

​	

#### Posibilidad de vulnerar el status quo

El análisis espacial en la WinSet Calculator (1998) arroja un *status quo vulnerable*, particularmente al comparar la cartera de Finanzas frente a la de Justicia y Relaciones Exteriores. Sin embargo, plantea un *status quo invulnerable* al comparar la cartera de Relaciones Exteriores con la de Justicia.

Para el par de carteras **Finanzas v. Justicia **arroja como posibles repartos:. 

```r
																	Preferred by:
Winpoint no. 1 :  |   B |   A |    B    C    E
             2 :  |   C |   A |    B    C    E <- This point has an empty winset.
             3 :  |   E |   A |    B    C    E
```
- De esta distribución, solamente C (Finanzas) y A (Justicia) tienen un $C(X_{0}) = ∅$. 

Además, para la comparación entre **Finanzas v. Relaciones Exteriores**, arroja como posibles repartos:

```R
																	Preferred by: 
Winpoint no. 1 :  |   B |   A |    B    C    E
						 2 :  |   B |   D |    B    C    E
             3 :  |   C |   A |    B    C    E <- This point has an empty winset.
             4 :  |   C |   B |    B    C    E
             5 :  |   C |   D |    B    C    E
             6 :  |   E |   A |    B    C    E
             7 :  |   E |   D |    B    C    E
```
- De esta distribución, solamente C (Finanzas) y A (Relaciones Exteriores) tienen un $C(X_{0}) = ∅$

Para la comparación entre **Relaciones Exteriores v. Justicia**, el modelo arroja que

```R
Investigating :  |   A |   A | <- This point has an empty winset.
```
- Es decir, que para esta comparación el modelo identifica un *status quo invulnerable* $C(X_{0}) = ∅$.

### Conclusiones

Es anticipable que el *Sweden Democrats* (SD) busque consolidar su discurso de "rechazo frontal a la inmigración" en políticas públicas concretas. El mismo Jimmie Åkesson, líder del *Sweden Democrats*, aseguró que si hay un cambio en el poder, tendrán un papel central (Torralba, 2022), pero no es del todo claro el gobierno que lograría formarse y si este sería sostenible en el tiempo. Es decir, si se forma un nuevo gobierno, no es seguro que sea un nuevo *status quo invulnerable*. La posibilidad de un gobierno entre C y A (*Swedish Social Democratic Party* y *Sweden Democrats*) parece difícil de conseguirse, pues es en sí misma una alianza "contra natura". En este sentido, me es imposible definir el tipo de gobierno que se formaría (Unified, single–party minority; etc.), ya que no es anticipable un *status quo invulnerabl*e que sea acorde con las diferencias ideológicas de los partidos en juego.

### Bibliografía

Anderson Christina, Isabella Kwai. 2022. "In Dramatic Shift, Right-Wing Bloc Wins Slim Majority in Sweden." New York Times, 14 september 2022. https://nyti.ms/3ziJmTY

Doyle, Paul. 1998. "Winset Calculator".Last update:  25th June 1998. http://homepage.tinet.ie/~doylep/Winset/ws_index.htm

Jolly, Seth, Ryan Bakker, Liesbet Hooghe, Gary Marks, Jonathan Polk, Jan Rovny, Marco Steenbergen, and Milada Anna Vachudova. Forthcoming. “Chapel Hill Expert Survey Trend File, 2019.” Electoral Studies. https://doi.org/10.1016/j.electstud.2021.102420

Laver Michael, Kenneth A. Shepsle. 1990. "Coalitions and cabinet government" . American Political Science Association, vol. 84, No. 3:. 873-90. https://doi.org/10.2307/1962770

Laver Michael, Kenneth A. Shepsle. 1996. Making and breaking govertments: cabinets and legislatures in parliamentary democracies. New York: Cambridge University Press.

Regeringskansliet (Government Offices of Sweden). 2014. "How Sweden is governed". Government Offices Communications Department. https://bit.ly/3TEWnzd

Torralba, Carlos. 2022a. "El Partido Socialdemócrata gana las elecciones en Suecia pero la subida de la derecha deja en el aire el Gobierno." El País, 11 de septiembre de 2022. https://bit.ly/3FhI6UP

Torralba, Carlos. 2022b. "La primera ministra sueca anuncia su dimisión tras confirmarse la victoria del bloque de la derecha." El País, 14 de septiembre de 2022. https://bit.ly/3SDR1mC

Valmyndigheten. 2022. "Swedish election results: 2022." Published 23 September 2022. https://bit.ly/3THpNfZ