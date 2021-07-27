##R --vanilla
require(chron)
require(dplyr)

(load(file="MVcamarasBurroNegro.rda"))

##chequear coordenadas
camaras %>% group_by(UEM) %>% summarise(latitud=mean(latitud),longitud=mean(longitud)) %>% print.AsIs

# camaras sin información sobre el equipo utilizado:
camaras %>% filter( Equipo %in% "")

# verificar fechas de inicio y fin
#camaras$f1 <- chron(dates.=camaras$fecha_ini,format=list(dates="y-m-d"),out.format="y-m-d")
#camaras$f2 <- chron(dates.=camaras$fecha_fin,format=list(dates="y-m-d"),out.format="y-m-d")


# esfuerzo por bloque (URA) y periodo (PMY)
camaras %>% group_by(URA,PMY) %>% summarise(n_camaras=n_distinct(cdg),total_camera_days=sum(f2-f1))


#####
## Chequear número de eventos anotados e identificaciones completadas

fts %>% mutate(grupo=substr(camara,9,11)) %>% group_by(grupo,periodo) %>% summarise(n_fotos=n_distinct(filename),n_eventos=n_distinct(evento)) -> f1

fts %>% filter(Anotado) %>% mutate(grupo=substr(camara,9,11)) %>% group_by(grupo,periodo) %>% summarise(eventos_anotados=n_distinct(evento))-> f2

fts %>% filter(Identificado) %>% mutate(grupo=substr(camara,9,11)) %>% group_by(grupo,periodo) %>% summarise(eventos_identificados=n_distinct(evento))-> f3

f1 %>% left_join(f2)%>% left_join(f3) -> tabla_resumen

tabla_resumen %>% print.AsIs


fts %>% group_by(camara) %>% summarise(n_fotos=n_distinct(filename),n_eventos=n_distinct(evento)) -> f1

fts %>% filter(Anotado) %>% group_by(camara) %>% summarise(eventos_anotados=n_distinct(evento))-> f2

fts %>% filter(Identificado) %>% group_by(camara) %>% summarise(eventos_identificados=n_distinct(evento))-> f3

f1 %>% left_join(f2)%>% left_join(f3) -> tabla_resumen

tabla_resumen %>% print.AsIs



camaras %>% transmute(camara=cdg,
                      basisOfRecord="MachineObservation",
                      type="StillImage",
                      license="http://creativecommons.org/publicdomain/zero/1.0/legalcode",
                      datasetName="LEE-COL-RD",
                      rightsHolder="Instituto Venezolano de Investigaciones Científicas",
                      institutionID="https://ror.org/02ntheh91",
                      decimalLongitude=longitud,
                      decimalLatitude=latitud,
                      geodeticDatum="WGS84",
                      coordinateUncertaintyInMeters=EPE,
                      verbatimLocality=sprintf("%s, Municipio Lagunillas, estado Zulia",Ubicacion),
                      countryCode="VE",
                      country="Venezuela") -> f1

ids %>% transmute(evento,
                  scientificName=sprintf("%s (%s %s)", val, Author,Date),
                  class="Mammalia",
                  order=Order,
                  family=Family,
                  genus=Genus,
                  nameAccordingTo="",
                  vernacularName=NombreLinares,
                  identifiedBy=identificador,
                  identificationRemarks=sprintf("%s , basado en %s , certeza %s%%",criterio, fuente, certeza)) -> f2
                  
            
fts %>% transmute(camara,evento,
            occurrenceID=sprintf("LEE-COL-RD:%s",evento),
            occurrenceStatus=if_else(Identificado %in% TRUE,"present","absent"),
            eventDate=format(fts$f1,'%Y-%I-%dT%H:%M:%S-0400')
            ) %>% left_join(f1,by="camara") %>% left_join(f2,by="evento") -> dC
