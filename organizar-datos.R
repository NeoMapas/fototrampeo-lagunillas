##R --vanilla
require(chron)
require(dplyr)
require(readr)

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

fts %>% group_by(camara) %>% summarise(n_fotos=n_distinct(filename),n_eventos=n_distinct(evento)) -> f1

fts %>% filter(Anotado) %>% group_by(camara) %>% summarise(eventos_anotados=n_distinct(evento))-> f2

fts %>% filter(Identificado) %>% group_by(camara) %>% summarise(eventos_identificados=n_distinct(evento))-> f3

f1 %>% left_join(f2)%>% left_join(f3) -> tabla_resumen

tabla_resumen %>% print.AsIs

####

## Crear tabla de cámaras 

camaras %>% transmute(datasetName="LEE-COL-RD",
                      locationId=cdg,
                      decimalLongitude=longitud,
                      decimalLatitude=latitud,
                      geodeticDatum="WGS84",
                      coordinateUncertaintyInMeters=EPE,
                      verbatimLocality="Parque Recreativo Burro Negro, Reserva Nacional Hidráulica de Pueblo Viejo, Municipio Lagunillas, estado Zulia",
                      countryCode="VE",
                      country="Venezuela",
                      dateBegin=fecha_ini,
                      dateEnd=fecha_fin
                      ) -> cameraList

write_csv(cameraList,path="data/camera-locations.csv")

## Crear tabla de eventos por cámara

notas %>% transmute(datasetName="LEE-COL-RD",
                    locationId=camara,
                    eventId=sprintf("LEE-COL-RD:%s",evento),
                    annotatedBy=case_when(
                        anotador == "LM" ~ "Morán, Lisandro",
                        anotador == "AYSM" ~ "Sánchez-Mercado, Ada Yelitza",
                        anotador == "JRFP" ~ "Ferrer-Paris, José R.",
                        TRUE ~ "Personal Laboratorio Ecología Espacial"
                    ),
                    eventType=tipo
                      ) -> eL

fts %>% group_by(camara,evento) %>% summarise(fchini=min(f1),fchfin=max(f1)) %>%
    mutate(
            eventId=sprintf("LEE-COL-RD:%s",evento),
            eventBegin=format(fchini,'%Y-%I-%dT%H:%M:%S-0400'),
            eventEnd=format(fchfin,'%Y-%I-%dT%H:%M:%S-0400')
            ) %>% left_join(eL,by="eventId") %>% ungroup %>% select(datasetName,eventId,locationId,eventBegin,eventEnd,eventType,annotatedBy) -> eventList

write_csv(cameraList,path="data/all-event-annotations.csv")

## Crear tabla de identificaciones con columnas siguiendo el estándard de Darwin Core https://dwc.tdwg.org/

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
                      verbatimLocality="Parque Recreativo Burro Negro, Reserva Nacional Hidráulica de Pueblo Viejo, Municipio Lagunillas, estado Zulia",
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
                  identifiedBy=case_when(
                        identificador == "LM" ~ "Morán, Lisandro",
                        identificador == "AYSM" ~ "Sánchez-Mercado, Ada Yelitza",
                        identificador == "JRFP" ~ "Ferrer-Paris, José R.",
                        TRUE ~ "Personal Laboratorio Ecología Espacial"
                    ),
                  identificationRemarks=sprintf("%s , basado en %s , certeza %s%%", criterio, fuente, certeza)) -> f2
                  
            
fts %>% filter(Identificado %in% TRUE) %>% group_by(camara,evento) %>% summarise(fch=min(f1)) %>%
    mutate(
            occurrenceID=sprintf("LEE-COL-RD:%s",evento),
            occurrenceStatus="present",
            eventDate=format(fch,'%Y-%I-%dT%H:%M:%S-0400')
            ) %>% left_join(f1,by="camara") %>% left_join(f2,by="evento") %>% ungroup %>% select(occurrenceID:identificationRemarks) -> dC

write_csv(cameraList,path="data/events-with-ids.csv")
