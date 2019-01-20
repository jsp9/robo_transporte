
data <- fromJSON("data/carpetas-de-investigacion-pgj-cdmx.json", flatten=TRUE)

###LIMPIEZA TABLA#####

rows<-nrow(data)
head(data)
data$fields.lon<-as.numeric(data$fields.lon)
data$fields.lat<-as.numeric(data$fields.lat)

data<-data[complete.cases(data[, c('fields.lon', 'fields.lat')]),]
rows_geo<-nrow(data)



#with(data[sample(nrow(data), 1000),], plot(fields.lon, fields.lat))

data<-data[grepl('ROBO A PASAJERO A BORDO DE',data$fields.delito),]
data<-data[!grepl('METRO|TAXI',data$fields.delito),]
unique(data$fields.delito)
table(data$fields.delito)[grepl('ROBO A PASAJERO A BORDO DE',names(table(data$fields.delito)))]


table(data$fields.ao)

table(data$fields.delito)

####CONVERTIR A GEOESPACIAL######

data<-SpatialPointsDataFrame(coords = data[, c('fields.lon', 'fields.lat')],
                             data = data,
                             proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))

puntos<-coordinates(data)
bbox<-c(min(puntos[,1]),
        min(puntos[,2]),
        max(puntos[,1]),
        max(puntos[,2]))

mapa_df<-get_stamenmap(bbox = bbox,maptype =  "terrain-lines") #Pedir mapa que vamos a usar para gráficas