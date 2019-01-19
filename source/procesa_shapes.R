rm(list=ls())

setwd('~/Documents/GIT/robo_transporte/')

library(jsonlite)
library(rgdal)
library(lubridate)
library(ggmap)
library(rgeos)
library("dbscan")
library(ggplot2)
library(viridis)
library(gridExtra)


data <- fromJSON("data/carpetas-de-investigacion-pgj-cdmx.json", flatten=TRUE)
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


trans<-readOGR(path.expand('~/Downloads/rutas-y-corredores-del-transporte-publico-concesionado/'),
               'rutas-y-corredores-del-transporte-publico-concesionado')

data<-SpatialPointsDataFrame(coords = data[, c('fields.lon', 'fields.lat')],data = data, proj4string = trans@proj4string)
plot(data, col='red')
lines(trans)
deg_p_m<-(0.001/111.32)

CreateSegments <- function(coords, length = 0, n.parts = 0) {
  stopifnot((length > 0 || n.parts > 0))
  # calculate total length line
  total_length <- 0
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    total_length <- total_length + d
  }
  
  # calculate stationing of segments
  if (length > 0) {
    stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
  } else {
    stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
                    total_length)
  }
  
  # calculate segments and store the in list
  newlines <- list()
  for (i in 1:(length(stationing) - 1)) {
    newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 
                                                                       1])
  }
  return(newlines)
}
CreateSegment <- function(coords, from, to) {
  distance <- 0
  coordsOut <- c()
  biggerThanFrom <- F
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    distance <- distance + d
    if (!biggerThanFrom && (distance > from)) {
      w <- 1 - (distance - from)/d
      x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
      y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
      coordsOut <- rbind(coordsOut, c(x, y))
      biggerThanFrom <- T
    }
    if (biggerThanFrom) {
      if (distance > to) {
        w <- 1 - (distance - to)/d
        x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
        y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
        coordsOut <- rbind(coordsOut, c(x, y))
        break
      }
      coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 
                                                               2]))
    }
  }
  return(coordsOut)
}
MergeLast <- function(lst) {
  l <- length(lst)
  lst[[l - 1]] <- rbind(lst[[l - 1]], lst[[l]])
  lst <- lst[1:(l - 1)]
  return(lst)
}

lineas<-list()
for( i in 1:length(trans)){
  data_lineas<-trans@data[i,]
  test<-CreateSegments(coordinates(trans[i,]@lines[[1]])[[1]],
                       length = deg_p_m*1000)
  #test<-MergeLast(test)
  
  
  segmentos<-list()
  for (i_x in 1:length(test)){
    segmentos[[i_x]]<-Line(test[[i_x]])
    segmentos[[i_x]]<-Lines(segmentos[[i_x]], ID = i_x)
    segmentos[[i_x]]<-SpatialLines(LinesList = list(segmentos[[i_x]]),
                                   proj4string =  trans@proj4string)
    data_lineas$id<-paste(data_lineas$id, i_x, sep = '_')
    data_lineas$ruta_corre<-paste(data_lineas$ruta_corre, i_x, sep = '_')
    segmentos[[i_x]]<-SpatialLinesDataFrame(sl = segmentos[[i_x]],
                                            data = data_lineas,
                                            match.ID = F)
  }
  lineas[[i]]<-segmentos
}

lineas<-unlist(lineas, recursive=FALSE)
lineas<-do.call(rbind, lineas)


#lines(lineas[1:10,]@data, col='red')


lineas<-gBuffer(lineas, byid = T, width = deg_p_m*30)



#######CLUSTERING########


clust<-hdbscan(coordinates(data), minPts = 25, gen_simplified_tree = T)
plot(clust)
plot(data, col=clust$cluster+1)
table(clust$cluster)

data@data<-cbind.data.frame(data@data,
                            cluster=clust$cluster)


puntos<-coordinates(data)

puntos<-as.data.frame(puntos)

bbox <- c(min(puntos[, 1])-0.15,
          min(puntos[, 2])-0.15,
          max(puntos[, 1])+0.15,
          max(puntos[, 2])+0.15)

overlay <- stat_density2d(
  aes(x = fields.lon, y = fields.lat, fill = ..level..,
      alpha = ..level..),
  bins = 4, geom = "polygon",
  data = puntos)

mapa_df<-get_stamenmap(bbox = bbox,maptype =  "terrain-lines")

data@data$fields.fechainicio<-strptime(data@data$fields.fechainicio,'%Y-%m-%dT%H:%M:%S' )

puntos_horas<-split(data@data, hour(data@data$fields.fechainicio))
for (i in 1:24){
  overlay <- stat_density2d(
    aes(x = fields.lon, y = fields.lat, fill = ..level..,
        alpha = ..level..),
    bins = 4, geom = "polygon",
    data = puntos_horas[[i]])
  
  mapa_datos<-ggmap(mapa_df,  extent = "normal", maprange=FALSE)+
    overlay+
    theme_void()+
    theme(legend.position = 'none')+
    scale_fill_gradient(low = "green", high = "red")+
    annotate('text',
             x = min(bbox)+0.27,
             y=max(bbox),
             label= paste('Densidad de robos en transporte público a las:', i, ':00 horas (', nrow(puntos_horas[[i]]), ' totales)'))
  ggsave(filename = paste0('~/Documents/gif_densidades/mapa_', i, 'horas.png'), device = 'png',plot = mapa_datos) 
}
#scale_alpha(range = c(0.00, 1), guide = FALSE)

centroide_alcaldia<-aggregate(data= data@data, cbind(fields.lon, fields.lat)~fields.alcaldiahechos, mean)




puntos_horas<-split(data@data, hour(data@data$fields.fechainicio))

max_conteos<-max(unlist(lapply(puntos_horas, nrow)))
for (i in 1:24){
  puntos_horas[[i]]<-merge(centroide_alcaldia, aggregate(data=puntos_horas[[i]],
                                                         datasetid~fields.alcaldiahechos,
                                                         function(x) cbind(length(x))))
  puntos_horas[[i]]<-rbind.data.frame(puntos_horas[[i]], data.frame(fields.alcaldiahechos=c('NA', 'NA'),
                                                                    fields.lon=c(1000, 1000),
                                                                    fields.lat=c(1000, 1000),
                                                                    datasetid=c(0, max_conteos)))
  
  mapa_datos<-ggmap(mapa_df,  extent = "normal", maprange=T)+
    geom_point(data = puntos_horas[[i]],
               aes(x = fields.lon, y = fields.lat,
                   color=log(datasetid), size=log(datasetid)))+
    theme_void()+
    theme(legend.position = 'none')+
    scale_color_viridis()+
    annotate('text',
             x = min(bbox)+0.27,
             y=max(bbox)-0.1,size=5,
             label= paste('Número de robos en transporte público a las:', i, ':00 horas (', sum(puntos_horas[[i]]$datasetid), ' totales)'))
  ggsave(filename = paste0('~/Documents/gif_conteos//mapa_', i, 'horas.png'), device = 'png',plot = mapa_datos) 
}

head(data@data)
prop.table(table(data@data$fields.mes))*100

calendar_plot<-data@data
calendar_plot<-aggregate(data=calendar_plot, rep(1, nrow(calendar_plot))~fields.ao+fields.mes,sum)
meses<-c('Enero', 'Febrero', 'Marzo',
  'Abril', 'Mayo', 'Junio',
  'Julio', 'Agosto', 'Septiembre',
  'Octubre', 'Noviembre', 'Diciembre')
calendar_plot$fields.mes<-calendar_plot$fields.mes<-factor(calendar_plot$fields.mes, levels = meses)
names(calendar_plot)<-c('yr', 'mes', 'n')
plt<-ggplot(data=calendar_plot, aes(x=mes, y=yr, fill=log(n)))+
  geom_tile()+
  geom_label(aes(label=n), fill='white', size=10)+
  scale_fill_viridis()+
  labs(x="",y="")+
  theme_bw(base_size=50)+
  theme(legend.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_blank(),
        legend.position="none", text = element_text(size = 32))
ggsave('~/Documents/mes_ao.png',plot = plt, device = 'png', height = 4.5*1.5, width = 15.5*1.5)





plot(lineas[1:20,])
points(data, col='red')


#prop.table(table(data@data$cluster)>0))



data@data$diasemana<-weekdays(data@data$fields.fechainicio, abbreviate = T)

data@data$diasemana<-ifelse(data@data$diasemana=='Mon', '01 Lunes',
                            ifelse(data@data$diasemana=='Tue', '02 Martes',
                                   ifelse(data@data$diasemana=='Wed', '03 Miércoles',
                                          ifelse(data@data$diasemana=='Thu', '04 Jueves',
                                                 ifelse(data@data$diasemana=='Fri', '05 Viernes',
                                                        ifelse(data@data$diasemana=='Sat', '06 Sábado',
                                                               ifelse(data@data$diasemana=='Sun', '07 Domingo','0')))))))

data@data$hora<-hour(data@data$fields.fechainicio)
barplot(prop.table(table(data@data$hora))*100)
heat_diasemana<-data@data

heat_diasemana<-as.data.frame(table(heat_diasemana$hora,heat_diasemana$diasemana ))


plt<-ggplot(data=heat_diasemana, aes(x=Var1, y=Var2, fill=Freq))+
  geom_tile()+
  scale_fill_viridis() + theme_bw()+
  ylab('')+xlab('Hora del día')+
  theme(legend.title = element_blank(), text = element_text(size=32))+
  ggtitle('Número de robos en transporte público por día/hora')
ggsave('~/Documents/dia_hora.png',plot = plt, device = 'png', height = 4.5*1.5, width = 15.5*1.5)


heat_alcaldia<-data@data

heat_alcaldia<-as.data.frame(table(heat_alcaldia$hora,heat_alcaldia$fields.alcaldiahechos ))


totales_alcadia<-as.data.frame(table(data@data$fields.alcaldiahechos))
heat_alcaldia<-base::merge.data.frame(heat_alcaldia,totales_alcadia,by.x='Var2', by.y = 'Var1')

levels(heat_alcaldia$Var2)

abrev_delegacion<-c('ALV. OBR.', 'AZCAPOTZALCO', 'B.J.', 'COYOACÁN',
  'CUAJIMALPA', 'CUAUHTEMOC', 'G.A.M', 'IZTACALCO','IZTAPALAPA',
  'MAGDALENA CONT.', 'M. H.', 'MILPA ALTA', 'TLAHUAC', 'TLALPAN',
  'VENUSTIANO C.', 'XOCHIMILCO')
heat_alcaldia$Var2<-factor(heat_alcaldia$Var2, labels = abrev_delegacion)
                                      

heat_alcaldia$Var2<-paste0(heat_alcaldia$Var2, ' (', heat_alcaldia$Freq.y, ')')


plt<-ggplot(data=heat_alcaldia, aes(x=Var1, y=Var2, fill=Freq.x))+
  geom_tile()+
  geom_text(data=heat_alcaldia[heat_alcaldia$Freq.x>50,], aes(x=Var1, y=Var2, label=Freq.x), size=4)+
  scale_fill_viridis() + theme_bw()+
  ylab('')+xlab('')+theme(text = element_text(size = 20), legend.title = element_blank())+
  ggtitle('Número de robos en transporte público \n por hora del día y delegación',subtitle = 'Totales entre paréntesis')
ggsave('~/Documents/hora_delegacion.png',plot = plt, device = 'png',width = 20/2, height = 15/2)


heat_alcaldia_marg<-data@data
heat_alcaldia_marg<-as.data.frame(prop.table(table(heat_alcaldia_marg$hora,heat_alcaldia_marg$fields.alcaldiahechos ),2))
heat_alcaldia_marg<-base::merge.data.frame(heat_alcaldia_marg,
                                           totales_alcadia,
                                           by.x='Var2', by.y = 'Var1')

heat_alcaldia_marg$Var2<-paste0(heat_alcaldia_marg$Var2, ' (', heat_alcaldia$Freq.y, '=100%)')




plt<-ggplot(data=heat_alcaldia_marg, aes(x=Var1, y=Var2, fill=Freq.x))+
  geom_tile()+
  geom_text(data=heat_alcaldia_marg[heat_alcaldia_marg$Freq.x>0.05,], aes(x=Var1, y=Var2, label=paste0(round(Freq.x*100),'%' )))+
  scale_fill_viridis() + theme_bw()+
  ggtitle('Proporción marginal de robos en transporte público', 'Dada la delegación')+
  ylab('')+
  xlab('Hora del día')+
  theme(legend.position = 'none')
ggsave('Documents/hora_delegacion_marg.png',plot = plt, device = 'png')


data@data$fields.mes_ao<-as.Date(paste0(data@data$fields.mes_ao, '-01'))

mes_ao<-aggregate(data=data@data, rep(1, nrow(data@data))~as.Date(paste0(year(fields.fechainicio),'-',
                                                                         stringr::str_pad(month(fields.fechainicio),2,'left','0'),
                                                                         '-01')) , sum)
names(mes_ao)<-c('mes_ao', 'n')
library(ggrepel)
plt<-ggplot(data=mes_ao, aes(x=mes_ao, n), group=1)+
  geom_line()+
  ggtitle('Robo a bordo de pesero o transporte público \n con o sin violencia mensual')+
  geom_text_repel(aes(x=mes_ao, n, label=n))+
  theme_classic()+
  scale_x_date(name = '', date_breaks = '6 months', date_labels = ('%Y-%m'))+
  ylab('')+
  theme(text = element_text(size=15))
ggsave('~/Documents/serie_total.png',plot = plt, device = 'png')

mes_ao_alc<-aggregate(data=data@data, rep(1, nrow(data@data))~as.Date(paste0(year(fields.fechainicio),'-',
                                                                             stringr::str_pad(month(fields.fechainicio),2,'left','0'),
                                                                             '-01'))+fields.alcaldiahechos , sum)
names(mes_ao_alc)<-c('mes_ao','alcaldia', 'n')

plt<-ggplot(data=mes_ao_alc, aes(x=mes_ao, y=n, group=alcaldia, color=alcaldia))+
  geom_line()+
  #geom_text_repel(aes(x=mes_ao, y=n, group=alcaldia, color=alcaldia,label=n))+
  ggtitle('Robo a bordo de pesero o transporte público con o sin violencia mensual')+
  theme_classic()+
  scale_x_date(name = '', date_breaks = '1 year', date_labels = ('%Y-%m'))+
  ylab('')+
  theme(legend.position = 'none', text = element_text(size=15))+
  facet_wrap(~alcaldia)
ggsave('~/Documents/serie_delegacion.png',plot = plt, device = 'png')





matches<-over(lineas, data, returnList = T)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

ceros<-data.frame(datasetid='000',
                  recordid='000',
                  record_timestamp='000',
                  fields.categoriadelito='000',
                  fields.coloniahechos='000',
                  fields.mes_ao='000',             
                  fields.geopoint='000',
                  fields.lon='000',
                  fields.fechainicio='000',
                  fields.ao='000',
                  fields.calle1='000',
                  fields.calle='000',
                  fields.agencia='000',
                  fields.fiscalia='000',
                  fields.mes='000',
                  fields.lat='000',
                  fields.delito='000',
                  fields.unidadinvestigacion='000',
                  fields.alcaldiahechos='000',
                  geometry.type='000',
                  geometry.coordinates='000',
                  cluster='000',
                  diasemana='000',
                  hora='000')


rows_matches<-unlist(lapply(matches, nrow))
for( i in 1:length(matches)){
  if (unname(rows_matches[i])<1){
    matches[[i]]<-rbind.data.frame(matches[[i]],ceros,
                                   stringsAsFactors = F)
  }
}

for( i in 1:length(matches)){
  if (unname(rows_matches[i])<1){
    matches[[i]]$uno<-0
  } else{matches[[i]]$uno<-1}
}


matches<-lapply(matches,
                function(x) data.frame(aggregate(data=x,
                                                 cbind(diasemana,
                                                       hora,
                                                       fields.mes,
                                                       fields.ao,
                                                       fields.alcaldiahechos,
                                                       fields.calle)~1,getmode),
                                       n_robos=sum(x$uno)))


library(data.table)
matches<-rbindlist(matches)

lineas@data<-cbind.data.frame(lineas@data, matches)
data_lineas<-lineas@data
lineas_for_ggplot<-ggplot2::fortify(lineas)


library(dplyr)


names(data_lineas)[names(data_lineas) %in% 'id']<-'id_dataset'
data_lineas$id<-row.names(data_lineas)
lineas_for_ggplot<-left_join(lineas_for_ggplot, data_lineas, by='id')
mapa_df<-get_stamenmap(bbox = bbox,maptype = 'toner-lines' , zoom = 11)
mapa_rutas<-ggmap(mapa_df)+
  geom_polygon(data=lineas_for_ggplot,
               aes(x=long, y=lat,group=group,fill=log(n_robos+1),
               color=log(n_robos+1), alpha=0.3))+
  scale_color_viridis(option = 'magma')+
  scale_fill_viridis(option = 'magma')+
  theme_void()+
  theme(legend.text = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())
ggsave(filename = '~/Documents/mapa_rutas.pdf', device = 'pdf', plot = mapa_rutas)



peores_rutas<-lapply(split(data_lineas,
                           data_lineas$fields.alcaldiahechos), function(x)
                             head(x[order(x$n_robos, decreasing = T),c('id_dataset',
                                                                       'descrip',
                                                                       'fields.calle',
                                                                       'n_robos',
                                                                       'ruta_corre',
                                                                       'fields.alcaldiahechos')],50))


peores_rutas<-rbindlist(peores_rutas)
peores_rutas<-peores_rutas[peores_rutas$fields.alcaldiahechos!='1',]

peores_rutas<-peores_rutas[peores_rutas$n_robos>0,]






mapa_df<-get_stamenmap(bbox = bbox,maptype =  "terrain-background")


#mapa_peor_ruta<-get_stamenmap(lineas[lineas$id == peores_rutas$id_dataset[4],]@bbox+(deg_p_m*3000),
#                              maptype =  "toner-lite",
#                              zoom = 16)
library(plotly)

head(peores_rutas)
mapa_peores_rutas<-ggmap(mapa_df,maprange = F)+
  geom_polygon(data=lineas_for_ggplot[lineas_for_ggplot$id_dataset %in% peores_rutas$id_dataset,],
               aes(x=long, y=lat,group=group,text=paste('Desc. ruta: ', detalle,'Calle: ', fields.calle),
                   text1=n_robos, fill=fields.alcaldiahechos,color=log(n_robos+1)), alpha=0.3)+
  scale_color_viridis(option = 'magma')+
  guides(color=F)+
  #scale_fill_viridis(option = 'magma')+
  theme_void()+
  theme(legend.text = element_blank(), legend.title = element_blank())

mapa_peores_rutas<-ggplotly(mapa_peores_rutas, 
                            tooltip = c('text','text1', 'color'))


htmlwidgets::saveWidget(mapa_peores_rutas, '~/Documents/mapa_peores_rutas.html')


hdbscan()
peores_rutas<-split(peores_rutas,peores_rutas$fields.alcaldiahechos )

peores_rutas<-lapply(peores_rutas, function(x) head(x, 1))

peores_rutas<-rbind_list(peores_rutas)

write.csv(peores_rutas, '~/Documents/peores_rutas.csv')

puntos_clust<-data[data@data$cluster>0,]
library(rgeos)

conv_hull_clust<-lapply(split(puntos_clust,puntos_clust$cluster),
       function(x) gConvexHull(x, byid = F,id =unique(x$cluster) ))


conv_hull_clust<-do.call(rbind, conv_hull_clust)

png(filename = '~/Documents/metodo_clusters.png')
plot(conv_hull_clust, col=2:length(conv_hull_clust))
points(puntos_clust,col=rgb(0,0,0,0.3))
points(data,col=rgb(0,0,0,0.1))
dev.off()

data_clusters<-over(conv_hull_clust, puntos_clust, returnList = T)
data_clusters_lineas<-over(conv_hull_clust, lineas, returnList = T)


n_robos_cluster<-lapply(data_clusters,
       function(x) data.frame(n_robos=nrow(x)))


data_clusters<-lapply(data_clusters,
                function(x) data.frame(aggregate(data=x,
                                                 cbind(diasemana,
                                                       hora,
                                                       fields.mes,
                                                       fields.ao,
                                                       fields.coloniahechos,
                                                       fields.calle)~1,getmode)))

data_clusters_lineas<-lapply(data_clusters_lineas,
                      function(x) data.frame(aggregate(data=x,
                                                       descrip~1,getmode)))

n_robos_cluster<-rbindlist(n_robos_cluster)
data_clusters<-rbindlist(data_clusters)
data_clusters_lineas<-rbindlist(data_clusters_lineas)
data_clusters<-cbind.data.frame(data_clusters, data_clusters_lineas, n_robos_cluster)

data_clusters$id<-as.character(1:nrow(data_clusters))
conv_hull_clust<-fortify(conv_hull_clust)

conv_hull_clust<-left_join(conv_hull_clust, data_clusters, by='id')




mapa_df<-get_stamenmap(bbox = bbox, maptype = "terrain-lines")

head(conv_hull_clust)
###HTML CLUSTERS###
mapa_hora_calle<-ggmap(mapa_df)+
  geom_polygon(data=conv_hull_clust, aes(x=long, y=lat, group=group,
                                         fill=log(n_robos),
                                         text=paste("Año con más robos:",fields.ao,
                                                    "\n Mes con más robos:", fields.mes,
                                                    "\n Hora con más robos:", paste0(hora, ':00'),
                                                    "\n Día con más robos:", diasemana,
                                                    "\n Colonia con más robos:", fields.coloniahechos,
                                                    "\n Ruta:", descrip
                                                    )))+
  scale_fill_viridis(option='magma')+
  theme_void()+theme(legend.title =  element_blank())+
  ggtitle('Clusters de robos en transporte público')


mapa_hora_calle<-ggplotly(mapa_hora_calle, tooltip = c('text', 'text1') )

htmlwidgets::saveWidget(mapa_hora_calle, '~/Documents/info_clusters.html')



###gif CLUSTERS###

info_tabla<-conv_hull_clust[!duplicated(conv_hull_clust[conv_hull_clust$id,]),]

i<-1

library(gridExtra)


library(ggpmisc)

vars_tooltip<-c('fields.ao',
'fields.mes',
'hora',
'diasemana',
'fields.coloniahechos',
'fields.calle',
'descrip')

for( i in 1:nrow(info_tabla)){

label_tabla<-list(as.data.frame(t(info_tabla[i,vars_tooltip])))


label_tabla[[1]]<-cbind.data.frame(c('Año con más robos:',
                            'Mes con más robos:',
                            'Hora con más robos',
                            'Día con más robos:',
                            'Colonia con más robos:',
                            'Calle con más robos:',
                            'Ruta con más robos:'), label_tabla[[1]])

names(label_tabla[[1]])<-c('Variable','Moda')
mapa_hora_calle_gif<-ggmap(mapa_df)+
  geom_polygon(data=conv_hull_clust, aes(x=long, y=lat, group=group,
                                         fill=log(n_robos),
                                         alpha=id==i))+
  annotate(geom = "table",
           x = min(conv_hull_clust$long),
           y = min(conv_hull_clust$lat)-0.02,
           label = label_tabla, 
           vjust = 1, hjust = 0)+scale_fill_viridis(option='magma')+scale_alpha_discrete(guide=F)+
  theme_void()+theme(legend.title =  element_blank(), legend.text=element_blank())+
  ggtitle('Clusters de robos en transporte público')
ggsave(filename = paste0('~/Documents/gif_mapa/', 'mapa_clusters_',i, '.png' ), plot = mapa_hora_calle_gif,device = 'png')
       
}





