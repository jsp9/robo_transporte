#######CLUSTERING E INTERSECCION CON RUTAS########


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



matches<-rbindlist(matches)

lineas@data<-cbind.data.frame(lineas@data, matches)
data_lineas<-lineas@data
lineas_for_ggplot<-ggplot2::fortify(lineas)





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