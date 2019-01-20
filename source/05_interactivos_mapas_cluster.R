##########INTERACTIVOS Y MAPAS CLUSTERS##########
mapa_df<-get_stamenmap(bbox = bbox,maptype =  "terrain-background")



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


htmlwidgets::saveWidget(mapa_peores_rutas,file =  paste0(getwd(),"/out/", 'mapa_peores_rutas.html'))


peores_rutas<-split(peores_rutas,peores_rutas$fields.alcaldiahechos )

peores_rutas<-lapply(peores_rutas, function(x) head(x, 1))

peores_rutas<-rbind_list(peores_rutas)

write.csv(peores_rutas, './out/peores_rutas.csv')

puntos_clust<-data[data@data$cluster>0,]


conv_hull_clust<-lapply(split(puntos_clust,puntos_clust$cluster),
                        function(x) gConvexHull(x, byid = F,id =unique(x$cluster) ))


conv_hull_clust<-do.call(rbind, conv_hull_clust)

png(filename = './out/metodo_clusters.png')
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

htmlwidgets::saveWidget(mapa_hora_calle,
                        paste0(getwd(),"/out/",'info_clusters.html'))



###gif CLUSTERS###

info_tabla<-conv_hull_clust[!duplicated(conv_hull_clust[conv_hull_clust$id,]),]

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
  ggsave(filename = paste0('./out/gif_mapa/', 'mapa_clusters_',i, '.png' ), plot = mapa_hora_calle_gif,device = 'png')
  
}





