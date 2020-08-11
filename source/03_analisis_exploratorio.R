#######ANÁLISIS EXPLORATORIO########

#####¿Cómo está la distribución espacio temporal?####

data@data$fields.fechainicio<-strptime(data@data$fields.fechainicio,'%Y-%m-%dT%H:%M:%S' )

data@data$hora_dia<-hour(data@data$fields.fechainicio)

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
  ggsave(filename = paste0('./out/gif_densidades/mapa_', i, 'horas.png'), device = 'png',plot = mapa_datos) 
}

######GIF delegación hora######

centroide_alcaldia<-aggregate(data= data@data, cbind(fields.lon, fields.lat)~fields.alcaldiahechos, mean)

puntos_horas<-split(data@data, data@data$hora_dia)

max_conteos<-max(unlist(lapply(puntos_horas, nrow)))

shp_munis<-readOGR('/Users/pepe_opi/Downloads/889463674658_s/09_ciudaddemexico/conjunto de datos//', '09mun')

shp_munis<-spTransform(shp_munis, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 '))

shp_munis@data$NOMGEO<-c('AZCAPOTZALCO',
                    'COYOACAN',
                    'CUAJIMALPA DE MORELOS',
                    'GUSTAVO A MADERO',
                    'IZTACALCO',
                    'IZTAPALAPA',
                    'LA MAGDALENA CONTRERAS',
                    'MILPA ALTA',
                    'ALVARO OBREGON',
                    'TLAHUAC',
                    'TLALPAN',
                    'XOCHIMILCO',
                    'BENITO JUAREZ',
                    'CUAUHTEMOC',
                    'MIGUEL HIDALGO',
                    'VENUSTIANO CARRANZA'
)


shp_munis<-fortify(shp_munis, region = 'NOMGEO')

names(shp_munis)[names(shp_munis)=='id']<-'fields.alcaldiahechos'


shp_munis_data

head(shp_munis)

i<-1
table(shp_munis$piece)


for (i in 1:24){
  puntos_horas[[i]]<-left_join(shp_munis, aggregate(data=puntos_horas[[i]],
                                                    datasetid~fields.alcaldiahechos,
                                                    function(x) cbind(length(x))))
  puntos_horas[[i]]$datasetid[is.na(puntos_horas[[i]]$datasetid)]<-0.001
  puntos_horas[[i]]<-rbind.data.frame(puntos_horas[[i]], data.frame(long=c(NA, NA),
                                                                    lat=c(NA, NA),
                                                                    order=c(nrow(shp_munis), nrow(shp_munis)),
                                                                    hole=c(F, F),
                                                                    piece=c(1, 1),
                                                                    fields.alcaldiahechos=c(NA, NA),
                                                                    group=c(NA, NA),
                                                                            datasetid=c(0.001, max_conteos)))
  
  mapa_datos<-ggplot()+
    geom_polygon(data = puntos_horas[[i]],
               aes(x = long, y = lat,group=fields.alcaldiahechos,
                   color=log(datasetid), fill=log(datasetid)))+
    theme_void()+
    theme(legend.position = 'none')+
    scale_color_viridis()+
    scale_fill_viridis()+
    ggtitle(paste('Número de robos en transporte público a las:',
                  i,
                  ':00 horas (',
                  floor(sum(puntos_horas[[i]]$datasetid[!duplicated(puntos_horas[[i]]$fields.alcaldiahechos)])),
                  ' totales)'))
  ggsave(filename = paste0('./out/gif_conteos/mapa_', i, 'horas.png'), device = 'png',plot = mapa_datos) 
}


prop.table(table(data@data$fields.mes))*100 #Distribución por mes

####CALENDAR PLOT####
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
ggsave('./out/mes_ao.png',plot = plt, device = 'png', height = 4.5*1.5, width = 15.5*1.5)
######



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
ggsave('./out/dia_hora.png',plot = plt, device = 'png', height = 4.5*1.5, width = 15.5*1.5)


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
ggsave('./out/hora_delegacion.png',plot = plt, device = 'png',width = 20/2, height = 15/2)


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
ggsave('./out/hora_delegacion_marg.png',plot = plt, device = 'png')


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
ggsave('./out//serie_total.png',plot = plt, device = 'png')

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
ggsave('./out/serie_delegacion.png',plot = plt, device = 'png')
