#######PROCESAMIENTO DE SHAPES DE LINEAS######

trans<-readOGR(path.expand('./data/rutas-y-corredores-del-transporte-publico-concesionado/'),
               'rutas-y-corredores-del-transporte-publico-concesionado')




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


lineas<-gBuffer(lineas, byid = T, width = deg_p_m*30)

