rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
load(file = "results/zoon.data.RData")
####
### Compiling shapes files with segments and clsses information
library(rgdal)
library(raster)
library(mapview)
library(dplyr)
library(rgeos)

ref.files <- paste0("shp/",list.files(path = "shp/", pattern = "4326.shp"))

y <- list()
for(i in seq_along(ref.files)){
  y[[i]] <- readOGR(ref.files[i])
}
names(y) <- c("esp", "pchh", "pchs", "smi", "stb")
#mapview(y[["esp"]]) + mapview(y[["pchh"]]) + mapview(y[["pchs"]]) + 
#  mapview(y[["smi"]]) + mapview(y[["stb"]])

for(i in seq_along(y)){
  y[[i]]@data <- y[[i]]@data[,c("IDENTIF", "PROVINCIA", "REGION", "MAJORITY")]
  y[[i]]@data$IDENTIF <- as.numeric(as.character(y[[i]]@data$IDENTIF))
  names(y[[i]]@data) <- c("id", "prov.id", "reg.id", "unsef")
}


regiones <- c("smi", "stb", "pchh", "pchs", "esp", "pch")
reg <- data.frame(region = regiones, reg.id = 1:6)

provincias <- c("Corrientes", "Jujuy", "La Rioja", "Formosa", "Misiones", 
                "Catamarca", "Salta", "San Luis", "Entre Ríos", "Tucumán",
                "Santa Fe", "Buenos Aires", "Chaco", "Córdoba", 
                "Santiago del Estero", "La Pampa", "San Juan")
prov <- data.frame(provincia = provincias, prov.id = 1:17)

for(i in seq_along(y)){
  y[[i]]@data <-  data.frame(y[[i]]@data, 
                             prov[match(y[[i]]@data$prov.id, prov$prov.id),])
  y[[i]]@data <- y[[i]]@data[,-length(y[[i]]@data)]
}

for(i in seq_along(y)){
  y[[i]]@data <-  data.frame(y[[i]]@data, reg[match(y[[i]]@data$reg.id, reg$reg.id),])
  y[[i]]@data <- y[[i]]@data[,-length(y[[i]]@data)]
}

prov.pchh <- c(1,4,11,13)
for(i in seq_along(y)){
  y[[i]]@data$region[y[[i]]@data$region == "pch" & 
                       y[[i]]@data$prov.id %in% prov.pchh] <- "pchh" 
  }
for(i in seq_along(y)){
  y[[i]]@data$region[y[[i]]@data$region == "pch"] <- "pchs" 
}

clases <- c( "TF_estable","OTF_estable", "OT_estable", "TF_perdida_1998-2006",
             "OTF_perdida_1998-2006", "TF_perdida_2006-2013",  
             "OTF_perdida_2006-2013", "TF_perdida_2013-2017",  
             "OTF_perdida_2013-2017", "No_es_posible_determinar")
class <- data.frame(clase = clases, cla.id = 1:10)

for(i in seq_along(y)){
  y[[i]]@data <-  data.frame(y[[i]]@data, class[match(y[[i]]@data$unsef, class$cla.id),])
  y[[i]]@data <- y[[i]]@data[,-length(y[[i]]@data)]
}
for(i in seq_along(y)){
  names(y[[i]]@data) <- c("id", "prov.id", "reg.id", "cla.id", "provincia", 
                          "region", "unsef" )
}

for(i in names(y)){
  y[[i]]@data <-  data.frame(y[[i]]@data, r[[i]][match(y[[i]]@data$id, r[[i]]$id),])
}

columns <- c("id", "provincia","region", "unsef", "unlu","seg")

for(i in names(y)){
  y[[i]]@data <-  y[[i]]@data[,columns]
}

for(i in names(y)){
  y[[i]]@data$estrato <-  paste0(y[[i]]@data$region,".",y[[i]]@data$provincia)
}

# calcular area de cada polígono
# reproyectar
posgar98 <- CRS("+proj=tmerc +lat_0=-90 +lon_0=-66 +k=1 +x_0=3500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
wgs84 <- CRS("+init=epsg:4326")

for(i in names(y)){
  y[[i]] <- spTransform(y[[i]],posgar98)
  y[[i]]$ha <- area(y[[i]])/10000
}

# Para el análisis estadístico hace falta la superficie de las regiones, 
# de las provincias y de las regiones por provincia.

# seleccionar provincias y calcular area
p <- readOGR("shp/gadm36_ARG_1.shp")
p <- spTransform(p, posgar98)
p <- p[which(p$NAME_1 %in% provincias),]
p <- p[,4]
names(p) <- "provincia"
p@data <-  data.frame(p@data, 
                      prov[match(p@data$provincia, prov$provincia),])[,-2]
p$area.prov <- area(p)/10000

# regiones y calcular area
R <- shapefile("shp/regiones_forestales_2.shp")
R <- spTransform(R, posgar98)
R$REGIONES <- c("stb", "pch", "esp", "esp", "smi")
R$area.reg <- area(R)/10000
R@data <- R@data[,c(1,5)]
names(R)[1] <- c("regiones")
# intersectar provincias y regiones y calcular area
rp <- intersect(R,p)
rp$area.estrato <- area(rp)/10000 #superficie de regiones por provincia-region

rp$estrato <- paste0(rp$regiones,".",rp$provincia)

rp@data <- rp@data[,c("estrato", "area.reg", "area.prov", "area.estrato")]
#resultado
mapview(rp, zcol = "estrato")

# Ahora cruzamos segmentos con estratos
for (i in seq_along(y)) {
y[[i]]@data <- merge(x = y[[i]]@data, y = rp, by = "estrato", all.x = TRUE)
y[[i]]@data <- y[[i]]@data[c("id", "estrato", "provincia", "region", "unsef",
                             "unlu", "seg", "ha", "area.reg", "area.prov", 
                             "area.estrato")]
}
# y ahora juntamos todos los segmentos, (en un shape y) en una tabla
# library(maptools) no finalizado
# spRbind(y[[1]],y[[2]])

for (i in seq_along(y)) {
  y[[i]] <- as.data.frame(y[[i]])
}

Y <- rbind(y[[1]], y[[2]], y[[3]], y[[4]], y[[5]])

# fix 
Y[Y$id == 141464,8:10] <- Y[Y$id == 140038,8:10]
Y[Y$id == 2649,8:10] <- Y[Y$id == 226475,8:10]

rm(list = ls()[-19])
save.image("~/git/Bosques/results/segmentos.RData")
