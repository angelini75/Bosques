rm(list = ls())

load(file = "results/zoon.data.RData")
names(r)[4] <- "stb"
####
### Compiling shapes files with segments and clsses information
library(rgdal)
library(raster)
library(mapview)

ref.files <- paste0("shp/",list.files(path = "shp/", pattern = "4326.shp"))

y <- list()
for(i in seq_along(ref.files)){
  y[[i]] <- readOGR(ref.files[i])
}
names(y) <- c("esp", "pchh", "pchs", "smi", "stb")
mapview(y[["esp"]]) + mapview(y[["pchh"]]) + mapview(y[["pchs"]]) + 
  mapview(y[["smi"]]) + mapview(y[["stb"]])




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
                "Santiago del Estero", "La Pampa", "San juan")
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

# for(i in names(y)[2:5]){
#   y[[i]]@data <-  data.frame(y[[i]]@data, r[[i]][match(y[[i]]@data$id, r[[i]]$id),])
# }
# 
# columns <- c("IDENTIF", "Provincia","region","categoria", "id","variable.x","value.x")
# 
# for(i in names(y)[2:5]){
#   y[[i]]@data <-  y[[i]]@data[,columns]
# }
