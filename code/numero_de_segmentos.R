# cálculo de segmentos por archivo shp
library(rgdal)
library(raster)
library(mapview)
library(dplyr)
library(rgeos)

rm(list = ls())
ref.files <- paste0("shp/segmentos_originales/",list.files(path = "shp/segmentos_originales/", pattern = "4326.shp"))
ref.files
y <- list()
for(i in seq_along(ref.files)){
y[[i]] <- readOGR(ref.files[i])
}
n <- as.vector(NULL)
for(i in seq_along(y)){n <- append(n,dim(y[[i]])[1])}
n
e <- substr(ref.files,26,35)
t <- data_frame(e,n)

t$r <- substr(t$e,8,10)

t$r[4] <- "pch"
t$r[10] <- "esp"

t$e <- substr(t$e,1,2)


View(t[,c(3,1,2)])
