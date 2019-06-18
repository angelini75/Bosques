rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
load(file = "results/segmentos.RData")
rp <- rp[-1,]
library(mapview)
library(tidyverse) # requiere entender https://r4ds.had.co.nz/transform.html

# Por ahora nos quedamos con los segmentos con datos completos
Y <- Y[complete.cases(Y),]

# # primero creamos una tabla con los estratos (e) y sus superficies
# name(Y)
# (e <- unique(Y[, c("estrato", "provincia", "region", "area.estrato")]))
# rownames(e) <- 1:length(e[,1])

# A_h es el área del estrato
names(e)[4] <- "A_h"

# y_hi = área de los segmentos bien clasificados
Y$y_hi <- 0 # 0 para aquellos segmentos mal clasificados
Y$y_hi[Y$unsef == Y$unlu] <- Y$ha[Y$unsef == Y$unlu]

# veamos el porcentage de superficie total bien clasificada (sólo informativo)
sum(Y$y_hi)/sum(Y$ha)

# ahora empezamos a usar tidyverse
(y <- as.tbl(Y))
y$estrato <- y$estrato %>% as.factor

# Agrupamos por estratos
e <- y %>% group_by(estrato, region, provincia, area.estrato) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(n_h = sum(ha), # area total muestreada dentro del segmento 
            N_h = unique(area.estrato), # cantidad de segmentos
            pi_hi = n_h/N_h, # probabilidad de inclusión del segmento
            hatt_h = sum(y_hi / pi_hi), # area bien clasificada
            hatp_h = hatt_h/N_h) # proporción bien clasificada
e

# mostrar en el mapa
rp@data$estrato <- as.character(rp@data$estrato)
e$estrato <- as.character(e$estrato)
rp@data <- data.frame(rp@data, e[match(rp@data$estrato, e$estrato),])

mapview(rp, zcol = "hatp_h", at = seq(0.4, 1,length.out = 6))

# estimación de \hat{t}_r y \hat{p}_r por region
e$region_t <- as.character(e$region)
e$region_t[e$region_t == "pchh"]<- "pch"
e$region_t[e$region_t == "pchs"]<- "pch"
(r <- e %>% group_by(region_t) %>% 
  summarise(hatt_r = sum(hatt_h),
            hatp_r = hatt_r/sum(area.estrato)))

rp@data <- data.frame(rp@data, r[match(rp@data$region, r$region),])
mapview(rp, zcol = "hatp")

# estimación de \hat{t}_p y \hat{p}_p por provincia
(p <- e %>% group_by(provincia) %>% 
    summarise(hatt_p = sum(hatt_h),
              hatp_p = hatt_p/sum(area.estrato)))


# estimación de hat{t} y hat{p} total
(e %>% group_by() %>% 
    summarise(hatt_p = sum(hatt_h),
              hatp_p = hatt_p/sum(area.estrato)))

