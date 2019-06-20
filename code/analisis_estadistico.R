rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
load(file = "results/segmentos.RData")
rp <- rp[-1,]
library(mapview)
library(tidyverse) # requiere entender https://r4ds.had.co.nz/transform.html

# Por ahora nos quedamos con los segmentos con datos completos
Y <- Y[complete.cases(Y),]
# N_h <- read.csv("csv/N_h.csv")
# N_h$estrato <- paste0(N_h$region, ".", N_h$provincia)
# Y <- merge(x = Y, y = N_h[,3:4], by = "estrato") 

# resulta que cada clase dentro de cada estrato tiene una probabilidad 
# de ser elegido diferente, por ello hay que incluir el no. de segmentos 
# totales por estrato y clase
# cargamos los N de las provincias
N <- read.csv("csv/N.csv")
clases <- c( "TF_estable","OTF_estable", "OT_estable", "TF_perdida_1998-2006",
             "OTF_perdida_1998-2006", "TF_perdida_2006-2013",  
             "OTF_perdida_2006-2013", "TF_perdida_2013-2017",  
             "OTF_perdida_2013-2017", "No_es_posible_determinar")
class <- data.frame(clas.name = clases, clase = 1:10)
N <- merge(x = N, y = class, by = "clase")
N$estrato <- paste0(N$region, ".", N$provincia)
N$estrato.clase <- paste0(N$estrato, ".", N$clas.name)
# creamos el mismo campo en Y
Y$estrato.clase <- paste0(Y$estrato, ".", Y$unsef)
# ahora taemos el numero de segmentos por estrato.clase
N <- unique(N[, c("estrato.clase","N")])
N$estrato.clase <- gsub(pattern = " ",replacement = "_", x = N$estrato.clase)
Y$estrato.clase <- gsub(pattern = " ",replacement = "_", x = Y$estrato.clase)
Y <- merge(x = Y, y = N, by = "estrato.clase", all.x = T)

# y_hi = área de los segmentos bien clasificados
Y$y_hi <- 0 # 0 para aquellos segmentos mal clasificados
Y$y_hi[Y$unsef == Y$unlu] <- Y$ha[Y$unsef == Y$unlu] # bien clasificados
Y$ix <- ifelse(test = Y$unsef == Y$unlu, yes = 1, no = 0) # alternativa

# ahora empezamos a usar tidyverse
(y <- as.tbl(Y))
y$estrato <- y$estrato %>% as.factor
y <- y %>% group_by(estrato.clase) %>% mutate(pi_hi = n()/N)

# Agrupamos por estratos
e <- y %>% group_by(estrato, area.estrato) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = unique(area.estrato),
            # n_h = n(), # area total muestreada dentro del estrato 
            # N_h = unique(N), # área total del estrato
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # area bien clasificada
            hatp_h = hatt_h/A # proporción bien clasificada
            # var_y_h = var(y_hi), # varianza de y dentro del segmento
            #                      # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            # var_hatt = var_y_h / n_h # varianza de hatt
            )
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

# The variance of the estimated total of a stratum can be estimated by:
# var(\hat{t}_h) = S^2_h(y)/n_h 
# with S^2_h(y) the sample variance of y in stratum h

# Estimación de la varianza de \hat{t}_h y \hat{p}_h
# Agrupamos por estrato nuevamente
var_e <- y %>% group_by(estrato, area.estrato) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(n_h = sum(ha), # area total muestreada dentro del segmento 
            var_y_h = var(y_hi), # varianza de y dentro del segmento
                               # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            N_h = unique(area.estrato), # área total del estrato
            var_hatt = var_y_h/ pi_hi, # varianza de hatt
            hatp_h = var_hatt/N_h) 
var_e
