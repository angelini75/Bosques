rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
load(file = "results/segmentos.RData")
rp <- rp[-1,]
library(mapview)
library(tidyverse) # requiere entender https://r4ds.had.co.nz/transform.html
library(ggalluvial)

area.reg <- Y %>% select(region, estrato,area.estrato) %>% group_by(estrato) %>% 
  mutate(n=n()) %>% group_by(region) %>% unique() %>% 
  summarise(area.reg = sum(area.estrato))
area.prov <- Y %>% select(provincia, estrato,area.estrato) %>% 
  group_by(estrato) %>% mutate(n=n()) %>% group_by(provincia) %>% 
  unique() %>% summarise(area.prov = sum(area.estrato))

Y <- Y %>% select(-area.prov, -area.reg) %>% 
  left_join(area.reg, by = "region") %>% 
  left_join(area.prov, by = "provincia")

# Por ahora nos quedamos con los segmentos con datos completos
Y <- Y[complete.cases(Y),]
# N_h <- read.csv("csv/N_h.csv")
# N_h$estrato <- paste0(N_h$region, ".", N_h$provincia)
# Y <- merge(x = Y, y = N_h[,3:4], by = "estrato") 
tt <- Y %>% select(unlu, unsef, ha) %>% group_by(unlu,unsef) %>% 
  mutate(freq = n(), area = sum(ha)) %>% unique()

ggplot(data = tt,
       aes(axis1 = unlu, axis2 = unsef, y = area)) +
  scale_x_discrete(limits = c("unlu", "unsef"), expand = c(.1, .05)) +
  xlab("") + geom_alluvium(aes(fill = unlu)) + geom_stratum() +
geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() 
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")

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
N <- N %>% select(provincia, N) %>% unique() %>% group_by(provincia) %>%
  mutate(N.prov = sum(N)) %>% select(provincia, N.prov) %>% unique() %>% 
  merge(y = N, by = "provincia")
N <- N %>% select(region, N) %>% unique() %>% group_by(region) %>%
  mutate(N.reg = sum(N)) %>% select(region, N.reg) %>% unique() %>% 
  merge(y = N, by = "region")
N <- N %>% select(estrato, N) %>% unique() %>% group_by(estrato) %>%
  mutate(N.estr = sum(N)) %>% select(estrato, N.estr) %>% unique() %>% 
  merge(y = N, by = "estrato")
N <- N %>% select(estrato.clase, N.estr.cls = N, N.estr, N.prov, N.reg)


# creamos el mismo campo en Y
Y$estrato.clase <- paste0(Y$estrato, ".", Y$unsef)
# ahora traemos el numero de segmentos por estrato.clase
N$estrato.clase <- gsub(pattern = " ",replacement = "_", x = N$estrato.clase)
Y$estrato.clase <- gsub(pattern = " ",replacement = "_", x = Y$estrato.clase)
Y <- merge(x = Y, y = N, by = "estrato.clase", all.x = T)

# y_hi = área de los segmentos bien clasificados
Y$y_hi <- 0 # 0 para aquellos segmentos mal clasificados
Y$y_hi[Y$unsef == Y$unlu] <- Y$ha[Y$unsef == Y$unlu] # bien clasificados
Y$index <- ifelse(test = Y$unsef == Y$unlu, yes = 1, no = 0) # alternativa

# ahora empezamos a usar tidyverse
(y <- as.tbl(Y))

# calculamos el peso de cada muestra en base a la cantidad de veces clasificados
# de la misma manera
# y <- y %>%  mutate(w = 1/seg)
y <- y %>% group_by(estrato.clase) %>% mutate(pi_hi = n()/N.estr.cls, n = n())


# Resultado por estrato ########################################################
e <- y %>% group_by(estrato) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = unique(area.estrato),
            n = n(), # numero de segmentos muestreados dentro del estrato 
            N_h = first(N.estr), # numero total de segmentos por estrato
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # area bien clasificada
            hatp_h = hatt_h/A, # proporción bien clasificada
                               # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            var_hatt = (N_h^2) * var(y_hi)/n,# varianza de hatt
            error = qt(0.975, df = n - 1) * sqrt(var_hatt),
            hatt.ul.CI = hatt_h + error, # area upper limit confidence interval
            hatt.ll.CI = hatt_h - error, # area lower limit confidence interval
            hatp.ll.CI = hatt.ll.CI/A,
            hatp.ul.CI = hatt.ul.CI/A,
            region = unique(region),
            provincia = unique(provincia)
            )
estrato <- e %>% select(Estrato = estrato, 
                        LIArea = hatt.ll.CI,
                        Area = hatt_h,
                        LSArea = hatt.ul.CI,
                        LIProporcion = hatp.ll.CI,
                        Proporcion = hatp_h,
                        LSProporcion = hatp.ul.CI,
                        n = n,
                        Provincia = provincia,
                        Region = region)

ggplot(estrato, aes(x = Provincia, y = Proporcion)) +
  geom_bar(stat = "identity") + facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Proporción de área bien clasificada") +
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=90, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9))

# Resultado por región ########################################################
# y$region <- as.character(y$region)
# y$region[y$region == "pchh"] <- "pch"
# y$region[y$region == "pchs"]<- "pch"
e <- y %>% group_by(region) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = unique(area.reg),
            n = n(), # area total muestreada dentro de la region 
            N_h = unique(N.reg), # área total de la region
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # área bien clasificada
            hatp_h = hatt_h/A, # proporción bien clasificada
                               # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            var_hatt = (N_h^2) * var(y_hi)/n,# varianza de hatt
            error = qt(0.975, df = n - 1) * sqrt(var_hatt),
            hatt.ul.CI = hatt_h + error, # area upper limit confidence interval
            hatt.ll.CI = hatt_h - error, # area lower limit confidence interval
            hatp.ll.CI = hatt.ll.CI/A,
            hatp.ul.CI = hatt.ul.CI/A
            )
region <- e %>% select(Region = region, 
                        LIArea = hatt.ll.CI,
                        Area = hatt_h,
                        LSArea = hatt.ul.CI,
                        LIProporcion = hatp.ll.CI,
                        Proporcion = hatp_h,
                        LSProporcion = hatp.ul.CI,
                        n = n)

ggplot(region, aes(x = Region, y = Proporcion)) +
  geom_bar(stat = "identity") + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Proporción de área bien clasificada") +
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=90, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9))


# Resultado por provincia ########################################################
e <- y %>% group_by(provincia) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = unique(area.prov),
            n = n(), # area total muestreada dentro de la region 
            N_h = unique(N.prov), # área total de la region
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # área bien clasificada
            hatp_h = hatt_h/A, # proporción bien clasificada
            # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            var_hatt = (N_h^2) * var(y_hi)/n,# varianza de hatt
            error = qt(0.975, df = n - 1) * sqrt(var_hatt),
            hatt.ul.CI = hatt_h + error, # area upper limit confidence interval
            hatt.ll.CI = hatt_h - error, # area lower limit confidence interval
            hatp.ll.CI = hatt.ll.CI/A,
            hatp.ul.CI = hatt.ul.CI/A
  )
provincia <- e %>% select(Provincia = provincia, 
                       LIArea = hatt.ll.CI,
                       Area = hatt_h,
                       LSArea = hatt.ul.CI,
                       LIProporcion = hatp.ll.CI,
                       Proporcion = hatp_h,
                       LSProporcion = hatp.ul.CI,
                       n = n)

ggplot(provincia, aes(x = Provincia, y = Proporcion)) +
  geom_bar(stat = "identity") + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Proporción de área bien clasificada") +
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=90, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9))

