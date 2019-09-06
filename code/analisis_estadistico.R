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

# Hay 11 segmentos sin clasificar.
# Se incluye aquí las categorías definidas por un solo intérprete
Y[!complete.cases(Y),]
Y[Y$id == 28301372,c("unlu","seg")] <- c("TF_perdida_2006-2013", 1)
Y[Y$id == 21881021,c("unlu","seg")] <- c("TF_estable", 1)
Y[Y$id == 24630709,c("unlu","seg")] <- c("OTF_perdida_2013-2017", 1)
Y[Y$id == 30779554,c("unlu","seg")] <- c("OT_estable", 1)
Y[Y$id ==   212886,c("unlu","seg")] <- c("OTF_perdida_2006-2013", 1)
Y[Y$id ==   191067,c("unlu","seg")] <- c("OT_estable", 1)
Y[Y$id ==    74981,c("unlu","seg")] <- c("OTF_estable", 1)
Y[Y$id ==    16940,c("unlu","seg")] <- c("OT_estable", 1)
Y[Y$id ==    99900,c("unlu","seg")] <- c("TF_perdida_2013-2017", 1)
Y[Y$id ==   149759,c("unlu","seg")] <- c("OTF_estable", 1)
Y[Y$id ==   117388,c("unlu","seg")] <- c("OTF_estable", 1)
# Por ahora nos quedamos con los segmentos con datos completos
Y <- Y[complete.cases(Y),]
# N_h <- read.csv("csv/N_h.csv")
# N_h$estrato <- paste0(N_h$region, ".", N_h$provincia)
# Y <- merge(x = Y, y = N_h[,3:4], by = "estrato") 
# tt <- Y %>% select(unlu, umsef, ha) %>% group_by(unlu,umsef) %>% 
#   mutate(freq = n(), area = sum(ha)) %>% unique()
# 
# ggplot(data = tt,
#        aes(axis1 = unlu, axis2 = umsef, y = area/1000)) +
#   scale_x_discrete(limits = c("unlu", "umsef"), expand = c(.1, .05)) +
#   xlab("") + geom_alluvium(aes(fill = unlu)) + geom_stratum() +
# geom_text(stat = "stratum", label.strata = TRUE) +
#   theme_minimal() 
  # ggtitle("passengers on the maiden voyage of the Titanic",
  #         "stratified by demographics and survival")

# Cálculo de número de muestras por clase y por estrato

write.csv(
  Y %>% group_by(region, provincia, umsef) %>% summarise(n = n()) %>% 
    unique() %>% arrange(region,provincia,umsef) %>% as.data.frame(), 
  file = "results/segmentos_x_clase.csv"
)


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
Y$estrato.clase <- paste0(Y$estrato, ".", Y$umsef)
# ahora traemos el numero de segmentos por estrato.clase
N$estrato.clase <- gsub(pattern = " ",replacement = "_", x = N$estrato.clase)
Y$estrato.clase <- gsub(pattern = " ",replacement = "_", x = Y$estrato.clase)
Y <- merge(x = Y, y = N, by = "estrato.clase", all.x = T)

# y_hi = área de los segmentos correctamente clasificados
Y$y_hi <- 0 # 0 para aquellos segmentos mal clasificados
Y$y_hi[Y$umsef == Y$unlu] <- Y$ha[Y$umsef == Y$unlu] # correctamente clasificados
Y$index <- ifelse(test = Y$umsef == Y$unlu, yes = 1, no = 0) # alternativa

# ahora empezamos a usar tidyverse
(y <- as.tbl(Y))

# calculamos el peso de cada muestra en base a la cantidad de veces clasificados
# de la misma manera
# y <- y %>%  mutate(w = 1/seg)
y <- y %>% group_by(estrato.clase) %>% mutate(pi_hi = n()/N.estr.cls, n = n())

# Unificamos pchh y pchs
y$region <- as.character(y$region)
y$region[y$region == "pchh"] <- "pch"
y$region[y$region == "pchs"] <- "pch"

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
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # area correctamente clasificada
            hatp_h = hatt_h/A, # proporción correctamente clasificada
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

estrato$Region <- gsub(estrato$Region, pattern = "esp", replacement = "Espinal")
estrato$Region <- gsub(estrato$Region, pattern = "pch", replacement = "Parque Chaqueño")
estrato$Region <- gsub(estrato$Region, pattern = "smi", replacement = "Selva Misionera")
estrato$Region <- gsub(estrato$Region, pattern = "stb", replacement = "Selva Tucumano-Boliviana")



png("results/estr.prop.png", width = 2000, height = 1400, res = 200)
ggplot(estrato, aes(x = Provincia, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) +
  facet_wrap(~Region, scales = "free_x") + coord_cartesian(ylim = c(0,1)) +
  ggtitle("") + ylab("Proporción de área correctamente clasificada") +  
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=90, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9))+ 
  labs(tag = "a") 
dev.off()

estrato$LIArea <- estrato$LIArea/1000
estrato$Area <- estrato$Area/1000
estrato$LSArea <- estrato$LSArea/1000

png("results/estr.area.png", width = 2000, height = 1400, res = 200)
ggplot(estrato, aes(x = Provincia, y = Area)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) +
  facet_wrap(~Region, scales = "free") +
  ggtitle("") + ylab("Área correctamente clasificada (ha x 1000)") +
  geom_errorbar(aes(ymin = LIArea, ymax = LSArea), width = 0.2) + 
  theme(text = element_text(size=16), 
        axis.text.x = element_text(angle=90, vjust=0)) + labs(tag = "b") 
dev.off()

# Resultado por región ########################################################
e <- y %>% group_by(region) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = sum(unique(area.reg)),
            n = n(), # area total muestreada dentro de la region 
            N_h = sum(unique(N.reg)), # área total de la region
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # área correctamente clasificada
            hatp_h = hatt_h/A, # proporción correctamente clasificada
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

region$Region <- gsub(region$Region, pattern = "esp", replacement = "Espinal")
region$Region <- gsub(region$Region, pattern = "pch", replacement = "Parque Chaqueño")
region$Region <- gsub(region$Region, pattern = "smi", replacement = "Selva Misionera")
region$Region <- gsub(region$Region, pattern = "stb", replacement = "Selva Tucumano-Boliviana")

png("results/region.prop.png", width = 2000, height = 1100, res = 200)
ggplot(region, aes(x = Region, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Proporción de área correctamente clasificada") + xlab("Región") +
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=0, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9)) + 
  labs(tag = "a") 
dev.off()

region$LIArea <- region$LIArea/1000
region$Area <- region$Area/1000
region$LSArea <- region$LSArea/1000

region$LIProporcion <- region$LIProporcion * 100
region$Proporcion <- region$Proporcion * 100
region$LSProporcion <- region$LSProporcion * 100

kable(region, format = "latex", digits = 1)
png("results/region.area.png", width = 2000, height = 1100, res = 200)
ggplot(region, aes(x = Region, y = Area)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Área correctamente clasificada (ha x 1000)") + xlab("Región") +
  geom_errorbar(aes(ymin = LIArea, ymax = LSArea), width = 0.2) + 
  theme(text = element_text(size=16), 
        axis.text.x = element_text(angle=0, vjust=0)) + labs(tag = "b") 
dev.off()


# Resultado por provincia ########################################################
e <- y %>% group_by(provincia) %>% 
  filter(provincia %in% c("Salta", "Santiago del Estero", "Chaco", "Formosa")) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = unique(area.prov),
            n = n(), # area total muestreada dentro de la region 
            N_h = unique(N.prov), # área total de la region
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # área correctamente clasificada
            hatp_h = hatt_h/A, # proporción correctamente clasificada
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
png("results/prov.prop.png", width = 2000, height = 1300, res = 200)
ggplot(provincia, aes(x = Provincia, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Proporción de área correctamente clasificada") +
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=90, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9)) +
  labs(tag = "a") 
dev.off()

provincia$LIArea <- provincia$LIArea/1000
provincia$Area <- provincia$Area/1000
provincia$LSArea <- provincia$LSArea/1000

png("results/prov.area.png", width = 2000, height = 1300, res = 200)
ggplot(provincia, aes(x = Provincia, y = Area)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Área correctamente clasificada (ha x 1000)") +
  geom_errorbar(aes(ymin = LIArea, ymax = LSArea), width = 0.2) + 
  theme(text = element_text(size=16), 
        axis.text.x = element_text(angle=90, vjust=0))  + labs(tag = "b") 
dev.off()

region[,2:4] <- region %>% select(LIArea,Area,LSArea) %>% 
  round(0)
region[,5:7] <- region %>% select(LIProporcion,Proporcion,LSProporcion) %>% 
  round(3)
provincia[,2:4] <- provincia %>% select(LIArea,Area,LSArea) %>% 
  round(0)
provincia[,5:7] <- provincia %>% select(LIProporcion,Proporcion,LSProporcion) %>% 
  round(3)
estrato[,2:4] <- estrato %>% select(LIArea,Area,LSArea) %>% 
  round(0)
estrato[,5:7] <- estrato %>% select(LIProporcion,Proporcion,LSProporcion) %>% 
  round(3)

write_csv(region, "results/region.csv")
write_csv(provincia, "results/provincia.csv")
write_csv(estrato, "results/estrato.csv")


# estimación para toda el área completa
y %>% select(area.reg) %>% unique() %>% sum()
y %>% select(N.reg) %>% unique() %>% sum()
e <- y %>% group_by() %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(A = 101149015,
            n = n(), # area total muestreada dentro de la region 
            N_h = 5917429, # área total de la region
            hatt_h = sum(y_hi/pi_hi , na.rm = TRUE), # área correctamente clasificada
            hatp_h = hatt_h/A, # proporción correctamente clasificada
            # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            var_hatt = (N_h^2) * var(y_hi)/n,# varianza de hatt
            error = qt(0.975, df = n - 1) * sqrt(var_hatt),
            hatt.ul.CI = hatt_h + error, # area upper limit confidence interval
            hatt.ll.CI = hatt_h - error, # area lower limit confidence interval
            hatp.ll.CI = hatt.ll.CI/A,
            hatp.ul.CI = hatt.ul.CI/A
  )
(total <- e %>% select(LIArea = hatt.ll.CI,
                      Area = hatt_h,
                      LSArea = hatt.ul.CI,
                      LIProporcion = hatp.ll.CI,
                      Proporcion = hatp_h,
                      LSProporcion = hatp.ul.CI,
                      n = n))
write_csv(total, "results/total.csv")


############ Producto 3 - informe final #############################
# Resultado por estrato ########################################################
clases <- tibble(clases = c( "TF_estable","OTF_estable", "OT_estable", 
                             "TF_perdida_1998-2006",
                             "OTF_perdida_1998-2006", "TF_perdida_2006-2013",  
                             "OTF_perdida_2006-2013", "TF_perdida_2013-2017",  
                             "OTF_perdida_2013-2017", "No_es_posible_determinar"),
                 code = 1:10)


y <- y %>% filter(unlu != "No_es_posible_determinar")
y <- y %>% left_join(y = class, by = c("unlu" = "clas.name"))  
y <- y %>% rename(code.unlu = clase)
y <- y %>% left_join(y = class, by = c("umsef"="clas.name"))  
y <- y %>% rename(code.umsef = clase)
y <- y %>% mutate(hap = (ha/pi_hi)/1000,
                  happ = (hap*1000/101149015)*100)

y$code.unlu <- as.factor(y$code.unlu)
y$code.umsef <- as.factor(y$code.umsef)

y %>% group_by(code.umsef, code.unlu) %>% select(code.unlu, code.umsef, happ)  %>% 
  summarise(sum = sum(happ, na.rm = T)) %>% 
  ggplot(aes(x = code.unlu, y = code.umsef, fill = sum)) + geom_tile() +
  scale_fill_gradient2(low = "#fef0d9",
                       high = "#b30000", space = "Lab",
                       na.value = "transparent", trans = "log",
                       #name = "x1000 ha"
                       guide=FALSE) + 
  geom_text(aes(label=paste0(round(sum,2),"%"))) + labs(x = "Clases de referencia",
                                            y = "Clases del mapa")


y %>% group_by(region) %>% 
  mutate(area = sum(hap, na.rm = T)) %>% 
  group_by(code.umsef, code.unlu, region) %>%                                                             
  select(region, code.unlu, code.umsef, hap, area)  %>% 
    summarise(sum = sum(hap, na.rm = T),
              perc = 100*(sum/unique(area))) %>% 
    ggplot(aes(x = code.unlu, y = code.umsef, fill = sum)) + geom_tile() +
    scale_fill_gradient2(low = "#fef0d9",
                         high = "#b30000", space = "Lab",
                         na.value = "transparent", trans = "log",
                         guide=FALSE) + 
    geom_text(aes(label=paste0(round(sum,2)," ha"))) + labs(x = "Clases de referencia",
                                                          y = "Clases del mapa") + 
  facet_wrap(~region)

  
y %>% group_by(provincia) %>%
  # arrage
  filter(provincia == "Chaco"| provincia == "Santiago del Estero" |
           provincia == "Formosa" | provincia == "Salta") %>% 
  mutate(area = sum(hap, na.rm = T)) %>% 
  group_by(code.umsef, code.unlu, provincia)  %>% 
  select(provincia, code.unlu, code.umsef, hap, area)  %>% 
  summarise(sum = sum(hap, na.rm = T),
            perc = 100*(sum/unique(area))) %>% 
  # Plot
  ggplot(aes(x = code.unlu, y = code.umsef, fill = perc)) + geom_tile() +
  scale_fill_gradient2(low = "#fef0d9",
                       high = "#b30000", space = "Lab",
                       na.value = "transparent", trans = "log",
                       guide=FALSE) + 
  geom_text(aes(label=paste0(round(perc,2),"%"))) + labs(x = "Clases de referencia",
                                                         y = "Clases del mapa") + 
  facet_wrap(~provincia)

# export to excell
write.csv(row.names = F,
  y %>% #group_by(region) %>% filter(region == "smi") %>% 
  group_by(code.umsef, code.unlu) %>%                                                             
    mutate(area = sum(hap, na.rm = T)) %>% 
    select(code.unlu, code.umsef, hap) %>% 
    summarise(sum = sum(hap, na.rm = T)), "results/matriz_global.csv")

write.csv(row.names = F,
          y %>% #group_by(region) %>% filter(region == "smi") %>% 
            group_by(code.umsef, code.unlu, provincia) %>% 
            filter(provincia %in% c("Santiago del Estero", "Salta", "Chaco", "Formosa")) %>% 
            mutate(area = sum(hap, na.rm = T)) %>% 
            select(code.unlu, code.umsef, hap) %>% 
            summarise(sum = sum(hap, na.rm = T)), "results/matriz_provincias.csv")


#### Matrices por region ####
clases <- tibble(clases = c( "TF_estable","OTF_estable", "OT_estable", 
                             "TF_perdida_1998-2006",
                             "OTF_perdida_1998-2006", "TF_perdida_2006-2013",  
                             "OTF_perdida_2006-2013", "TF_perdida_2013-2017",  
                             "OTF_perdida_2013-2017", "No_es_posible_determinar"),
                 code = 1:10)


y <- y %>% filter(unlu != "No_es_posible_determinar")
y <- y %>% left_join(y = class, by = c("unlu" = "clas.name"))  
y <- y %>% rename(code.unlu = clase)
y <- y %>% left_join(y = class, by = c("umsef"="clas.name"))  
y <- y %>% rename(code.umsef = clase)


y$code.unlu <- as.factor(y$code.unlu)
y$code.umsef <- as.factor(y$code.umsef)



e <- y %>% group_by(region, code.unlu ) %>% 
  # pi_i es la probabilidad de inclusión del segmento i en el estrato h
  # \hat{t)_h = sum{i=1}^{n_h} y_{hi}/pi_{hi} with 0
  # pi_{hi} = n_h/N_h 
  # n_h is sample size of stratum h, 
  # N_h total number of segments in stratum h, and 
  # y_{hi} the area correctly classified in segment i of stratum h.
  summarise(#A = sum(unique(area.estrato)),
            n = n(), # area total muestreada dentro de la region 
            N_h = sum(unique(N.estr.cls)), # área total de la region
            hatt_h = sum(ha/pi_hi , na.rm = TRUE), # área correctamente clasificada
            #hatp_h = hatt_h/A, # proporción correctamente clasificada
            # 1/(n-1) sum_{i=1}^n (y_i - \bar{y})^2.
            var_hatt = (N_h^2) * var(ha)/n,# varianza de hatt
            error = qt(0.975, df = n - 1) * sqrt(var_hatt),
            hatt.ul.CI = hatt_h + error, # area upper limit confidence interval
            hatt.ll.CI = hatt_h - error # area lower limit confidence interval
            ) %>% 
  select(code.unlu, n, -N_h, superficie = hatt_h, -var_hatt, error, LS = hatt.ul.CI, LI = hatt.ll.CI) %>% 
  mutate(code.unlu, superficie = round(superficie/1000,1),
         LS = round(LS/1000, 1), LI = round(LI/1000,1), error = round(error/1000,1),
         tasa = round(error/superficie, 3)*100)

library(readxl)
write_excel_csv(e, "results/regiones.xlsx")

region <- e %>% select(Region = region, 
                       LIArea = hatt.ll.CI,
                       Area = hatt_h,
                       LSArea = hatt.ul.CI,
                       LIProporcion = hatp.ll.CI,
                       Proporcion = hatp_h,
                       LSProporcion = hatp.ul.CI,
                       n = n)

region$Region <- gsub(region$Region, pattern = "esp", replacement = "Espinal")
region$Region <- gsub(region$Region, pattern = "pch", replacement = "Parque Chaqueño")
region$Region <- gsub(region$Region, pattern = "smi", replacement = "Selva Misionera")
region$Region <- gsub(region$Region, pattern = "stb", replacement = "Selva Tucumano-Boliviana")

png("results/region.prop.png", width = 2000, height = 1100, res = 200)
ggplot(region, aes(x = Region, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.5) + #facet_grid(~Region, scales = "free_x") + 
  ggtitle("") + ylab("Proporción de área correctamente clasificada") + xlab("Región") +
  geom_errorbar(aes(ymin = LIProporcion, ymax = LSProporcion), width = 0.2) + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=0, vjust=0)) +
  geom_text(aes(label = n, vjust=3.5), position = position_dodge(width=0.9)) + 
  labs(tag = "a") 
dev.off()

y$seg <- as.factor(y$seg)

########## análisis de concordancia entre interpretes #######
clases <- tibble(clases = c( "TF_estable","OTF_estable", "OT_estable", 
                             "TF_perdida_1998-2006",
                             "OTF_perdida_1998-2006", "TF_perdida_2006-2013",  
                             "OTF_perdida_2006-2013", "TF_perdida_2013-2017",  
                             "OTF_perdida_2013-2017", "No_es_posible_determinar"),
                 code = 1:10)


y <- y %>% filter(unlu != "No_es_posible_determinar")
y <- y %>% left_join(y = class, by = c("unlu" = "clas.name"))  
y <- y %>% rename(code.unlu = clase)
y <- y %>% left_join(y = class, by = c("umsef"="clas.name"))  
y <- y %>% rename(code.umsef = clase)


y$code.unlu <- as.factor(y$code.unlu)
y$code.umsef <- as.factor(y$code.umsef)


#seg.global <- 
  y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% 
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.global.png", plot = seg.global, 
       width = 15, height = 10, units = "cm")  


seg.pch <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(region == "pch") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.pch.png", plot = seg.pch, 
       width = 15, height = 10, units = "cm")  

seg.esp <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(region == "esp") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.esp.png", plot = seg.esp, 
       width = 15, height = 10, units = "cm")  

seg.smi <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(region == "smi") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.smi.png", plot = seg.smi, 
       width = 15, height = 10, units = "cm")  

seg.stb <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(region == "stb") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.stb.png", plot = seg.stb, 
       width = 15, height = 10, units = "cm")  

seg.salta <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(provincia == "Salta") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.salta.png", plot = seg.salta, 
       width = 15, height = 10, units = "cm")  

seg.santiago <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(provincia == "Santiago del Estero") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.santiago.png", plot = seg.santiago, 
       width = 15, height = 10, units = "cm")  

seg.chaco <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(provincia == "Chaco") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.chaco.png", plot = seg.chaco, 
       width = 15, height = 10, units = "cm")  

seg.formosa <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>% filter(provincia == "Formosa") %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 3, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal")

ggsave(filename = "results/seg.formosa.png", plot = seg.formosa, 
       width = 15, height = 10, units = "cm")  

### grafico por regiones
y <- y %>% mutate(region=replace(region, region == "esp", "Espinal")) %>% 
  mutate(region=replace(region, region == "pch", "Parque Chaqueño")) %>%
  mutate(region=replace(region, region == "stb", "Selva Tucumano-Boliviana")) %>%
  mutate(region=replace(region, region == "smi", "Selva Misionera"))

seg.regiones <- y %>% group_by(code.unlu) %>% select(code.unlu, seg, region, provincia) %>% 
  mutate(Concordancia = seg) %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 4, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal", text = element_text(size=16)) + 
  facet_wrap(~region, ncol = 1)

ggsave(filename = "results/seg.regiones.png", plot = seg.regiones, 
       width = 210, height = 300, units = "mm", dpi = 300)  

### grafico por provincia
seg.provincias <- y %>% group_by(code.unlu) %>% 
  select(code.unlu, seg, region, provincia) %>% 
  filter(provincia %in% c("Salta", "Santiago del Estero", "Chaco", "Formosa")) %>% 
  mutate(Concordancia = seg) %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 4, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal", text = element_text(size=16)) + 
  facet_wrap(~provincia, ncol = 1)

ggsave(filename = "results/seg.provincias.png", plot = seg.provincias, 
       width = 210, height = 300, units = "mm", dpi = 300)  

### grafico global
seg.global <- y %>% group_by(code.unlu) %>% 
  filter(provincia %in% c("Salta", "Santiago del Estero", "Chaco", "Formosa")) %>% 
  mutate(Concordancia = seg) %>%  
  ggplot(aes(x= code.unlu,fill= Concordancia))+ geom_bar(position="fill", color = "black") +
  geom_text(aes(label=..count..), size = 4, stat='count',
            position = position_fill(vjust=0.5)) + xlab("Categorías") + 
  ylab("Proporción") + scale_size_discrete("n") + scale_fill_brewer() +
  theme(legend.position="bottom", legend.direction="horizontal", text = element_text(size=16)) 
ggsave(filename = "results/seg.provincias.png", plot = seg.provincias, 
       width = 210, height = 100, units = "mm", dpi = 300)  

############ producir matrices de confusión con sumas y estadísticos ###############

clases <- tibble(clases = c( "TF_estable","OTF_estable", "OT_estable", 
                             "TF_perdida_1998-2006",
                             "OTF_perdida_1998-2006", "TF_perdida_2006-2013",  
                             "OTF_perdida_2006-2013", "TF_perdida_2013-2017",  
                             "OTF_perdida_2013-2017", "No_es_posible_determinar"),
                 code = 1:10)


y <- y %>% filter(unlu != "No_es_posible_determinar")
y <- y %>% left_join(y = class, by = c("unlu" = "clas.name"))  
y <- y %>% rename(code.unlu = clase)
y <- y %>% left_join(y = class, by = c("umsef"="clas.name"))  
y <- y %>% rename(code.umsef = clase)


y$code.unlu <- as.factor(y$code.unlu)
y$code.umsef <- as.factor(y$code.umsef)

r <- unique(y$region)
p <- as.character(unique(y$provincia)[c(7,8,13,15)])
library(knitr)

for (i in r) {
a <- y %>% group_by(code.umsef, code.unlu ) %>% 
  filter(region == i) %>%  
  select(code.unlu,code.umsef, ha, pi_hi) %>% 
  summarise(a = sum(ha/pi_hi, na.rm = TRUE)/1000) %>% 
  select(code.unlu, code.umsef, a) %>% 
  spread(key = code.unlu, value = a)
a$Total <- NA
a$Precision <- NA
for (k in 1:9) {
  a$Total[k] <- sum(a[k,2:10], na.rm = TRUE)
  a[k,12] <- (a[k,1+k]/a$Total[k])*100
  a[10,k+1] <- sum(a[,k+1], na.rm = TRUE)
  a[11,k+1] <- (a[k,k+1] / a[10,k+1]) * 100
}
a$Total[10] <- sum(a$Total[1:9])
a$code.umsef <- as.character(a$code.umsef)
a[10,1] <- "Total"
a[11,1] <- "Precision"
names(a)[1] <- i
print(kable(a,format = "markdown", digits = 2))
}

# Provincias
for (i in p) {
  a <- y %>% group_by(code.umsef, code.unlu ) %>% 
    filter(provincia == i) %>%  
    select(code.unlu,code.umsef, ha, pi_hi) %>% 
    summarise(a = sum(ha/pi_hi, na.rm = TRUE)/1000) %>% 
    select(code.unlu, code.umsef, a) %>% 
    spread(key = code.unlu, value = a)
  a$Total <- NA
  a$Precision <- NA
  for (k in 1:9) {
    a$Total[k] <- sum(a[k,2:10], na.rm = TRUE)
    a[k,12] <- (a[k,1+k]/a$Total[k])*100
    a[10,k+1] <- sum(a[,k+1], na.rm = TRUE)
    a[11,k+1] <- (a[k,k+1] / a[10,k+1]) * 100
  }
  a$Total[10] <- sum(a$Total[1:9])
  a$code.umsef <- as.character(a$code.umsef)
  a[10,1] <- "Total"
  a[11,1] <- "Sencibilidad"
  names(a)[1] <- i
  print(kable(a,format = "latex", digits = 2))
}

# global
a <- y %>% group_by(code.umsef, code.unlu ) %>% 
  select(code.unlu,code.umsef, ha, pi_hi) %>% 
  summarise(a = sum(ha/pi_hi, na.rm = TRUE)/1000) %>% 
  select(code.unlu, code.umsef, a) %>% 
  spread(key = code.unlu, value = a)
a$Total <- NA
a$Precision <- NA
for (k in 1:9) {
  a$Total[k] <- sum(a[k,2:10], na.rm = TRUE)
  a[k,12] <- (a[k,1+k]/a$Total[k])*100
  a[10,k+1] <- sum(a[,k+1], na.rm = TRUE)
  a[11,k+1] <- (a[k,k+1] / a[10,k+1]) * 100
}
a$Total[10] <- sum(a$Total[1:9])
a$code.umsef <- as.character(a$code.umsef)
a[10,1] <- "Total"
a[11,1] <- "Sensibilidad"
names(a)[1] <- "global"

x <- NULL
for (i in 1:9) {
 x <- append(x, as.numeric(a[i,i+1]))
}
a[11,12] <- sum(x)/a[10,11]*100
print(kable(a,format = "latex", digits = 2))

  