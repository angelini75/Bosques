rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
detach("package:data.table", unload=TRUE)
detach("package:dplyr", unload=TRUE)
detach("package:caret", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:jsonlite", unload=TRUE)
detach("package:lubridate", unload=TRUE)
detach("package:magrittr", unload=TRUE)
detach("package:raster", unload=TRUE)
detach("package:rgdal", unload=TRUE)
detach("package:reshape2", unload=TRUE)
detach("package:stringr", unload=TRUE)
detach("package:sp", unload=TRUE)
detach("package:tidyjson", unload=TRUE)
detach("package:tidyr", unload=TRUE)

####

library(tidyjson)
library(magrittr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

#### files ###
setwd("~/git/Bosques/")
classifications_file <- "csv/parque-chaqueno-humedo-classifications.csv"
SHP <- "shp/union_seg_pch_h_4326.shp"
write.shp <- "shp/pchh_consolidado.shp"

jdata <- read.csv(classifications_file, stringsAsFactors = F)

head(jdata)
for (i in 10:15) {
     jdata$annotations[i] %>% prettify %>% print
}
for (i in 1:5) {
     jdata$subject_data[i] %>% prettify %>% print
}

flat.clases <- function(jdata = jdata){
  clases <- jdata %>% 
    select(., subject_ids, user_name, classification_id, workflow_version, annotations) %>%
    as.tbl_json(json.column = "annotations") %>%
    gather_array(column.name = "task_index") %>% # really important for joining later
    spread_values(.,task = jstring("task"), task_label = jstring("task_label"), value = jstring("value"))
  clases
}

clases <- flat.clases(jdata = jdata)
clases %>% data.frame %>% head
# Summary user by group
clases %>% data.frame %>% 
     group_by(., user_name, task) %>% summarise(., n()) %>% print
flat.segmentos <- 
  function(jdata = jdata, col.name.id = "id", col.name.image = "image_1"){
    s <- jdata %>% 
      select(., subject_ids, user_name, classification_id, 
             workflow_version, subject_data) %>%
      as.tbl_json(json.column = "subject_data") %>%
      gather_keys(column.name = "task") %>% # really important for joining later
      spread_values(.,id = jstring(col.name.id), 
                    id_img = jstring(col.name.image), 
                    date = jstring("retired", "updated_at"))
    s <- s[,c("classification_id", "id", "id_img")]
    s
  }

segmentos <- flat.segmentos(jdata = jdata, col.name.id = "id", col.name.image = "image_1")
segmentos %>% data.frame %>% head

# segmentos$id <- segmentos$id_img %>% gsub(pattern = "pch_",replacement = "")
# segmentos$id <- segmentos$id %>%
#   gsub(pattern = "reclas.png",replacement = "") %>% as.numeric()
# merge tablas

region <- full_join(x=clases, y=segmentos, by = "classification_id", copy = FALSE)
name(region)

# plot
library(ggplot2)
ggplot(region, aes(x = as.factor(value)))+ geom_bar() +
  facet_wrap(facets = ~user_name)+
  theme(text = element_text(size=12),
           axis.text.x = element_text(angle=90, hjust=1))

# data transformation
data <- as_tibble(region)

library(reshape2)
s <- as_tibble(dcast(data,id+user_name~value,fun.aggregate = length))
colname <- c("id", "user_name",
             "No_es_posible_determinar", "OT_estable", "OTF_estable", 
             "OTF_perdida_1998-2006", "OTF_perdida_2006-2013", 
             "OTF_perdida_2013-2017", "TF_estable", "TF_perdida_1998-2006",
             "TF_perdida_2006-2013", "TF_perdida_2013-2017")
names(s) <- gsub(pattern = " ", replacement = "_", x = names(s))
s <- s %>% select(which(colnames(s) %in% colname))
m <- as.matrix(s[,3:12])
m[which(m>1)] <- 1
m <- as.data.frame(m)
s[,3:12] <- m

s <- s %>% mutate(rowsum = rowSums(x = s[,3:12]))

new <- melt(data = s[,c(-2,-13)], id.vars = "id")
new <- new[new$value != 0,-3]

sb <- as_tibble(dcast(new, id~variable, fun.aggregate = length))

sc <- melt(sb,id.vars = "id")
sc <- sc[sc$value>0,]
sd <- sc %>% group_by(id)
summary(as.factor(sd$value))

library(data.table)
group <- as.data.table(sd)

sz <- as.data.frame(group[group[, .I[which.max(value)], by=id]$V1])

summary(as.factor(sz$value))
### merge with shape

library(caret)
library(rgdal)
library(raster)
shp <- readOGR(SHP) 

shp@data = data.frame(shp@data, sz[match(shp@data$ID, sz$id),])


writeOGR(shp, dsn = write.shp, layer ='shp', driver = 'ESRI Shapefile')

