rm(list = ls())
name <- function(x) { as.data.frame(names(x))}
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
##########

classif.files <- paste0("csv/",list.files(path = "csv/", pattern = "classif"))

y <- list()
for(i in seq_along(classif.files)){
  y[[i]] <- read.csv(classif.files[i], stringsAsFactors = F)
}

for (i in 1:10) {
  y[[i]]$annotations[i] %>% prettify %>% print
}
for (i in 1:10) {
  y[[i]]$subject_data[i] %>% prettify %>% print
}
# Functions
flat.clases <- function(jdata = jdata){
  clases <- jdata %>% 
    select(., subject_ids, user_name, classification_id, workflow_version, annotations) %>%
    as.tbl_json(json.column = "annotations") %>%
    gather_array(column.name = "task_index") %>% # really important for joining later
    spread_values(.,task = jstring("task"), task_label = jstring("task_label"), value = jstring("value"))
  clases
}
flat.segmentos <- 
  function(jdata = jdata, col.name.id = "id", col.name.image = "image"){
    s <- jdata %>% 
      select(., subject_ids, user_name, classification_id, 
             workflow_version, subject_data) %>%
      as.tbl_json(json.column = "subject_data") %>%
      gather_keys(column.name = "task") %>% # really important for joining later
      spread_values(.,id = jstring(col.name.id), 
                    image = jstring(col.name.image), 
                    date = jstring("retired", "updated_at"))
    s <- s[,c("classification_id", "id", "image")]
    s
  }
clases <- list()
segmentos <- list()
for(i in seq_along(y)){
  clases[[i]] <- flat.clases(jdata = y[[i]])
  segmentos[[i]] <- flat.segmentos(jdata = y[[i]],  col.name.id = "id", col.name.image = "image")
}

segmentos[[6]]$id <- segmentos[[6]]$image %>% gsub(pattern = "pch_",replacement = "")
segmentos[[6]]$id <- segmentos[[6]]$id %>%
  gsub(pattern = "reclas.png",replacement = "") %>% as.numeric()

for(i in seq_along(segmentos)){
  segmentos[[i]]$id <- as.integer(segmentos[[i]]$id)  
}

## Sort tables by region
csv.names <- c("pchh", "pchs", "pchsf_r","pchsf", "pchs_r", "pchh_r",
               "smi_r", "smi", "yungas_r", "yungas")
names(clases) <- csv.names
names(segmentos) <- csv.names

clases <- clases[order(names(clases))]
segmentos <- segmentos[order(names(segmentos))]

# Join clases and segmentos
regions <- list()
for(i in seq_along(segmentos)){
  regions[[i]] <- full_join(x=clases[[i]], y=segmentos[[i]], 
                            by = "classification_id", copy = FALSE)
}

## reshape data
library(reshape2)
library(data.table)
s <- list()

colname <- c("id", "user_name",
             "No_es_posible_determinar", "OT_estable", "OTF_estable", 
             "OTF_perdida_1998-2006", "OTF_perdida_2006-2013", 
             "OTF_perdida_2013-2017", "TF_estable", "TF_perdida_1998-2006",
             "TF_perdida_2006-2013", "TF_perdida_2013-2017")

for(i in seq_along(regions)){
  # reshape long to wide data
  s[[i]] <- as_tibble(dcast(regions[[i]], id+user_name~value, fun.aggregate = length))
  # change column names
  names(s[[i]]) <- gsub(pattern = " ", replacement = "_", x = names(s[[i]]))
  # remove non meaning columns
  s[[i]] <- s[[i]] %>% select(which(colnames(s[[i]]) %in% colname))

}
# missing classes 
s[[6]]$No_es_posible_determinar <- 0
s[[8]]$No_es_posible_determinar <- 0
s[[10]]$No_es_posible_determinar <- 0
s[[10]]$`OTF_perdida_2013-2017` <- 0

sz <- list()
for(i in seq_along(s)){
  # reorder columns
  s[[i]] <- s[[i]][,colname]
  # some segments were classifed more than once by the same interpreter
  # replace value > 1 by 1
  m <- as.matrix(s[[i]][,3:12])
  m[which(m>1)] <- 1
  m <- as.data.frame(m)
  s[[i]][,3:12] <- m
  # add a column with row sums (each row is a segment)
  # s[[i]] <- s[[i]] %>% mutate(rowsum = rowSums(x = s[[i]][,3:12]))
  # reshape data from wide to long table; remove user_name (and rowSums) 
  new <- melt(data = s[[i]][,c(-2)], id.vars = "id")
  # Segments classified three times with the same class ar in one row,
  # Segments classified with different classes are in more than one row
  # with its respective classes
  new <- new[new$value != 0,-3] 

  sb <- as_tibble(dcast(new, id~variable, fun.aggregate = length))
  
  sc <- melt(sb,id.vars = "id")
  sc <- sc[sc$value>0,]
  sd <- sc %>% group_by(id)
  summary(as.factor(sd$value))
  
  library(data.table)
  group <- as.data.table(sd)
  
  sz[[i]] <- as.data.frame(group[group[, .I[which.max(value)], by=id]$V1])
}

# Jpin tables regions + revised
names(sz) <- csv.names[order(csv.names)]
r <- list()
l <- c(1,3,5,7,9)
for(i in seq_along(l)){
  r[[i]] <- full_join(x = sz[[l[i]]], y = sz[[l[i]+1]], by = "id", copy = FALSE, all.x = T )
  names(r)[i] <- csv.names[order(csv.names)][l[i]]
}

# row bind pchs and pchs faltantes
r[["pchs"]] <- rbind(r[["pchs"]],r[["pchsf"]])
r <- r[-3]

# Replace values.x by values.y (revisados) #Warning. There are missing values
for(i in seq_along(r)){
  r[[i]]$variable.x <- as.character(r[[i]]$variable.x)
  r[[i]]$variable.y <- as.character(r[[i]]$variable.y)
}

for(i in seq_along(r)){
r[[i]]$variable.x[r[[i]]$value.x == 1] <- r[[i]]$variable.y[r[[i]]$value.x == 1]
}

for(i in seq_along(r)){
  r[[i]]$variable.x[r[[i]]$variable.x == "No_es_posible_determinar" & !is.na(r[[i]]$variable.x)] <- 
    r[[i]]$variable.y[r[[i]]$variable.x == "No_es_posible_determinar" & !is.na(r[[i]]$variable.x)]
  r[[i]] <- r[[i]][,1:3]
}

#### END compiling zooniverse data #####
rm(list = ls()[-13])
name <- function(x) { as.data.frame(names(x))}

save.image("~/git/Bosques/results/zoon.data.RData")

