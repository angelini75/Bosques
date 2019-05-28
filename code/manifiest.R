rm(list = ls())

setwd("~/Documents/GDB/UNLU/BOSQUE/Analisis/SMI/")
# 
# m <- data.frame(id=1:180, 
#                 image_1=paste0("pch_",1:180,".png"),
#                 image_2=paste0("pch_",1:180,"LR.png"),
#                 image_3=paste0("pch_",1:180,"HD.png"))



###########

list.files(pattern = "HD.png", path = "salidas/")
csv <- data.frame(X=list.files(pattern = "HD.png", path = "salidas/"))
csv$Y <- paste0(gsub(csv$X,pattern = "HD.png", replacement = ""), ".png")
csv$Z <- paste0(gsub(csv$X,pattern = "HD.png", replacement = "LR"), ".png")
csv$id <- gsub(csv$X,pattern = "HD.png", replacement = "")
csv$id <- gsub(csv$id,pattern = "smi_", replacement = "")
csv$id <- as.numeric(csv$id)
csv <- csv[,c(4,2,1,3)]
write.csv(x = csv, file = "salidas/manifiesto.csv")

rm(list = ls())

#list.files(pattern = "faltantes", path = "faltantes/")
csv <- data.frame(X=list.files(pattern = "HD.png", path = "salidas/"))
csv$Y <- paste0(gsub(csv$X,pattern = "_HD.png", replacement = ""), ".png")
#csv$Z <- paste0(gsub(csv$X,pattern = "_HD.png", replacement = "LR"), ".png")
csv$id <- gsub(csv$X,pattern = "_HD.png", replacement = "")
csv$id <- gsub(csv$id,pattern = "pch_", replacement = "")
csv$id <- as.numeric(csv$id)
csv <- csv[,c(3,2,1)]
write.csv(x = csv, file = "salidas/manifiesto.csv")
