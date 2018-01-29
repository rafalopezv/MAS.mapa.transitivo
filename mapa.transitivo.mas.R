rm(list = ls())
Sys.setlocale(locale = "es_ES.UTF-8")

library(tidyverse)
library(magrittr)
library(rio)

bases <- list.files(recursive = T)
bases <- bases[!grepl(".Rproj|.geojson|.mas.R|/Icon\r|limpio.xlsx|final.xlsx|.Rmd|.html|.png", bases)]

datos <- list()

for(i in 1:length(bases)) {
  datos[[i]] <- import(bases[i])
}

for(i in 1:length(bases)) {
  colnames(datos[[i]])  %<>% toupper() %>% trimws()
  colnames(datos[[i]]) %<>% gsub("CÓDIGO INE", "CODIGO", .)
  colnames(datos[[i]]) %<>% gsub("\\<MAS\\>", "MAS-IPSP", .)
  colnames(datos[[i]]) %<>% gsub("MAS-IPSP-IPSP", "MAS-IPSP", .)
  colnames(datos[[i]]) %<>% gsub("\\<SÍ\\>", "SI", .)
}


vars <- c("CODIGO", "MUNICIPIO", "VÁLIDOS", "EMITIDOS", "MAS-IPSP", "SI", "NO", "NULOS", "BLANCOS", "5000H")
for(i in 1:length(datos)) {
  datos[[i]] %<>% select(one_of(vars))
}

for(i in 1:length(datos)) {
  datos[[i]][, 3:ncol(datos[[i]])]  %<>% apply(., 2, as.numeric)
}

for(i in 1:length(datos)) {
  datos[[i]] %<>% group_by(CODIGO, MUNICIPIO)  %>% 
    summarize_all(funs(sum))
}

temp <- which(sapply(datos, function(x) any(colnames(x) == "MAS-IPSP")))
for(i in temp) {
  datos[[i]]$MAS <- (datos[[i]]$`MAS-IPSP` / datos[[i]]$VÁLIDOS) *100
}

temp <- which(sapply(datos, function(x) any(colnames(x) == "SI")))
for(i in temp) {
  datos[[i]]$MAS <- (datos[[i]]$SI / datos[[i]]$VÁLIDOS) *100
}

datos[[7]]$MAS <- (datos[[7]]$`5000H` / datos[[7]]$VÁLIDOS) *100

temp <- c(8:12, 15:18)
for(i in temp) {
  datos[[i]]$MAS <- (datos[[i]]$VÁLIDOS / datos[[i]]$EMITIDOS) *100
  datos[[i]] %<>% arrange(CODIGO)
}

datos[[8]]$MAS <- (datos[[8]]$MAS + datos[[9]]$MAS + datos[[10]]$MAS + datos[[11]]$MAS + datos[[12]]$MAS)/5
datos[[15]]$MAS <- (datos[[15]]$MAS + datos[[16]]$MAS + datos[[17]]$MAS + datos[[18]]$MAS)/4

ine <- import("codigos.limpio.xlsx")
ine %<>% select(CODIGO, MUNICIPIO)

for(i in 1:length(datos)) {
  datos[[i]]  %<>% select(CODIGO, MUNICIPIO, MAS) %>% 
    rename(name = MUNICIPIO)
  datos[[i]] %<>% merge(., ine, by="CODIGO", all.y=T) %>% 
    select(-name)  %>% 
    rename(name = MUNICIPIO) %>% 
    mutate(MAS = ifelse(is.na(MAS), 0, MAS))
  datos[[i]]$MAS %<>% round(., 2)
}

temp <- c(1:6, 8, 13:15)
bases <- bases[temp]
datos <- datos[temp]

datos[[1]]$FECHA <- "2002"
datos[[2]]$FECHA <- "2005"
datos[[3]]$FECHA <- "2006"
datos[[4]]$FECHA <- "2008"
datos[[5]]$FECHA <- "2009"
datos[[6]]$FECHA <- "2009a"
datos[[7]]$FECHA <- "2011"
datos[[8]]$FECHA <- "2014"
datos[[9]]$FECHA <- "2016"
datos[[10]]$FECHA <- "2017"

final <- bind_rows(datos)
export(final, "final.xlsx")
#---



