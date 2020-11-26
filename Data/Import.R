library(dplyr)
wd<-getwd()

blue <- read.csv("BT_laser.csv")
blue <- data.frame(lapply(blue, function(x) gsub("Below LOD","0",x)),stringsAsFactors=FALSE)

laser <- read.csv("ALL_laser.csv")
laser <- laser[laser$SAMPLE!='NIST610',]
laser <- laser[laser$SAMPLE!='NIST612',]
laser <- data.frame(lapply(laser, function(x) gsub("Below LOD","0",x)),stringsAsFactors=FALSE)

lilly <- read.csv("LKL_laser.csv")
lilly <- data.frame(lapply(lilly, function(x) gsub("-","0",x)),stringsAsFactors=FALSE)

ALL <- full_join(blue, laser)
ALL <- full_join(ALL, lilly)
ALL[,c(4,9,14:188)] <- data.frame(lapply(ALL[,c(4,9,14:188)], as.numeric))
ALL[,2:3] <- data.frame(lapply(ALL[,2:3], as.factor))

pegmatite <- ALL[ALL$STYLE=="Pegmatite",]
greisen <- ALL[ALL$STYLE=="Greisen",]
skarn <- ALL[ALL$STYLE=="Skarn",]

setwd(wd)
rm(wd)