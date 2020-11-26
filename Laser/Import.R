library(dplyr)
library(tidyr)

bluetier <- read.csv("laser_BT.csv")
bluetier <- data.frame(lapply(bluetier, function(x) gsub("Below LOD",NA,x)),stringsAsFactors=FALSE)
bluetier <- bluetier[,-c(67:69,109:111,139:141)]#remove Ga71, Sn119, W182
colnames(bluetier) <- gsub(pattern="_ppm_m\\d+",replacement="", colnames(bluetier))
colnames(bluetier) <- gsub(pattern="_EPMA_ppm",replacement="", colnames(bluetier))
bluetier[c(6,11,13,15:dim(bluetier)[2])] <- data.frame(lapply(bluetier[c(6,11,13,15:dim(bluetier)[2])], as.numeric))
bluetier[-c(6,11,13,15:dim(bluetier)[2])] <- data.frame(lapply(bluetier[-c(6,11,13,15:dim(bluetier)[2])], as.character))

jason <- read.csv("laser_JB.csv")
jason <- data.frame(lapply(jason, function(x) gsub("Below LOD",NA,x)),stringsAsFactors=FALSE)
jason <- jason[,-c(70:72,109:111,139:141)]#remove Ga71, Sn119, W182
colnames(jason) <- gsub("_ppm_m\\d+","", colnames(jason))
jason[c(6,11,13,15:dim(jason)[2])] <- data.frame(lapply(jason[c(6,11,13,15:dim(jason)[2])], as.numeric))
jason[-c(6,11,13,15:dim(jason)[2])] <- data.frame(lapply(jason[-c(6,11,13,15:dim(jason)[2])], as.character))
#apply conversion factor of 0.1499 to concentration (ppm)
jason[,seq(from= which(colnames(jason)=="Li"),to=dim(jason)[2],by=3)] <- jason[,seq(from= which(colnames(jason)=="Li"),to=dim(jason)[2],by=3)]*0.1499 

lilly <- read.csv("laser_LKL_full.csv")
lilly[,-(1:6)] <- data.frame(lapply(lilly[,-(1:6)], function(x) gsub("<\\d+",NA,x)),stringsAsFactors=FALSE)
colnames(lilly) <- gsub("_ppm_m\\d+","", colnames(lilly))
lilly[7:dim(lilly)[2]] <- data.frame(lapply(lilly[7:dim(lilly)[2]], as.numeric))
lilly[-(7:dim(lilly)[2])] <- data.frame(lapply(lilly[-(7:dim(lilly)[2])], as.character))

tony <- read.csv("laser_TK.csv")
tony[,-(1:7)] <- data.frame(lapply(tony[,-(1:7)], function(x) gsub("<\\d+(\\.\\d+){,1}",NA,x)),stringsAsFactors=FALSE)
colnames(tony) <- gsub("\\d+","", colnames(tony))
tony[c(8:dim(tony)[2])] <- data.frame(lapply(tony[c(8:dim(tony)[2])], as.numeric))
tony[-c(8:dim(tony)[2])] <- data.frame(lapply(tony[-c(8:dim(tony)[2])], as.character))

JCU <- read.csv("laser_JCU.csv")
JCU <- JCU[,-c(15,31,32,50)]#remove Cr53, Sn119, Sn115, In115
JCU[,-(1:5)] <- data.frame(lapply(JCU[,-(1:5)], function(x) gsub("<\\d+(\\.\\d+){,1}",NA,x)),stringsAsFactors=FALSE)
colnames(JCU) <- gsub("\\d+","", colnames(JCU))
JCU[6:dim(JCU)[2]] <- data.frame(lapply(JCU[6:dim(JCU)[2]], as.numeric))
JCU[-(6:dim(JCU)[2])] <- data.frame(lapply(JCU[-(6:dim(JCU)[2])], as.character))

common_elements <- c("SAMPLE", "LOCALITY", "STYLE", "IS", "Ti", "Mn", "Y", "Zr", "Nb", "Sb", "Hf", "Ta", "W", "Ce", "U", "Pb",
                     "Ti_LOD", "Mn_LOD", "Y_LOD", "Zr_LOD", "Nb_LOD", "Sb_LOD", "Hf_LOD", "Ta_LOD", "W_LOD", "Ce_LOD", "U_LOD", "Pb_LOD")

common <- rbind(bluetier[bluetier$Fe57_CPS!="NaN",(colnames(bluetier) %in% common_elements)],
                jason[,colnames(jason) %in% common_elements],
                lilly[,colnames(lilly) %in% common_elements],
                tony[,colnames(tony) %in% common_elements],
                JCU[,colnames(JCU) %in% common_elements]) 
common <- mutate_at(common, vars(common_elements[-(1:4)]), ~as.numeric(.))
common <- mutate_at(common, vars(common_elements[(1:4)]), ~as.factor(.))

rm(common_elements)