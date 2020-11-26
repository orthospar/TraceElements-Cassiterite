library(dplyr)
library(ggplot2)

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

elements <- c("Al_ppm_m27","Sc_ppm_m45","Ti_ppm_m49","V_ppm_m51","Mn_ppm_m55",
              "Co_ppm_m59","Ni_ppm_m60","Ga_ppm_m69","Y_ppm_m89",
              "Zr_ppm_m90","Nb_ppm_m93","Mo_ppm_m97","In_ppm_m113","Sb_ppm_m121",
              "Cs_ppm_m133","Ba_ppm_m138","La_ppm_m139","Ce_ppm_m140","Lu_ppm_m175",
              "Hf_ppm_m179","Ta_ppm_m181","W_ppm_m182","U_ppm_m238")

for(e in elements){
  i <- which(colnames(ALL)==e)
  p <- ggplot(data=ALL, aes(x=SAMPLE, y=ALL[,i]))+
    geom_violin()+
    scale_y_log10(limits=c(0.1,10000))+
    coord_flip()+
    labs(y=e)
    ggsave(paste0("plots/contents/",e,".png"), p, width=20, height=20, units="cm")
  }

