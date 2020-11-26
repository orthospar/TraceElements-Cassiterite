library(ggplot2)
library(ggtern)

laser <- read.csv("BT_laser.csv")
laser <- laser[laser$SAMPLE=='BT-Main',]

source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R")
BT <- dplyr::filter(data, LOCALITY=="Blue Tier")

#excluded below detection: Li, Si, Ca, Cu, Zn, As, Se, Rb, Sr, Tl, Pb, Bi, Th
#excluded too close to detection: Cr, Ge, Nd
#questionable: P, Kr
#second isotopes: Ga71, W183
elements <- c("RELDIST",
              "Al_ppm_m27","Sc_ppm_m45","Ti_ppm_m49","V_ppm_m51","Mn_ppm_m55",
              "Fe_ppm_m57","Co_ppm_m59","Ni_ppm_m60","Ga_ppm_m69","Y_ppm_m89",
              "Zr_ppm_m90","Nb_ppm_m93","Mo_ppm_m97","In_ppm_m113","Sb_ppm_m121",
              "Cs_ppm_m133","Ba_ppm_m138","La_ppm_m139","Ce_ppm_m140","Lu_ppm_m175",
              "Hf_ppm_m179","Ta_ppm_m181","W_ppm_m182","U_ppm_m238")
simple_names <- c("Distance",
                  "Al","Sc","Ti","V","Mn",
                  "Fe","Co","Ni","Ga","Y",
                  "Zr","Nb","Mo","In","Sb",
                  "Cs","Ba","La","Ce","Lu",
                  "Hf","Ta","W","U")

#select only the columns listed in elements, replace all instances of "Below LOD" with 0, set to numeric
mydata <- laser[,colnames(laser) %in% elements]
mydata <- data.frame(lapply(mydata, function(x) gsub("Below LOD","0",x)),stringsAsFactors=FALSE)
mydata <- data.frame(lapply(mydata, as.numeric))
colnames(mydata) <- simple_names

sum <-with(mydata, W/182+Fe/57+Mn/55+Nb/93+Ta/181+1000000/118)
W.at<-((mydata$W/182)/sum)*10
Fe.at<-((mydata$Fe/57)/sum)*10
Mn.at<-((mydata$Mn/55)/sum)*10
Nb.at<-((mydata$Nb/93)/sum)*10
Ta.at<-((mydata$Ta/181)/sum)*10

atomic <- data.frame(cbind(W.at, Fe.at, Mn.at, Nb.at, Ta.at))

tap<-function(x){x*2}
unity<-function(x){x}

p <- ggplot(atomic, aes(x=Fe.at+Mn.at, y=Ta.at+Nb.at))+
  geom_point(data=BT, aes(x=Fe.AT.+Mn.AT., y=Nb.AT.+Ta.AT.), colour="grey50")+
  geom_point(colour="red")+
    stat_function(fun=tap, geom="line")+
  stat_function(fun=unity, geom="line")+
  coord_fixed(xlim=c(0,1), ylim=c(0,1))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  theme_classic()

print(p)