library(ggplot2)
#library(grid)


laser <- read.csv("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/LAICPMS/180219/BT_laser.csv")
epma  <- read.csv("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/180209/180209.csv")
epma  <- epma[epma$SAMPLE=='BT-Main',]
laser <- laser[laser$SAMPLE=='BT-Main',]
laser <- data.frame(lapply(laser, function(x) gsub("Below LOD","0",x)),stringsAsFactors=FALSE)
laser <- data.frame(lapply(laser, as.numeric))

EPMA_Ti <- ggplot(data=NULL)+
  geom_line(data=epma, aes(x=RELDIST+35, y=Ti.WT.*10000))+
  geom_line(data=laser, aes(x=RELDIST+41, y=(Ti_ppm_m49)),colour='RED')+
  #coord_cartesian(xlim=c(0,2000))+
  theme_classic()

elements <- c("Al_ppm_m27",
              "Sc_ppm_m45","Ti_ppm_m49","V_ppm_m51","Cr_ppm_m52","Mn_ppm_m55",
              "Co_ppm_m59","Ni_ppm_m60","Ga_ppm_m71",
              "Y_ppm_m89","Zr_ppm_m90","Nb_ppm_m93","Mo_ppm_m97","In_ppm_m113",
              "Sb_ppm_m121","Cs_ppm_m133","Ba_ppm_m138","La_ppm_m139","Ce_ppm_m140",
              "Nd_ppm_m146","Lu_ppm_m175","Hf_ppm_m179","Ta_ppm_m181","W_ppm_m183",
              "Tl_ppm_m205","Pb_ppm_m208","Bi_ppm_m209","Th_ppm_m232","U_ppm_m238")

simple_names <- c("27Al",
                  "45Sc","49Ti","51V","52Cr","55Mn",
                  "59Co","60Ni","71Ga",
                  "89Y","90Zr","93Nb","97Mo","113In",
                  "121Sb","133Cs","138Ba","139La","140Ce",
                  "146Nd","175Lu","179Hf","181Ta","183W",
                  "205Tl","208Pb","209Bi","232Th","238U")

seq<-1

for(e in elements){
  i <- which(colnames(laser)==e)
  p<- ggplot(data=laser, aes(x=RELDIST+41))+
    geom_errorbar(aes(ymin=(laser[,i]-laser[,(i+1)]),ymax=(laser[,i]+laser[,(i+1)])),colour="grey80")+
    geom_point(aes(y=laser[,i]), size=1)+
    geom_line(aes(y=laser[,i]))+
    stat_function(fun=function(x){y=mean(laser[,i+2], na.rm=TRUE)}, colour="blue")+
    theme_classic()+
    theme(plot.margin=unit(c(0,0,0.5,0.5),"cm"))+
    scale_x_continuous(limits = c(0,7496.053), expand = c(0, 0))+
    scale_y_continuous(expand = c(0, 0))+
    labs(x="Distance (um)", title=simple_names[seq], y="ppm", subtitle=paste("Av. det lim =",signif(mean(laser[,i+2], na.rm=TRUE), digits=2),"ppm"))
  #ggsave(paste0(simple_names[seq],"_lin.png"), p, width=(38.24+0.5), height=4, units="cm")
  seq <- seq+1
}

nbta <- ggplot(data=laser, aes(x=RELDIST+41))+
  geom_line(aes(y=Nb_ppm_m93/Ta_ppm_m181))+
  theme(plot.margin=unit(c(0,0,0.5,0.5),"cm"))+
  scale_x_continuous(limits = c(0,7496.053), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  theme_classic()+
  labs(x="Distance (um)", y="Nb/Ta")

zrhf <- ggplot(data=laser, aes(x=RELDIST+41))+
  geom_line(aes(y=Zr_ppm_m90/Hf_ppm_m179))+
  theme(plot.margin=unit(c(0,0,0.5,0.5),"cm"))+
  scale_x_continuous(limits = c(0,7496.053), expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  theme_classic()+
  labs(x="Distance (um)", y="Zr/Hf")

nbzr <- ggplot(data=laser, aes(y=Nb_ppm_m93/Ta_ppm_m181, x=Zr_ppm_m90/Hf_ppm_m179))+
  geom_point()+
  scale_x_continuous(limits = c(0,40), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0))+
  theme_classic()+
  labs(x="Zr/Hf",y="Nb/Ta")

ggsave("nbta.png", nbta, width=(38.24+0.5), height=4, units="cm")
ggsave("zrhf.png", zrhf, width=(38.24+0.5), height=4, units="cm")
ggsave("nbzr.png", nbzr, width=15, height=15, units="cm")