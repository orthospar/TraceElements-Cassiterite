library(dplyr)
library(ggplot2)
library(cowplot)

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

NbTa <- ggplot(data=NULL, aes(x=Nb_ppm_m93/Ta_ppm_m181, y=In_ppm_m113))+
  scale_color_brewer(palette="Dark2")+
  geom_point(data=ALL, colour="grey80")+
  scale_x_log10(limits=c(-100,1000), breaks=c(1, 5, 10, 50, 100, 1000))+
  scale_y_log10()+
  theme_classic()+
  labs(y=NULL,x=NULL, title=NULL)

ZrHf <- ggplot(data=NULL, aes(x=Zr_ppm_m90/Hf_ppm_m179, y=In_ppm_m113))+
  scale_color_brewer(palette="Dark2")+
  geom_point(data=ALL, colour="grey80")+
  scale_x_log10(limits=c(1,50), breaks=c(1, 5, 10, 25, 50))+
  scale_y_log10()+
  theme_classic()+
  labs(y=NULL,x=NULL, title=NULL)

ZrHf_peg <- ZrHf + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
ZrHf_greisen <- ZrHf + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")

NbTa_peg <- NbTa + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
NbTa_greisen <- NbTa + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")

ggsave("plots/In-ZrHf_peg.pdf", ZrHf_peg, width=8, height=8, units="cm")
ggsave("plots/In-ZrHf_greisen.pdf", ZrHf_greisen, width=8, height=8, units="cm")
ggsave("plots/In-NbTa_peg.pdf", NbTa_peg, width=8, height=8, units="cm")
ggsave("plots/In-NbTa_greisen.pdf", NbTa_greisen, width=8, height=8, units="cm")