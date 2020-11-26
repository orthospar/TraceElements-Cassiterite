library(ggplot2)
library(cowplot)

source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/LAICPMS/Import.R")
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA/coloursafe.R")

ZrHf <- ggplot(data=ALL, aes(x=log10(Zr_ppm_m90), y=log10(Hf_ppm_m179)))+
  scale_color_brewer(palette="Dark2")+
  stat_function(fun=function(x){x-log10(30)}, geom="line", linetype=2)+
  stat_function(fun=function(x){x-log10(5)}, geom="line", linetype=2)+
  geom_point(colour="grey80")+
  scale_x_continuous(limits=c(1,4), breaks=c(1:4))+
  scale_y_continuous(limits=c(0,4), breaks=c(0:4))+
  theme_classic()+
  #labs(x="log10(Zr)",y="log10(Hf)")
  labs(x=NULL, y=NULL)

NbTa <- ggplot(data=ALL, aes(x=log10(Nb_ppm_m93), y=log10(Ta_ppm_m181)))+
  scale_color_brewer(palette="Dark2")+
  stat_function(fun=function(x){x-log10(1)}, geom="line", linetype=2)+
  stat_function(fun=function(x){x-log10(5)}, geom="line", linetype=2)+
  stat_function(fun=function(x){x-log10(13)}, geom="line", linetype=2)+
  stat_function(fun=function(x){x-log10(100)}, geom="line", linetype=2)+
  geom_point(colour="grey80")+
  scale_x_continuous(limits=c(0,5), breaks=c(0:5))+
  scale_y_continuous(limits=c(-2,5), breaks=c(-2:5))+
  theme_classic()+
  #labs(x="log10(Nb)",y="log10(Ta)")
  labs(x=NULL, y=NULL)

Zr <-ggplot(data=NULL, aes(x=Zr_ppm_m90, y=Zr_ppm_m90/Hf_ppm_m179))+
  scale_color_brewer(palette="Dark2")+
  geom_point(data=ALL, colour="grey80")+
  scale_x_log10(limits=c(10, 10000), breaks=c(10, 100, 1000, 10000))+
  scale_y_log10(limits=c(1,50), breaks=c(1, 5, 10, 25, 50))+
  theme_classic()+
  labs(x="Zr [ppm]",y="Zr/Hf Ratio")

Hf <-ggplot(data=NULL, aes(x=Hf_ppm_m179, y=Zr_ppm_m90/Hf_ppm_m179))+
  scale_color_brewer(palette="Dark2")+
  geom_point(data=ALL, colour="grey80")+
  scale_x_log10(limits=c(10,10000), breaks=c(1, 10, 100, 1000, 10000))+
  scale_y_log10(limits=c(1,50), breaks=c(1, 5, 10, 25, 50))+
  theme_classic()+
  labs(x="Hf [ppm]",y="Zr/Hf Ratio")

Nb <- ggplot(data=NULL, aes(x=Nb_ppm_m93, y=Nb_ppm_m93/Ta_ppm_m181))+
  scale_color_brewer(palette="Dark2")+
  geom_point(data=ALL, colour="grey80")+
  scale_x_log10(limits=c(1,100000), breaks=c(1, 10, 100, 1000, 10000, 100000))+
  scale_y_log10(limits=c(-100,1000), breaks=c(1, 5, 10, 50, 100, 1000))+
  theme_classic()+
  labs(x="Nb [ppm]", y="Nb/Ta Ratio")

Ta <- ggplot(data=NULL, aes(x=Ta_ppm_m181, y=Nb_ppm_m93/Ta_ppm_m181))+
  scale_color_brewer(palette="Dark2")+
  geom_point(data=ALL, colour="grey80")+
  scale_x_log10(limits=c(1,100000), breaks=c(1, 10, 100, 1000, 10000, 100000))+
  scale_y_log10(limits=c(-100,1000), breaks=c(1, 5, 10, 50, 100, 1000))+
  theme_classic()+
  labs(x="Ta [ppm]", y="Nb/Ta Ratio")


ZrHf_peg <- ZrHf + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
ZrHf_greisen <- ZrHf + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")

NbTa_peg <- NbTa + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
NbTa_greisen <- NbTa + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")

legend_greisen <- ggdraw(get_legend(ZrHf + geom_point(data=greisen, aes(colour=SAMPLE))))
legend_peg <- ggdraw(get_legend(ZrHf + geom_point(data=pegmatite, aes(colour=SAMPLE))))

#Hf below detection in renison samples, so cannot be plotted
Zr_peg <- Zr + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
Zr_greisen <- Zr + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")
Hf_peg <- Hf + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
Hf_greisen <- Hf + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")

Nb_peg <- Nb + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
Nb_greisen <- Nb + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")
Ta_peg <- Ta + geom_point(data=pegmatite, aes(colour=SAMPLE)) + theme(legend.position="none")
Ta_greisen <- Ta + geom_point(data=greisen, aes(colour=SAMPLE)) + theme(legend.position="none")

ggsave("plots/ZrHf_peg.pdf", ZrHf_peg, width=8, height=8, units="cm")
ggsave("plots/ZrHf_greisen.pdf", ZrHf_greisen, width=8, height=8, units="cm")
ggsave("plots/NbTa_peg.pdf", NbTa_peg, width=8, height=8, units="cm")
ggsave("plots/NbTa_greisen.pdf", NbTa_greisen, width=8, height=8, units="cm")
ggsave("plots/Zr_greisen.pdf", Zr_greisen, width=8, height=8, units="cm")
ggsave("plots/Hf_greisen.pdf", Hf_greisen, width=8, height=8, units="cm")
ggsave("plots/Nb_greisen.pdf", Nb_greisen, width=8, height=8, units="cm")
ggsave("plots/Ta_greisen.pdf", Ta_greisen, width=8, height=8, units="cm")
ggsave("plots/Zr_peg.pdf", Zr_peg, width=8, height=8, units="cm")
ggsave("plots/Hf_peg.pdf", Hf_peg, width=8, height=8, units="cm")
ggsave("plots/Nb_peg.pdf", Nb_peg, width=8, height=8, units="cm")
ggsave("plots/Ta_peg.pdf", Ta_peg, width=8, height=8, units="cm")

ggsave("plots/legend_peg.pdf", legend_peg, width=5, height= 10, units="cm")
ggsave("plots/legend_greisen.pdf", legend_greisen, width=5, height= 10, units="cm")