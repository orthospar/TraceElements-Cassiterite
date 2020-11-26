library(ggplot2)

source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/LAICPMS/Import.R")
source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/EPMA/coloursafe.R")

ballouard <- ggplot(data=NULL, aes(y=Nb_ppm_m93/Ta_ppm_m181, x=Zr_ppm_m90/Hf_ppm_m179, colour=STYLE))+
  scale_colour_manual(values = coloursafe)+
  scale_x_continuous(limits=c(0,50),expand = c(0, 0))+
  scale_y_continuous(limits=c(0,16),expand = c(0, 0))+
  geom_point(data=pegmatite)+
  geom_point(data=greisen)+
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"))+
  labs(y="Nb/Ta",x="Zr/Hf", title=NULL, colour=NULL)
ggsave("plots/ballouard.png", ballouard, width=12, height=10, units="cm")

nbzr <- ggplot(data=NULL, aes(y=Nb_ppm_m93/Ta_ppm_m181, x=Zr_ppm_m90/Hf_ppm_m179, colour=SAMPLE))+
  scale_colour_manual(values = coloursafe)+
  annotate("rect",colour="#8888DD", linetype=2, fill=NA, size=1,
           xmin=c(1,18,26),
           xmax=c(18,46,46),
           ymin=c(0.05,1,5),
           ymax=c(4,5,16))+
  annotate("text", colour="#444444",
           x=c(36,36),
           y=c(17.5,12),
           label=c("CI", "CC"))+
  scale_x_log10(limits=c(1,100), breaks=c(1, 5, 10, 25, 50, 100, 1000), expand=expand_scale(add=c(0.01,0.1)))+
  scale_y_log10(limits=c(0.05,1100), breaks=c(1, 5, 10, 25, 50, 100, 1000), expand=expand_scale(add=c(0.02,0)))+
  theme_classic()+
  theme(legend.justification=c(0,1),
        legend.position=c(0,1),
        legend.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"))+
  labs(y="Nb/Ta",x="Zr/Hf", title=NULL, colour=NULL)

nbzr_peg <- nbzr + geom_point(data=pegmatite)
nbzr_greisen <- nbzr + geom_point(data=greisen)
print(nbzr_peg)

Ti <- ggplot(data=NULL, aes(y=Nb_ppm_m93/Ta_ppm_m181, x=Fe_ppm_m57/Mn_ppm_m55, colour=SAMPLE))+
  scale_colour_manual(values = coloursafe)+
  geom_point(data=pegmatite)+
  theme_classic()
print(Ti)

ggsave("plots/nbzr_peg.pdf", nbzr_peg, width=12, height=12, units="cm")
ggsave("plots/nbzr_greisen.pdf", nbzr_greisen, width=12, height=12, units="cm")