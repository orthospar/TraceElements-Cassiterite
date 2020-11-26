library(ggplot2)
setwd("C:/Users/Jason/OneDrive - The University of Western Australia/Analysis/LaserMaps")
#import data file
bischoff_map <- read.delim("Bischoff_1a.txt")


#function to convert cps to mass and abundance normalised counts for Zr/Hf ratio
abund.mass <- function(data.column, threshold, mass, abundance){
  #data.column is a column of data from a data.frame
  #threshold (numeric) is the lower limit below which all is set to NA
  #mass (numeric) is the isotope mass
  #abundance (numeric) is the isotope abundance
  #returns normalised data.column
  
  norm <- data.column
  
  #threshold lower limit to reduce noise
  norm[norm<threshold] <- NA
  
  norm <- norm/(abundance*mass)
  return(norm)
}

NbTa <- abund.mass(bischoff_map$Nb93_CPS, threshold=0, mass=93, abundance=1)/
        abund.mass(bischoff_map$Ta181_CPS, threshold=0, mass=181, abundance=1)
ZrHf <- abund.mass(bischoff_map$Zr90_CPS, threshold=10000, mass=90, abundance=0.5145)/
        abund.mass(bischoff_map$Hf177_CPS, threshold=500, mass=177, abundance=0.186)
m <- data.frame(NbTa=NbTa[ZrHf>2], ZrHf=ZrHf[ZrHf>2])

setwd("C:/Users/Jason/OneDrive - The University of Western Australia/Analysis/Laser")
suppressWarnings(source("Import.R"))
source("coloursafe.R")

p <- ggplot(common, aes(x=Zr/Hf, y=Nb/Ta, colour=interaction(LOCALITY,SAMPLE, sep=" - ")))+
  geom_point(colour="grey100")+
  scale_colour_manual(values = coloursafe)+
  theme_classic()+
  annotate("rect",colour="grey70", linetype=2, fill=NA, size=1,
           xmin=c(1,18,26),
           xmax=c(18,46,46),
           ymin=c(0.05,1,5),
           ymax=c(4,5,16))+
  annotate("curve", colour="grey80", linetype=1, size=2,
           x=c(36), xend=c(13), y=c(8), yend=c(1.5),
           curvature=-0.5, angle=120, arrow=arrow(length=unit(0.5, "cm")),
           lineend="round")+
  annotate("text", colour="grey20",
           x=c(36,36),
           y=c(17.5,12),
           size=6,
           label=c(sprintf('\u2609'),sprintf('\u2641')))+
  scale_x_log10(limits=c(1,100), breaks=c(1, 2, 10, 30, 50, 100), expand=expand_scale(add=c(0.01,0.1)))+
  scale_y_log10(limits=c(0.05,1100), breaks=c(1, 4, 10, 30, 100, 1000), expand=expand_scale(add=c(0.02,0)))+
  theme(panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "transparent"),
        legend.justification =c(0,1),
        legend.position = c(0,0.95),
        legend.text=element_text(size=14))+
  labs(colour=NULL)+
  guides(alpha="none")

Moolyella <- c("Moolyella - A", "Moolyella - B","Moolyella - C","Moolyella - P1","Moolyella - P2")
Other <- c("Mount Francisco", "Siffleetes Reward", "Bamboo Creek", "Hang Gong", "Trident")
yilgarn <- c("Tin Shafts, Poona", "White Load, Poona", "Greenbushes")

Peg1 <- common[which(common$LOCALITY=="Moolyella"),]
Peg2 <- common[which(common$LOCALITY %in% Other),]
Peg3 <- common[which(common$LOCALITY %in% yilgarn),]
greisen <- common[common$STYLE=="Greisen",]
blue <- greisen[greisen$LOCALITY=="Blue Tier",]
greisen <- greisen[greisen$LOCALITY!="Blue Tier",]
vein <- common[common$STYLE=="Vein",]
skarn <- common[common$STYLE=="Skarn",]

p1 <- p +
  stat_density_2d(data=m, aes(x=ZrHf, y=NbTa, alpha=..level..), colour="#D55E00")+
  geom_point(data=skarn)+
  geom_point(data=vein)
p2 <- p + geom_point(data=Peg1)
p3 <- p + geom_point(data=Peg2)
p4 <- p + geom_point(data=Peg3)
p5 <- p + geom_point(data=greisen)
p6 <- p + geom_point(data=blue)

p7 <- ggplot(common, aes(x=Zr/Hf, y=Nb/Ta))+
  scale_colour_manual(values = coloursafe)+
  geom_point(aes(colour=STYLE), alpha=0.8)+
  theme_classic()+
  annotate("text", colour="grey20",
           x=c(36,36),
           y=c(17.5,12),
           size=6,
           label=c(sprintf('\u2609'),sprintf('\u2641')))+
  annotate("rect",colour="grey70", linetype=2, fill=NA, size=1,
           xmin=c(1,18,26),
           xmax=c(18,46,46),
           ymin=c(0.05,1,5),
           ymax=c(4,5,16))+
  annotate("curve", colour="grey30", linetype=1, size=2,
           x=c(36), xend=c(13), y=c(8), yend=c(1.5),
           curvature=-0.5, angle=120, arrow=arrow(length=unit(0.5, "cm")),
           lineend="round")+
  scale_x_log10(limits=c(1,100), breaks=c(1, 2, 10, 30, 50, 100), expand=expand_scale(add=c(0.01,0.1)))+
  scale_y_log10(limits=c(0.05,1100), breaks=c(1, 4, 10, 30, 100, 1000), expand=expand_scale(add=c(0.02,0)))+
  theme(panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "transparent"),
        legend.justification =c(0,1),
        legend.position = c(0,0.95),
        legend.text=element_text(size=14))+
  labs(colour=NULL)+
  guides(alpha="none") 

w<-15
ggsave(p7, filename="NbZrInterp.png", w=w, h=0.75*w, units="cm", dpi=600)
ggsave(p1, filename ="bischoff.png", w=w, h=0.75*w, units="cm", dpi=600)
ggsave(p2, filename ="NbZr2.png", w=w, h=0.75*w, units="cm", dpi=600)
ggsave(p3, filename ="NbZr3.png", w=w, h=0.75*w, units="cm", dpi=600)
ggsave(p4, filename ="NbZr4.png", w=w, h=0.75*w, units="cm", dpi=600)
ggsave(p5, filename ="NbZr5.png", w=w, h=0.75*w, units="cm", dpi=600)
ggsave(p6, filename ="NbZr6.png", w=w, h=0.75*w, units="cm", dpi=600)

l <- ggplot(common, aes(x=Zr, y=Hf, colour=interaction(LOCALITY,SAMPLE, sep=" - ")))+
  geom_point(colour="grey100")+
  scale_colour_manual(values = coloursafe)+
  theme_classic()+
  geom_abline(intercept=0, slope=1, colour="grey80")+
  geom_abline(intercept=0, slope=1/10, colour="grey80")+
  geom_abline(intercept=0, slope=1/36, colour="grey40")+
  geom_abline(intercept=0, slope=1/100, colour="grey80")+
  scale_x_continuous(limits=c(0.1,10000), breaks=c(0.01, 1, 10, 100, 1000, 10000),
                labels=c("", "1 ppm", "", "100 ppm", "", "1 wt%"))+
  scale_y_continuous(limits=c(0.01,1000), breaks=c(0.01, 0.1, 1, 10, 100, 1000),
                labels=c("10ppb","", "1 ppm", "", "100 ppm", ""))+
  coord_trans(x="log10", y="log10") +
  theme(panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "transparent"),
        legend.justification =c(0,1),
        legend.position = c(0,0.95),
        legend.text=element_text(size=14))+
  labs(colour=NULL)+
  guides(alpha="none")


l1 <- l +
  geom_point(data=skarn)+
  geom_point(data=vein)
l2 <- l + geom_point(data=Peg1)
l3 <- l + geom_point(data=Peg2)
l4 <- l + geom_point(data=Peg3)
l5 <- l + geom_point(data=greisen)
l6 <- l + geom_point(data=blue)

ggsave(l1, filename ="ZrHf1.png", w=w, h=0.75*w, units="cm")
ggsave(l2, filename ="ZrHf2.png", w=w, h=0.75*w, units="cm")
ggsave(l3, filename ="ZrHf3.png", w=w, h=0.75*w, units="cm")
ggsave(l4, filename ="ZrHf4.png", w=w, h=0.75*w, units="cm")
ggsave(l5, filename ="ZrHf5.png", w=w, h=0.75*w, units="cm")
ggsave(l6, filename ="ZrHf6.png", w=w, h=0.75*w, units="cm")

g <- ggplot(common, aes(x=Nb, y=Ta, colour=interaction(LOCALITY,SAMPLE, sep=" - ")))+
  geom_point(colour="grey100")+
  scale_colour_manual(values = coloursafe)+
  theme_classic()+
  geom_abline(intercept=0, slope=1, colour="grey80")+
  geom_abline(intercept=0, slope=1/12, colour="grey40")+
  geom_abline(intercept=0, slope=1/17.5, colour="grey40")+
  geom_abline(intercept=0, slope=1/100, colour="grey80")+
  geom_abline(intercept=0, slope=1/1000, colour="grey80")+
  scale_x_continuous(limits=c(0.01,100000), breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                     labels=c("","", "1 ppm", "", "100 ppm", "", "1 wt%", ""))+
  scale_y_continuous(limits=c(0.01,100000), breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                     labels=c("","", "1 ppm", "", "100 ppm", "", "1 wt%", ""))+
  coord_trans(x="log10", y="log10") +
  theme(panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "transparent"),
        legend.justification =c(0,1),
        legend.position = c(0,0.95),
        legend.text=element_text(size=14))+
  labs(colour=NULL)+
  guides(alpha="none")

g1 <- g +
  geom_point(data=skarn)+
  geom_point(data=vein)
g2 <- g + geom_point(data=Peg1)
g3 <- g + geom_point(data=Peg2)
g4 <- g + geom_point(data=Peg3)
g5 <- g + geom_point(data=greisen)
g6 <- g + geom_point(data=blue)


ggsave(g1, filename ="NbTa1.png", w=w, h=0.75*w, units="cm")
ggsave(g2, filename ="NbTa2.png", w=w, h=0.75*w, units="cm")
ggsave(g3, filename ="NbTa3.png", w=w, h=0.75*w, units="cm")
ggsave(g4, filename ="NbTa4.png", w=w, h=0.75*w, units="cm")
ggsave(g5, filename ="NbTa5.png", w=w, h=0.75*w, units="cm")
ggsave(g6, filename ="NbTa6.png", w=w, h=0.75*w, units="cm")

