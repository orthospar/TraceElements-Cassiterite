setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")
library(ggplot2)
library(openxlsx)
library(dplyr)
library(ggrepel)
library(Cairo)

source("Model.R")
source("coloursafe.R")

r <- seq(0.3,1.5,0.01)
curves <- data.frame(radius=r, Di4=Di(r, 4, 800), Di3=Di(r, 3, 800), Di2=Di(r, 2, 800), Di5=Di(r, 5, 800), Di6=Di(r, 6, 800), Di1=Di(r, 1, 800))

base <- ggplot(curves, aes(x=radius))+
  scale_colour_manual(values=coloursafe[c(2,6,1,4,3,5)])+
  geom_line(aes(y=Di4), colour=coloursafe[4], linetype=1)+
  geom_line(aes(y=Di3), colour=coloursafe[1], linetype=5)+
  geom_line(aes(y=Di5), colour=coloursafe[3], linetype=4)+
  geom_line(aes(y=Di6), colour=coloursafe[5], linetype=6)+
  geom_line(aes(y=Di2), colour=coloursafe[6], linetype=2)+
  geom_line(aes(y=Di1), colour=coloursafe[2], linetype=3)+
  annotate(geom="text", x = 0.69, y = 1, label=sprintf('\u2605'), colour=coloursafe[7])+
  scale_y_log10(breaks=c(1e-10,1e-8,1e-6,1e-4,1e-2,1e0),
                labels=NULL)+
  coord_cartesian(xlim=c(0.4,1.2), ylim=c(1e-10,1e+2))+
  theme_classic()+
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        panel.background = element_rect(fill="grey95"),
        panel.grid.major.y = element_line(size=0.5, color="white"))+
  labs(x=NULL, y=NULL)

ions <- read.xlsx("ionic_radii.xlsx", sheet="trace_all")
ions <- filter(ions, spin=="H"|is.na(spin)==TRUE)
ions <- data.frame(cbind(ions, D=Di(ions$radius, ions$charge, 800)))
transmet <- ions[ions$facet_panel=="Transition Metals",]
rest <- ions[ions$facet_panel!="Transition Metals",]
rest$block <- factor(rest$block, levels=c("s-Block","p-Block","d-Block"))

#cairo uses inches for width/height, define font family for star symbol
cairo_pdf(filename = "LSM.pdf", width=4.134, height = 5.905, family="Segoe UI Symbol")
  p1<-base +
  geom_point(data=transmet, aes(x=radius, y=D, colour=factor(charge))) +
  #geom_text_repel(data=transmet, aes(x=radius, y=D, label=name), parse=TRUE)+
  facet_grid(rows=vars(row))
  print(p1)
dev.off()

cairo_pdf(filename = "LSM2.pdf", width=4.134, height = 5.905, family="Segoe UI Symbol")
  p2 <- base +
  geom_point(data=rest, aes(x=radius, y=D, colour=factor(charge))) +
  #geom_text_repel(data=rest, aes(x=radius, y=D, label=name), parse=TRUE)+
  facet_grid(rows=vars(block))
   print(p2)
dev.off()
