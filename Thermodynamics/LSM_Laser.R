setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")
library(ggplot2)
library(openxlsx)
library(dplyr)
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

ree <- read.xlsx("ionic_radii.xlsx", sheet="trace_REE")
ree3 <- ree[ree$charge==3,]
ree3 <- cbind(ree3, D=Di(ree3$radius, ree3$charge, TK=800), coupled=Di((ree3$radius+0.64)/2, (ree3$charge+5)/2, TK=800))
ScY <- ree3[c(1:2),]
LaLu <- ree3[-c(1:2),]

#cairo uses inches for width/height, define font family for star symbol
cairo_pdf(filename = "LSM3.pdf", width=4.134, height = 5.905/3, family="Segoe UI Symbol")
earths <- base +
  geom_point(data=LaLu, aes(x=radius, y=D), colour=coloursafe[1])+
  geom_point(data=LaLu, aes(x=(radius+0.64)/2, y=coupled), colour=coloursafe[4])+
  geom_point(data=ScY, aes(x=radius, y=D), colour=coloursafe[3])+
  geom_point(data=ScY, aes(x=(radius+0.64)/2, y=coupled), colour=coloursafe[2])
print(earths)
dev.off()


UPb <- read.xlsx("ionic_radii.xlsx", sheet="trace_radiogenic")
UPb <- cbind(UPb, D=Di(UPb$radius, UPb$charge, TK=800))

cairo_pdf(filename = "LSM4.pdf", width=4.134, height = 5.905/3, family="Segoe UI Symbol")
uranics <- base +
  geom_point(data=UPb, aes(x=radius, y=D), colour="black")
  #geom_text_repel(data=UPb, aes(x=radius, y=log10(D), label=name), colour=coloursafe[1], parse = TRUE)
print(uranics)
dev.off()
