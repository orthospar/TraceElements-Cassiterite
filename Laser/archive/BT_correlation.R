library(dplyr)
library(ggplot2)
library(png)
library(grid)
library(imager)

blue <- read.csv("BT_laser.csv")
blue <- data.frame(lapply(blue, function(x) gsub("Below LOD","0",x)),stringsAsFactors=FALSE)
blue[,c(4,9,14:157)] <- data.frame(lapply(blue[,c(4,9,14:157)], as.numeric))
blue[,2:3] <- data.frame(lapply(blue[,2:3], as.factor))

img <- load.image("BlueTierStrat.png")
#export full image transect from photoshop

bluetier <- ggplot(data=blue, aes(x=RELDIST))+
  geom_line(aes(y=Nb_ppm_m93/Ta_ppm_m181, colour="Nb/Ta"))+
  geom_line(aes(y=Zr_ppm_m90/Hf_ppm_m179, colour="Zr/Hf"))+
  geom_line(aes(y=In_ppm_m113, colour="In"))+
  scale_x_continuous(limits = c(0,7500), expand = c(0, 0))+
  scale_y_continuous(limits = c(-10,40), expand = c(0, 0))+
  annotation_raster(img, 0, 7500, -10,0)+
  theme_classic()+
  labs(x="Distance (um)",y="Nb/Ta and Zr/Hf Ratio")
  
b2 <- ggplot(data=blue, aes(x=RELDIST))+
  geom_line(aes(y=log10(Nb_ppm_m93), colour="Nb"))+
  geom_line(aes(y=log10(Ta_ppm_m181), colour="Ta"))+
  geom_line(aes(y=log10(Zr_ppm_m90), colour="Zr"))+
  geom_line(aes(y=log10(Hf_ppm_m179), colour="Hf"))+
  scale_x_continuous(limits = c(0,7500), expand = c(0, 0))+
  scale_y_continuous(limits = c(-2.5,5), expand = c(0, 0))+
  annotation_raster(img, 0, 7500, -2.5, -1)+
  theme_classic()+
  labs(x="Distance (um)",y="contents (ppm)")

b_NbTa <- ggplot(data=blue, aes(x=RELDIST))+
  geom_line(aes(y=Nb_ppm_m93/Ta_ppm_m181, colour="Nb/Ta"))+
  scale_x_continuous(limits = c(0,7500), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0))+
  theme_classic()+
  labs(x="Distance (um)",y="Nb/Ta and Zr/Hf Ratio")

b_ZrHf <- ggplot(data=blue, aes(x=RELDIST))+
  geom_line(aes(y=Zr_ppm_m90/Hf_ppm_m179, colour="Zr/Hf"))+
  scale_x_continuous(limits = c(0,7500), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0))+
  theme_classic()+
  labs(x="Distance (um)",y="Nb/Ta and Zr/Hf Ratio")

b_In <- ggplot(data=blue, aes(x=RELDIST))+
  geom_line(aes(y=In_ppm_m113, colour="In"))+
  scale_x_continuous(limits = c(0,7500), expand = c(0, 0))+
  scale_y_continuous(limits = c(0,40), expand = c(0, 0))+
  theme_classic()+
  labs(x="Distance (um)",y="Nb/Ta and Zr/Hf Ratio")

#ggsave("plots/bluetier2.png", b2, width=45, height=15, units="cm")
ggsave("plots/blue_NbTa.png", b_NbTa, width=45, height=4, units="cm")
ggsave("plots/blue_ZrHf.png", b_ZrHf, width=45, height=4, units="cm")
ggsave("plots/blue_In.png", b_In, width=45, height=4, units="cm")