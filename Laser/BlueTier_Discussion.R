source("Import.R")
library(ggplot2)

plot <- ggplot(bluetier, aes(x=RELDIST))+
  geom_line(aes(y=(Fe/Al)/max(Fe/Al, na.rm=TRUE)), colour="green")+
  #geom_line(aes(y=(Zr/Hf)/max(Zr/Hf, na.rm=TRUE)), colour="red")+
  #geom_line(aes(y=(Nb/Ta)/max(Nb/Ta, na.rm=TRUE)), colour="blue")+
  #geom_line(aes(y=(U/W)/max(U/W, na.rm=TRUE)), colour="blue")+
  geom_line(aes(y=(Ga/Al)/max(Ga/Al, na.rm=TRUE)), colour="red")+
  #geom_line(aes(y=In/max(In, na.rm=TRUE)), colour="blue")+
  geom_line(aes(y=Ga/max(Ga, na.rm=TRUE)), colour="blue")+
  theme_classic()

print(plot)
