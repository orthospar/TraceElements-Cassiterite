setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Thermodynamics")
library(openxlsx)
library(ggplot2)

S<-read.xlsx("thermodynamics.xlsx", sheet="S") #Standard Entropy table, in J mol-1 K-1
H<-read.xlsx("thermodynamics.xlsx", sheet="dH")  #Standard Enthalpy of Formation table, in kJ mol-1 K-1
H[,-1]<-H[,-1]*1000 #convert enthalpy from kJ to J
R<-8.3144589 #Gas constant, J mol-1 K-1

#select the temperature of interest (limited to T intervals from table)
T_i <- which(S$TK==800)

#calculate the mid-point for NNO at the temperature of interest
fO2_NNO <- ((((2*H$NiO)-(2*H$Ni+H$O2))-S$TK*((2*S$NiO)-(2*S$Ni+S$O2)))/(R*S$TK*log(10)))[T_i]

#Calculate the mid-points for each buffer reaction as a difference from NNO
dNNO <- data.frame(
  #the main buffer reactions  
  QIF   = ((((H$fayalite)-(H$quartz+2*H$Fe+H$O2))-S$TK*((S$fayalite)-(S$quartz+2*S$Fe+S$O2)))/(R*S$TK*log(10)))[T_i]-fO2_NNO,
  QFM   = ((((2*H$magnetite+3*H$quartz) - (3*H$fayalite+H$O)) - S$TK*((2*S$magnetite+3*S$quartz) - (3*S$fayalite+S$O)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  MH    = ((((6*H$hematite) - (4*H$magnetite+H$O2)) - S$TK*((6*S$hematite) - (4*S$magnetite+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  WM    = ((((2*H$magnetite) - (6*H$FeO+H$O2)) - S$TK*((2*S$magnetite) - (6*S$FeO+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  CHO   = ((((0.5*H$CO2+H$H2O) - (0.5*H$CH4+H$O2)) - S$TK*((0.5*S$CO2+S$H2O) - (0.5*S$CH4+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  IW    = ((((2*H$FeO) - (2*H$Fe+H$O2)) - S$TK*((2*S$FeO) - (2*S$Fe+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  WH    = ((((2*H$hematite) - (4*H$FeO+H$O2)) - S$TK*((2*S$hematite) - (4*S$FeO+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  H2O   = ((((2*H$H2O) - (2*H$H2+H$O2)) - S$TK*((2*S$H2O) - (2*S$H2+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  
  #the buffer reactions of interest
  SnSnO2= ((((H$SnO2) - (H$Sn+H$O2)) - S$TK*((S$SnO2) - (S$Sn+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  V0_2  = ((((2*H$VO) - (2*H$V+H$O2)) - S$TK*((2*S$VO) - (2*S$V+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  V2_3  = ((((2*H$V2O3) - (4*H$VO+H$O2)) - S$TK*((2*S$V2O3) - (4*S$VO+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  V3_5 =  ((((H$V2O5) - (H$V2O3+H$O2)) - S$TK*((S$V2O5)-(S$V2O3+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  #not calculated: entropy of formation values too high for VO2 in JANAF tables, returns spurious values
  V3_4  = ((((4*H$VO2) - (2*H$V2O3+H$O2)) - S$TK*((4*S$VO2) - (2*S$V2O3+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  V4_5  = ((((2*H$V2O5) - (4*H$VO2+H$O2)) - S$TK*((2*S$V2O5) - (4*S$VO2+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  
  Cr0_2 = ((((2*H$CrO) - (2*H$Cr+H$O2)) - S$TK*((2*S$CrO) - (2*S$Cr+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  Cr2_3 = ((((2*H$Cr2O3) - (4*H$CrO+H$O2)) - S$TK*((2*S$Cr2O3) - (4*S$CrO+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  Cr3_4 = ((((4*H$CrO2) - (2*H$Cr2O3+H$O2)) - S$TK*((4*S$CrO2) - (2*S$Cr2O3+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  Cr4_6 = ((((2*H$CrO3) - (2*H$CrO2+H$O2)) - S$TK*((2*S$CrO3) - (2*S$CrO2+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  
  Cr2_4 = ((((2*H$CrO2) - (2*H$CrO+H$O2)) - S$TK*((2*S$CrO2) - (2*S$CrO+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO,
  Cr2_6 = ((((H$CrO3) - (H$CrO+H$O2)) - S$TK*((S$CrO3) - (S$CrO+S$O2)))/ (R*S$TK*log(10)))[T_i] - fO2_NNO

)

#calculate the k-terms
k <- data.frame(
  Sn = 10^((dNNO$SnSnO2*4)/-4),
  
  V5 <- 10^((dNNO$V3_5*2)/-4),
  V3 <- 10^((dNNO$V2_3*1)/-4),
  V2 <- 10^((dNNO$V0_2*2)/-4),
  
  
  
  Fe2= 10^((dNNO$IW*2/-4)),
  Fe3= 10^((dNNO$WH*1/-4))
)

#The range in fO2 (units of dNNO) to model/plot
fO2_range <- seq(-50,30,0.1)

#calculate the speciation ratio of highest state across that range
speciation <- data.frame(
  fO2_range,
  Sn4  = (k$Sn*10^((fO2_range*4)/4))/ (1+(k$Sn*10^((fO2_range*4)/4))),   #Sn0 to Sn4  
  
  V5  = (k$V5*10^((fO2_range*2)/4))/ (1+(k$V5*10^((fO2_range*2)/4))), #V3 to V5
  V3  = (k$V3*10^((fO2_range*1)/4))/ (1+(k$V3*10^((fO2_range*1)/4))), #V2 to V3
  V2  = (k$V2*10^((fO2_range*2)/4))/ (1+(k$V2*10^((fO2_range*2)/4))), #V0 to V2

  Fe3  = (k$Fe3*10^((fO2_range*1)/4))/ (1+(k$Fe3*10^((fO2_range*1)/4))), #Fe2 to Fe3
  Fe2  = (k$Fe2*10^((fO2_range*2)/4))/ (1+(k$Fe2*10^((fO2_range*2)/4))) #Fe0 to Fe2
)

#Build the base plot
p <- ggplot(speciation, aes(x=fO2_range))+
  annotate(geom="rect", xmin=dNNO$H2O, xmax=0, ymin=0, ymax=1, fill="grey80")+
  geom_vline(xintercept = dNNO$MH, colour="red", linetype=2)+
  geom_vline(xintercept = dNNO$QFM, colour="#33a02c", linetype=2)+
  geom_vline(xintercept = dNNO$WM, colour="#1f78b4", linetype=2)+
  geom_vline(xintercept = dNNO$QIF, colour="black", linetype=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))+
  coord_cartesian(xlim=c(-50,30), ylim=c(0,1.01), expand=FALSE)+
  labs(x=expression("log"[10]~italic("f")[O[2]]~(Delta*"NNO")), y=expression(over(" M"^"n+",Sigma*"M")))

V <- p +
  geom_line(aes(y=1-(V2+V3+V5)), colour="red")+
  geom_line(aes(y=V2-(V3+V5)), colour="orange")+
  geom_line(aes(y=V3-V5), colour="yellow")+
  geom_line(aes(y=V5), colour="green")
print(V)
