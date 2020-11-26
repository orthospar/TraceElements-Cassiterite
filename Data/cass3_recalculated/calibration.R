library(dplyr)

setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/LAICPMS/cass3_recalculated")

IntStdKey <- read.csv("Jason_Cass_3_Fe_STDS-KEY.csv")
Fe57 <- read.csv("Laser_Assorted_All_Integrations_Fe57.csv")%>%
  filter(!grepl('NIST', Comments)) #remove NIST standards from calculation
Fe57 <- Fe57[IntStdKey$CONFIDENCE=='POINT',]
Sn119 <- read.csv("Laser_Assorted_All_Integrations_Sn119.csv")%>%
  filter(!grepl('NIST', Comments))
Sn119 <- Sn119[IntStdKey$CONFIDENCE=='POINT',]

w <- 15
png(filename="calibration.png",
    type="cairo", res=300,
    w=w, h=w*(13/12), units="cm")

par(fig=c(0,0.5,0.5,1), cex=0.6)
#Ti calculation comparison
lm_Ti <- lm(Fe57$Ti_ppm_m49 ~ 0 + Sn119$Ti_ppm_m49)
plot(Sn119$Ti_ppm_m49, Fe57$Ti_ppm_m49, main="Ti49 (ppm)",
     xlab="Reduced with Sn119", ylab="Reduced with Fe57",
     sub=paste0("Ti(Fe57) = ", round(coef(lm_Ti), 4)," Ti(Sn119)"))
abline(lm_Ti, col="blue")

par(fig=c(0.5,1,0.5,1), cex=0.6, new=TRUE)
#Zr calculation comparison
lm_Zr <- lm(Fe57$Zr_ppm_m90 ~ 0 + Sn119$Zr_ppm_m90)
plot(Sn119$Zr_ppm_m90, Fe57$Zr_ppm_m90, main="Zr90 (ppm)",
     xlab="Reduced with Sn119", ylab="Reduced with Fe57",
     sub=paste0("Zr(Fe57) = ", round(coef(lm_Zr), 4)," Zr(Sn119)"))
abline(lm_Zr, col="red")

par(fig=c(0,0.5,0,0.5), cex=0.6, new=TRUE)
#W calculation comparison
lm_W <- lm(Fe57$W_ppm_m183 ~ 0 + Sn119$W_ppm_m183)
plot(Sn119$W_ppm_m183, Fe57$W_ppm_m183, main="W183 (ppm)",
     xlab="Reduced with Sn119", ylab="Reduced with Fe57",
     sub=paste0("W(Fe57) = ", round(coef(lm_W), 4)," W(Sn119)"))
abline(lm_W, col="brown")

par(fig=c(0.5,1,0,0.5), cex=0.6, new=TRUE)
#Fe57 Internal standard vs Fe calculated from Sn119
plot(Sn119$Fe_ppm_m57, IntStdKey[IntStdKey$CONFIDENCE=='POINT',]$Fe*10000,
     main="Fe57 (ppm)", xlab="Reduced with Sn119", ylab="EPMA derived values") 
abline(lm_Ti, col="blue")
abline(lm_Zr, col="red")
abline(lm_W, col="brown")
graphics.off()