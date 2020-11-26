#Trace element partitioning calculations
#Practical Exercise, Jon Blundy Masterclass

library(ggplot2)
library(dplyr)

#A plagioclase-phyric andesite equilibrated at 900degC, 200MPa and fO2=NNO. 
#Plagioclase rims and coexisting melt (glass) have been measured for major elements by EPMA. 
#What are the partition coefficients for Sr and REE between plagioclase (An54) and silicate melt in this magma?
#What is the magnitude of the Eu anomaly?

XAn <- 0.54 #mole fractionation of An in plag
DNa <-1.35 #Partition coefficent for Na
DCa <- 4.10 #Partition coefficent fo Ca
TK <- 900+273.15 #Temperature in Kelvin
ro2 <- 1.96e-10 #ideal ionic radius for 2+ site, in Angstroms
E2 <- 120e9 #Youngs modulus for the 2+ site, in GPa
ro3 <- 1.294e-10 #ideal ionic radius for 3+ site, in Angstroms
E3 <- 135e9 #Youngs modulus for the 3+ site, in GPa
N <- 6.023e23 #Avogadro's Number, in mol-1
R <- 0.008314e3 #Gas constant in kJ K-1 mol-1

radii2<- data.frame(
  Ca = 1.12e-10,
  Sr = 1.26e-10,
  Eu2 = 1.25e-10
)

radii3 <- data.frame(
    cation=c("La",
             "Ce",
             "Pr",
             "Nd",
             "Sm",
             "Eu3",
             "Gd",
             "Tb",
             "Dy",
             "Ho",
             "Er",
             "Tm",
             "Yb",
             "Lu"),
    ri = c(1.16e-10,
           1.143e-10,
           1.126e-10,
           1.109e-10,
           1.079e-10,
           1.066e-10,
           1.053e-10,
           1.04e-10,
           1.027e-10,
           1.015e-10,
           1.004e-10,
           9.94e-11,
           9.85e-11,
           9.77e-11)
)

DLa <- (DCa^2/DNa)*exp((529/TK)-3.705)

DSr <- DCa*exp(((-4*pi*E2*N)/(R*TK))*(((ro2/2)*(radii2$Ca-radii2$Sr)^2)-((1/3)*(radii2$Ca-radii2$Sr)^3)))

DREE <- function(ri){
  Dri <- DLa*exp(((-4*pi*E3*N)/(R*TK))*(((ro3/2)*(radii3[1,2]-ri)^2)-((1/3)*(radii3[1,2]-ri)^3)))
  return(Dri)
}
plot <- cbind(radii3, Dri=DREE(radii3[2]))

