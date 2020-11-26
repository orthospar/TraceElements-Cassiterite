library(openxlsx)
library(ggplot2)

Cp_coefficients<-read.xlsx("thermo.xlsx", sheet="Cp")

#Heat Capacity Equation
Cp <- function(TK,Phase){
  v <- Cp_coefficients[Cp_coefficients$phase==Phase,]
  HeatCapacity <- v$a+v$b*TK+v$c/(TK^2)+v$d/(TK^0.5)+v$e*(TK^2)
  return(HeatCapacity)
}

#Integral of the Heat Capacity Equation from standard state (298K)
Int_Cp <- function(TK,Phase){
  v <- Cp_coefficients[Cp_coefficients$phase==Phase,]
  HeatCapacity <- v$a*(TK-298)+(v$b/2)*(TK^2-298^2)+v$c*(1/TK-1/298)+(2*v$d)*(TK^0.5-298^0.5)+(v$e/3)*(TK^3-298^3)
  return(HeatCapacity)
}

dH <- function(TK, Product, Reactants){
  p <- 0
  for(i in Product){
    p <- p + Int_Cp(TK, i)
  }
  r <- 0
  for(i in Reactants){
    r <- r + Int_Cp(TK, i)
  }
  dHf_298 <- Cp_coefficients$dHf_298[Cp_coefficients$phase==Product]
  return(dHf_298 + p - r)
}

#temp_range <- seq(300,800,10)
dH_quartz <- dH(800, c("quartz(alpha)"), c("O2(gas)", "Si(crystal)"))
