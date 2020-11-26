source("C:/Users/Jason/OneDrive - research.uwa.edu.au/Data/EPMA Transects/import.R")

library(ggplot2)

local <- split(data, data$SAMPLE)

beech <- local$CS12_Beechworth_C2
mool <- local$CS14_Moolyella_B
bamb <- local$CS14_Bamboo_I
pedra <- local$CS14_GoiasPB137_J
tin <- local$CS14_Tinshafts_D
salt3 <- local$CS13_Saltwater_E3
ren <- local$CS14_Renison_K
buriti <- local$CS13_GoiasBEBT_D3
salt1<- local$CS13_Saltwater_E1
white <- local$CS13_Whiteload_D4

p2 <- ggplot(tin, aes(x=RELDIST, y=Fe.WT.))+
  geom_line()
print(p2)
