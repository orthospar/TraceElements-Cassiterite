#produces a 'paragenetic' sequence diagram for the Mount Bischoff crystal MB3
#absolute concentrations visually estimated as a fraction of the maximum

#https://hihayk.github.io/scale/#5/5/0/60/25/-25/-100/100/FFB700/255/183/0
greyhot <- c("#CCCCCC", "#C9CAAF", "#CECC8D", "#D8CB64", "#E9C535", "#FFB700",
  "#FFA900", "#FF9200", "#FF7200", "#FF4A00", "#FF1A00")

m <- cbind(
 #zones: 1a,1b,2a,2b,3a,3b,4 ,5a,5b,6
  U  = c(10,08,02,04,06,04,00,06,06,00),
  W  = c(10,06,04,06,06,04,02,04,06,02),
  Ta = c(10,08,00,00,00,00,00,00,00,00),
  Hf = c(06,10,05,03,04,03,02,10,05,02), 
  sb = c(10,02,06,06,06,06,00,10,10,00), 
  In = c(02,02,06,08,10,06,00,06,08,00),
  Nb = c(10,09,05,05,00,00,00,00,00,00),
  Zr = c(10,10,06,04,06,04,02,10,08,02),
  Ga = c(08,02,04,07,10,05,00,07,03,00),
  Fe = c(03,04,05,07,10,06,01,07,08,00),
  V  = c(09,10,05,02,00,00,08,00,00,03), 
  Ti = c(10,08,05,02,00,00,01,00,00,00),
  Sc = c(00,00,00,02,00,02,00,10,08,00),
  Al = c(03,03,04,04,10,06,02,08,05,00)
)

png(filename="bischoff_summary.png", type="cairo", res=300,
    w=3, h=4, units="cm")
par(mar=c(0,0,0,0))
image(m, col=greyhot, xaxt="n", yaxt="n")
graphics.off()