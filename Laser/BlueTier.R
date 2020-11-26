source("Import.R")

elements <- c("Fe", "Ti", "Nb", "Ta", "Zr", "Hf", "W", "Mo", "Al", "Mn", "Sc", "In", "Sb", "Ga", "Y", "U", "Th", "Pb")

#https://hihayk.github.io/scale/#8/7/0/0/-180/180/100/-100/76FF00/118/255/0
map.colours15 <- c("#75708F","#606D9F","#5088AF","#40BBBF","#30CF99","#20DF57",
                   "#24EF10","#76FF00","#F2FF00","#FFBB00","#FF2500","#FF0000",
                   "#FF00AE","#FF00FF","#9300FF")
map.colours11 <- c("#557CAA","#40BBBF","#2BD586","#15EA1D","#76FF00",
                   "#FFFF00","#FF8C00","#FF0000","#FF0070","#FF00FF","#9300FF")

BT <- log10(as.matrix(bluetier[elements]))
BT <- t(BT[nrow(BT):1,])
scale1 <- seq(-3.25, 3.75,l=15)
nbta <- as.matrix(bluetier$Nb/bluetier$Ta)
nbta <- t(nbta[nrow(nbta):1,])
zrhf <- as.matrix(bluetier$Zr/bluetier$Hf)
zrhf <- t(zrhf[nrow(zrhf):1,])
scale2 <- seq(1,29,l=15)

norm <- as.matrix(bluetier[elements])
norm <- log10(apply(norm, 2, function(x){x/median(x, na.rm=TRUE)}))
norm <- t(norm[nrow(norm):1,])
scale3 <- seq(-2.75, 2.25,l=11)

cairo_pdf(filename="bluetier_profile.pdf",
          width=6.67, height=7.875, bg="transparent")

w <- 40
h <- 20
par(fig=c(0.1,0.1+0.2625,0,(2/h)), cex=0.6, mai=c(0.3,0.2,0.3,0.2))
hist(scale1, breaks=seq(-3.5, 4,l=16), col=map.colours15, border="transparent",
     main=NULL,xlab=NULL, ylab=NULL, axes=FALSE)
#axis(side=1)

par(fig=c(0.47-0.13125,0.47+0.13125,0,(2/h)), cex=0.6, mai=c(0.3,0.4,0.3,0),new=TRUE)
hist(scale2, breaks=seq(0,30,l=16), col=map.colours15, border="transparent",
     main=NULL,xlab=NULL, ylab=NULL, axes=FALSE)
#axis(side=1)

par(fig=c(0.87-0.2625,0.87,0,(2/h)), cex=0.6, mai=c(0.3,0.4,0.3,0),new=TRUE)
hist(scale3, breaks=seq(-3,2.5,l=12), col=map.colours11, border="transparent",
     main=NULL,xlab=NULL, ylab=NULL, axes=FALSE)
#(side=1)

par(fig=c(0,(18/w),(2/h),1),mar=c(0,0,0,0), new=TRUE)
image(BT, zlim=c(-3.5,4), breaks=seq(-3.5, 4,l=16), col = map.colours15, xaxt="n", yaxt="n", useRaster = TRUE, bty="n")

par(fig=c((19/w),(20/w),(2/h),1),mar=c(0,0,0,0), new=TRUE)
image(nbta, zlim=c(0,30), breaks=seq(0,30,l=16), col = map.colours15, xaxt="n", yaxt="n", useRaster = TRUE, bty="n")

par(fig=c((20/w),(21/w),(2/h),1),mar=c(0,0,0,0), new=TRUE)
image(zrhf, zlim=c(0,30), breaks=seq(0,30,l=16), col = map.colours15, xaxt="n", yaxt="n", useRaster = TRUE, bty="n")

par(fig=c((22/w),(40/w),(2/h),1),mar=c(0,0,0,0), new=TRUE)
image(norm, zlim=c(-3,2.5), breaks=seq(-3,2.5,l=12), col = map.colours11, xaxt="n", yaxt="n", useRaster = TRUE, bty="n")

graphics.off()