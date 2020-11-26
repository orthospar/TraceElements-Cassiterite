setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/LaserMaps")
#import data file
data <- read.delim("Bischoff_1a.txt")

#filter data to elements of interest
data <- data[-c(1,4,9,10,14,16,25:28)]

#https://hihayk.github.io/scale/#6/6/0/0/-180/180/100/-100/76FF00/118/255/0
map.colours <- c("#808080",#start with neutral grey
                 "#6C6A95","#557CAA","#40BBBF","#2BD586","#15EA1D","#76FF00",
                 "#FFFF00","#FF8C00","#FF0000","#FF0070","#FF00FF","#9300FF")

#function to process, create, and save an image
element.map <- function(data.column, name, thresholds=data.frame(c(NA,NA)), number=0, na2max=TRUE){
  #data.column is a numeric vector (column of the imported data.frame)
  #name is a character vector of length 1 (name for titles, etc)
  #threshold is a numeric vector of length 2, representing min and max thresholds
  #number is the numeric vector of length 1 (default=1), and is the image sequence number
  #replace NAs with the maximum (TRUE/FASLE, defaults to TRUE)
  
  #remove negative counts, set to 1 (non-zero)
  data.column[data.column<0] <- 1
  
  #remove NAs, set to max (NAs appeared when Al tripped the detector)
  #overwrites NA lines between transects, but these are trimmed later
  #only if na2max is TRUE
  if(na2max==TRUE){
  data.column[is.na(data.column)] <- max(data.column, na.rm=TRUE)
  }
  

  #shape data.column into matrix for image
  #the nrow specific to this data and manually checked
  m <- matrix(data.column, nrow=281)[,-c(1:3)]
  
  #the time series of each transect does not line up exactly...
  #adjust is a numeric list of length nrow, manually edited to
  #shift each row into alignment based on the start of Sn counts
  #above the background value
  adjust <- c(25,15,8,13,13,12,11,16,10,
              8,9,5,8,6,5,4,4,6,4,3,2,
              4,1,3,3,3,1,2,3,1,4,4,2,
              3,0,4,3,0,0,2,1,2,1,2,3,
              2,3,2,2,4,2,4,1)
  
  #apply the adjustment to the matrix, row by row, shifting the
  #background end values to the beginning (which is also background)
  for(j in 1:length(adjust)){
    m[,j] <- m[c(
                (nrow(m)-adjust[j]):nrow(m),
                1:(nrow(m)-(adjust[j]+1))
                ),j]
  }
  
  #trim the matrix to remove background
  m <- m[-c(1:83,228:281),]
  
  #flip into correct orientation
  m<-m[nrow(m):1,]
  
  #produce log10 plots before thresholding
  w <- 10 #set the width
  m10<-log10(m)
  dir.create("./maps_log10", showWarnings=FALSE)
  png(filename=paste0("./maps_log10/figure_", number, ".png"),
      type="cairo", res=300,
      w=w, h=w*(13/12), units="cm")
  par(bg="white")
  par(fig=c(0,1,0,(4/12)), cex=0.6, mai=c(0.3,0.3,0.2,0.1))
  hist(m10, breaks=seq(min(m10, na.rm=TRUE), max(m10, na.rm = TRUE),l=14), col=map.colours,
       main=NULL,xlab=NULL, ylab=NULL)
  #mtext(name, side=3, cex=1)
  par(fig=c(0,1,(4/12),1),mar=c(0,0,0,0), new=TRUE)
  image(m10, col = map.colours,xaxt="n", yaxt="n", useRaster = TRUE)
  graphics.off()
  
  #check if threshold values supplied and apply
  if(is.na(thresholds[1,])==FALSE){
    m[m<thresholds[1,]] <- thresholds[1,]
    }
  if(is.na(thresholds[2,])==FALSE){
  m[m>thresholds[2,]] <- thresholds[2,]
  }
  
  #create and save thresholded images to .png
  dir.create("./maps_linear", showWarnings=FALSE)
  png(filename=paste0("./maps_linear/figure_", number, ".png"),
      type="cairo", res=300,
      w=w, h=w*(13/12), units="cm")
  par(bg="white")
  par(fig=c(0,1,0,(4/12)), cex=0.6, mai=c(0.3,0.3,0.2,0.1))
  hist(m, breaks=seq(min(m, na.rm=TRUE), max(m, na.rm = TRUE),l=14), col=map.colours,
       main=NULL,xlab=NULL, ylab=NULL)
  #mtext(name, side=3, cex=1)
  par(fig=c(0,1,(4/12),1),mar=c(0,0,0,0), new=TRUE)
  image(m, col = map.colours,xaxt="n", yaxt="n", useRaster = TRUE)
  graphics.off()
}

elements <- colnames(data)

#data.frame for the thresholds applied to each element
thresh <- data.frame("Ti49_CPS"=c(NA,NA),
               "Al27_CPS"=c(NA,5e4),
               "Sc45_CPS"=c(NA,NA),
               "V51_CPS"=c(NA,NA),
               "Mn55_CPS"=c(NA,1e4),
               "Fe57_CPS"=c(NA,8e4),
               "Ga71_CPS"=c(NA,6e4),
               "Zr90_CPS"=c(NA,15e4),
               "Nb93_CPS"=c(NA,1e5),
               "In113_CPS"=c(NA,NA),
               "Sn118_CPS"=c(2e8,NA),
               "Sb121_CPS"=c(NA,6e3),
               "Hf177_CPS"=c(NA,6e3),
               "Ta181_CPS"=c(NA,5e3), 
               "W183_CPS"=c(NA,3e6),
               "Pb206_CPS"=c(NA,2e4),
               "Th232_CPS"=c(NA,500),
               "U238_CPS"=c(NA,2e4))

for(e in 1:length(elements)){
  element.map(data[,e], elements[e], thresh[e], e)
}

NIST610 <- read.delim("G_NIST610_IntegrationNo_0")
NIST612 <- read.delim("G_NIST612_IntegrationNo_0")

#function to convert cps to mass and abundance normalised counts for Zr/Hf ratio
abund.mass <- function(data.column, threshold, mass, abundance){
  #data.column is a column of data from a data.frame
  #threshold (numeric) is the lower limit below which all is set to NA
  #mass (numeric) is the isotope mass
  #abundance (numeric) is the isotope abundance
  #returns normalised data.column
  
  norm <- data.column
  
  #threshold lower limit to reduce noise
  norm[norm<threshold] <- NA
  
  norm <- norm/(abundance*mass)
  return(norm)
  }
  
Zr <- abund.mass(data$Zr90_CPS, threshold=10000, mass=90, abundance=0.5145)
Hf <- abund.mass(data$Hf177_CPS, threshold=500, mass=177, abundance=0.186)
Nb <- abund.mass(data$Nb93_CPS, threshold=0, mass=93, abundance=1)
Ta <- abund.mass(data$Ta181_CPS, threshold=0, mass=181, abundance=1)

element.map(Zr/Hf, name="Zr/Hf", threshold=data.frame(c(NA,50)), number=19, na2max=FALSE)
element.map(Nb/Ta, name="Nb/Ta", threshold=data.frame(c(NA,50)), number=20, na2max=FALSE)
