source("Import.R")
suppressWarnings(source("Import.R"))

#https://hihayk.github.io/scale/#6/6/0/0/-180/180/100/-100/76FF00/118/255/0
greyhot <- c("#CCCCCC", "#C9CAAF", "#CECC8D", "#D8CB64", "#E9C535", "#FFB700",
             "#FFA900", "#FF9200", "#FF7200", "#FF4A00", "#FF1A00")


normalise <- function(x){
  x[is.na(x)==TRUE] <- min(x, na.rm=TRUE)
  x <- x/max(x, na.rm=TRUE)
  return(x)
}

m <- as.matrix(cbind(
  normalise(bluetier$Al),
  normalise(bluetier$Sc),
  normalise(bluetier$Ti),
  normalise(bluetier$V),
  normalise(bluetier$Mn),
  normalise(bluetier$Ga),
  normalise(bluetier$Y),
  normalise(bluetier$Zr),
  normalise(bluetier$Nb),
  normalise(bluetier$In),
  normalise(bluetier$Sb),
  normalise(bluetier$Lu),
  normalise(bluetier$Hf),
  normalise(bluetier$Ta),
  normalise(bluetier$W),
  normalise(bluetier$U)
))

image(m, col=greyhot, xaxt="n", yaxt="n")
