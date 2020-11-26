setwd("C:/Users/Jason/OneDrive - research.uwa.edu.au/Analysis/Laser")
require(ggplot2)
require(Cairo)
require(dplyr)

source("Import.R")
source("coloursafe.R")
coloursafe <- coloursafe[c(1,6,4,2:3,5,7:9)]

metadata <- c("SAMPLE", "STYLE","IS")
elements <- c("Li","Al",
              "Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As",
              "Y","Zr","Nb","Mo","In","Sb",
              "La", "Ce", "Pr", "Nd","Sm", "Eu","Gd", "Tb","Dy","Ho","Er","Tm","Yb","Lu",
              "Hf","Ta","W","Pb",
              "Th","U")
detlimit <- paste0(elements, "_LOD")
join <- c(metadata,elements,detlimit)



#need to use full_join to add in missing columns
joined <- full_join(lilly[,colnames(lilly) %in% join], jason[,colnames(jason) %in% join]) %>%
  full_join(JCU[,colnames(JCU) %in% join])%>%
  full_join(bluetier[,colnames(bluetier) %in% join])%>%
  full_join(tony[,colnames(tony) %in% join])

localities <- c("Hayes Creek","Storey's Creek","Renison Bell","Mount Bischoff",
                          "Greenbushes - B","Greenbushes - A",
                          "Tin Shafts, Poona","White Load, Poona",
                          "Moolyella - C","Moolyella - B","Moolyella - A","Moolyella - P2","Moolyella - P1",
                          "Mount Francisco","Sifleetes Reward",
                          "Bamboo","Hang Gong","Trident",
                          "Forsayth","Borgamba","Beechworth",
                          "Blue Tier","Saltwater Creek",
                          "Buriti Mine","Pedra Branca")

joined$SAMPLE <- as.factor(joined$SAMPLE)
joined$SAMPLE <- factor(joined$SAMPLE, levels=localities)

joined$STYLE <- as.factor(joined$STYLE)
joined$IS <- as.factor(joined$IS)

violin <- function(data, element){
  s<-list()
  for(l in levels(data$SAMPLE)){s<-c(s,levels(data$STYLE)[data$STYLE[data$SAMPLE==l][1]])}
  s<-unlist(s)
  analyses <- data.frame(SAMPLE = localities,
                         STYLE = s,
                         y = c(rep(1e6,length(levels(data$SAMPLE)))),
                         label = cbind(by(data, data$SAMPLE,function(x){
                           n <- sum(!is.na(x[,which(colnames(x)==element)]))
                           if(n != 0){paste(n)}else{paste("")}
                         }))
                         )
  
  LLD <- which(colnames(data)==paste0(element,"_LOD"))
  
  p <- ggplot(data=data, aes(x=SAMPLE, y=data[,which(colnames(data)==element)]))+
    scale_fill_manual(values=coloursafe)+
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+4,ymax=1e+6,fill="#cccccc")+
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+2,ymax=1e+4,fill="#d6d6d6")+
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+0,ymax=1e+2,fill="#e0e0e0")+
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-3,ymax=1e-0,fill="#ebebeb")+
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-4,ymax=1e-3,fill="#f2f2f2")+
    geom_boxplot(aes(y=data[,LLD]), outlier.shape = NA, na.rm=TRUE, colour="#cccccc")+
    geom_jitter(aes(fill=IS, colour=IS), width=0.2, alpha=0.3, na.rm=TRUE)+
    geom_violin(fill=NA, colour="black", scale="width", width=0.5, na.rm=TRUE)+
    stat_summary(fun.y ="mean", geom="point", shape=21, colour="black", fill="#e4bf44", na.rm=TRUE)+
    geom_text(data=analyses, aes(y=y,label=label), size=3.5, colour="grey50",hjust=1, vjust=0.3)+
    #stat_summary(aes(y=data[,LLD]), fun.y ="mean", geom="point", colour="grey50", shape=124, size=4, na.rm=TRUE)+
    facet_grid(rows=vars(STYLE), scales="free", space="free", switch="y")+
    scale_y_log10(limits=c(1e-4,1e6), breaks=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6),
                  labels=c("1 ppb", "", "", "1 ppm", "", "", "", "1 wt%","",""),
                  name=NULL, expand=c(0,0))+
    labs(x=NULL,tag=element)+
    coord_flip()+
    theme_classic()+
    theme(legend.position = "none",
          strip.text.y = element_blank())
  return(p)
}

w<-12

for(e in elements){
  fn <- paste0("violin_",e,".pdf")
  ggsave(violin(joined, e), file=fn, w=w, h=w, units="cm")
}

