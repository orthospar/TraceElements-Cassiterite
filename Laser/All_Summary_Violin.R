library(ggplot2)

source("import.R")
source("coloursafe.R")

metadata <- c("SAMPLE", "STYLE","IS")
elements <- c("Ca","Se","Rb","Cs","Ba","Tl","Bi",
              "Li","Al",
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

data <- pivot_longer(joined[,c(metadata,elements)], cols = elements, names_to = "element", values_to = "ppm")
data <- cbind(data, pivot_longer(joined[,detlimit], cols = detlimit, names_to = "element", values_to= "detlimit")[,2])




data$STYLE <- factor(data$STYLE, levels=c("Pegmatite", "Greisen", "Vein", "Skarn"))

panel1 <- c("Ta", "Fe", "Nb", "Ti", "Zr", "Hf", "Mn", "W")
panel2 <- c("Al", "Sc", "Sb", "Ga",  "V", "In", "Mo", "Y",  "U", "Th", "Pb")
panel3 <- c("La", "Ce", "Pr", "Nd", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu")
panel4 <- c("Li","Rb","Cs","Ca","Ba",
            "Cr","Co","Ni","Cu","Zn",       
            "Ge","As","Se","Tl","Bi")

n_obs <- function(x) {return(c(y=6, label=sum(is.na(x)==FALSE)))}

data$element <- factor(data$element, levels=c(panel1[length(panel1):1],panel2,panel3,panel4))
p1 <- ggplot(data[data$element %in% panel1,], aes(x=element, y=ppm))+
  scale_color_manual(values=coloursafe)+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+4,ymax=1e+6,fill="#cccccc")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+2,ymax=1e+4,fill="#d6d6d6")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+0,ymax=1e+2,fill="#e0e0e0")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-3,ymax=1e-0,fill="#ebebeb")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-4,ymax=1e-3,fill="#f2f2f2")+
  geom_boxplot(aes(y=detlimit), outlier.shape = 4, outlier.size = 1, na.rm=TRUE, colour="#ff7d7d")+
  geom_jitter(aes(fill=STYLE, colour=STYLE), width=0.2, alpha=0.2, na.rm=TRUE)+
  geom_violin(fill=NA, colour="black", scale="width", width=0.5, na.rm=TRUE)+
  stat_summary(geom="point", fun.y="mean", shape=21, size=2.5, colour="black", fill="#e4bf44", na.rm = TRUE)+
  stat_summary(fun.data ="n_obs", geom="text", size=3, colour="black", hjust=1, position=position_nudge(y=-0.015))+
  facet_grid(cols=vars(STYLE))+
  scale_y_log10(limits=c(1e-4,1e6), breaks=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6),
                labels=c("", "-2", "", "0", "", "2", "", "4","","6"),
                name=NULL, expand=c(0,0.1))+
  labs(x=NULL)+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none",
        strip.text.x = element_blank())

data$element <- factor(data$element, levels=c(panel2[length(panel2):1],panel1,panel3,panel4))
p2 <- ggplot(data[data$element %in% panel2,], aes(x=element, y=ppm))+
  scale_color_manual(values=coloursafe)+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+4,ymax=1e+6,fill="#cccccc")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+2,ymax=1e+4,fill="#d6d6d6")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+0,ymax=1e+2,fill="#e0e0e0")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-3,ymax=1e-0,fill="#ebebeb")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-4,ymax=1e-3,fill="#f2f2f2")+
  geom_boxplot(aes(y=detlimit), outlier.shape = 4, outlier.size = 1, na.rm=TRUE, colour="#ff7d7d")+
  geom_jitter(aes(fill=STYLE, colour=STYLE), width=0.2, alpha=0.2, na.rm=TRUE)+
  geom_violin(fill=NA, colour="black", scale="width", width=0.5, na.rm=TRUE)+
  stat_summary(geom="point", fun.y="mean", shape=21, size=2.5, colour="black", fill="#e4bf44", na.rm = TRUE)+
  stat_summary(fun.data ="n_obs", geom="text", size=3, colour="black", hjust=1, position=position_nudge(y=-0.015))+
  facet_grid(cols=vars(STYLE))+
  scale_y_log10(limits=c(1e-4,1e6), breaks=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6),
                labels=c("", "-2", "", "0", "", "2", "", "4","","6"),
                name=NULL, expand=c(0,0.1))+
  labs(x=NULL)+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none",
        strip.text.x = element_blank())
data$element <- factor(data$element, levels=c(panel3[length(panel3):1],panel2,panel1,panel4))

p3 <- ggplot(data[data$element %in% panel3,], aes(x=element, y=ppm))+
  scale_color_manual(values=coloursafe)+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+4,ymax=1e+6,fill="#cccccc")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+2,ymax=1e+4,fill="#d6d6d6")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+0,ymax=1e+2,fill="#e0e0e0")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-3,ymax=1e-0,fill="#ebebeb")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-4,ymax=1e-3,fill="#f2f2f2")+
  geom_boxplot(aes(y=detlimit), outlier.shape = 4, outlier.size = 1, na.rm=TRUE, colour="#ff7d7d")+
  geom_jitter(aes(fill=STYLE, colour=STYLE), width=0.2, alpha=0.2, na.rm=TRUE)+
  geom_violin(fill=NA, colour="black", scale="width", width=0.5, na.rm=TRUE)+
  stat_summary(geom="point", fun.y="mean", shape=21, size=2.5, colour="black", fill="#e4bf44", na.rm = TRUE)+
  stat_summary(fun.data ="n_obs", geom="text", size=3, colour="black", hjust=1, position=position_nudge(y=-0.015))+
  facet_grid(cols=vars(STYLE))+
  scale_y_log10(limits=c(1e-4,1e6), breaks=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6),
                labels=c("", "-2", "", "0", "", "2", "", "4","","6"),
                name=NULL, expand=c(0,0.1))+
  labs(x=NULL)+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none",
        strip.text.x = element_blank())

data$element <- factor(data$element, levels=c(panel4[length(panel4):1],panel2,panel3,panel1))
p4 <- ggplot(data[data$element %in% panel4,], aes(x=element, y=ppm))+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+4,ymax=1e+6,fill="#cccccc")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+2,ymax=1e+4,fill="#d6d6d6")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e+0,ymax=1e+2,fill="#e0e0e0")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-3,ymax=1e-0,fill="#ebebeb")+
  annotate("rect", xmin=-Inf,xmax=Inf,ymin=1e-4,ymax=1e-3,fill="#f2f2f2")+
  geom_boxplot(aes(y=detlimit), outlier.shape = 4, outlier.size = 1, na.rm=TRUE, colour="#ff7d7d")+
  geom_jitter(aes(fill=STYLE, colour=STYLE), width=0.2, alpha=0.2, na.rm=TRUE)+
  geom_violin(fill=NA, colour="black", scale="width", width=0.5, na.rm=TRUE)+
  stat_summary(geom="point", fun.y="mean", shape=21, size=2.5, colour="black", fill="#e4bf44", na.rm = TRUE)+
  stat_summary(fun.data ="n_obs", geom="text", size=3, colour="black", hjust=1, position=position_nudge(y=-0.015))+
  facet_grid(cols=vars(STYLE))+
  scale_y_log10(limits=c(1e-4,1e6), breaks=c(1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3,1e4,1e5,1e6),
                labels=c("", "-2", "", "0", "", "2", "", "4","","6"),
                name=NULL, expand=c(0,0.1))+
  labs(x=NULL)+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none",
        strip.text.x = element_blank())

ggsave(p1, filename="Violin_1.pdf", width=19, height=0.8+length(panel1), units="cm")
ggsave(p2, filename="Violin_2.pdf", width=19, height=0.8+length(panel2), units="cm")
ggsave(p3, filename="Violin_3.pdf", width=19, height=0.8+length(panel3), units="cm")
ggsave(p4, filename="Violin_4.pdf", width=19, height=0.8+length(panel4), units="cm")

