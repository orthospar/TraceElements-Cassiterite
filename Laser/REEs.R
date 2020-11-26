source("import.R")
source("coloursafe.R")
library(ggplot2)

#No Pm
rare_earths <- c("LOCALITY","SAMPLE", "STYLE",
                 #"Sc", "Y",
                 "La", "Ce", "Pr", "Nd",
                 "Sm", "Eu",
                 "Gd", "Tb",
                 "Dy",
                 "Ho", "Er", "Tm", "Yb", "Lu")

#need to use full_join to add in missing columns
REEs <- full_join(lilly[,colnames(lilly) %in% rare_earths], jason[,colnames(jason) %in% rare_earths]) %>%
  full_join(JCU[,colnames(JCU) %in% rare_earths])%>%
  full_join(bluetier[,colnames(bluetier) %in% rare_earths])%>%
  full_join(tony[,colnames(tony) %in% rare_earths])
REEs <- cbind(REEs, POINT=c(1:dim(REEs)[1]))
REEs <- select(REEs, "POINT", rare_earths)
REEs[5:dim(REEs)[2]] <- data.frame(lapply(REEs[5:dim(REEs)[2]], as.numeric))

#chondrite normalised
#Values from absract of Pourmand et al (2012)
CInorm<- mutate(REEs,
                #"Sc"=Sc/5.493, "Y"=Y/1.395,
                "La"=La/0.2469,"Ce"=Ce/0.6321,"Pr"=Pr/0.0959,"Nd"=Nd/0.4854,#skip Pm
                "Sm"=Sm/0.1556,"Eu"=Eu/0.0599,
                "Gd"=Gd/0.2093,"Tb"=Tb/0.0378,
                "Dy"=Dy/0.2577,
                "Ho"=Ho/0.0554,"Er"=Er/0.1667,"Tm"=Tm/0.0261,"Yb"=Yb/0.1694,"Lu"=Lu/0.0256)

#ioninc radii for REE
#3+, VI coordination,  (Shannon 1976)
radii <- c(#"Sc"=0.745, "Y"=0.900,
  "La"=1.032,"Ce"=1.010,"Pr"=0.990,"Nd"=0.983,#skip Pm
  "Sm"=0.958,"Eu"=0.947,
  "Gd"=0.938,"Tb"=0.923,
  "Dy"=0.912,
  "Ho"=0.901,"Er"=0.890,"Tm"=0.880,"Yb"=0.868,"Lu"=0.861)

tidy_REEs <- pivot_longer(REEs, cols=c(-LOCALITY, -SAMPLE, -STYLE, -POINT), names_to="REE", values_to="ppm")
tidy_REEs <- cbind(tidy_REEs, pivot_longer(CInorm, cols=c(-LOCALITY, -SAMPLE, -STYLE, -POINT), names_to="REE", values_to="ppmCI")[6])
tidy_REEs <- cbind(tidy_REEs, "radii"=rep(radii, length.out=dim(tidy_REEs)[1]))
tidy_REEs <- tidy_REEs[complete.cases(tidy_REEs),]
tidy_REEs[,6:8]<-data.frame(lapply(tidy_REEs[,6:8], as.numeric))
tidy_REEs[,1:5]<-data.frame(lapply(tidy_REEs[,1:5], as.factor))
#tidy_REEs <- tidy_REEs[tidy_REEs$STYLE!="STANDARD",]



data <- split.data.frame(tidy_REEs, tidy_REEs$STYLE)

p <- ggplot(data=NULL, aes(x=radii, y=ppmCI, colour=LOCALITY, group=LOCALITY))+
  scale_colour_manual(values = coloursafe)+
  guides(colour = guide_legend(nrow=3))+
  geom_jitter(data=tidy_REEs[,7:8], aes(x=radii, y=ppmCI), colour="grey100", group=1)+
  scale_x_reverse(name=NULL,
                  expand=c(0.01,0),
                  breaks=c(#0.745,0.900,
                    1.032,1.010,0.990,0.983,
                    0.97,
                    0.958,0.947,
                    0.938,0.923,
                    0.912,
                    0.901,0.890,0.880,0.868,0.861),
                  labels=c(#"Sc", "Y",
                    "La","Ce","Pr","Nd",
                    "Pm",
                    "Sm","Eu",
                    "Gd","Tb",
                    "Dy",
                    "Ho","Er","Tm","Yb","Lu"))+
  scale_y_log10(name=NULL,limits=c(NA,1E4),
                breaks=c(1E-3,1E-2,1E-1,1E0,1E1,1E2,1E3,1E4),
                labels=c("0.001","0.01","0.01","1","10", "100","1000","10,000"))+
  theme_classic()+
  theme(panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "transparent"),
        legend.justification =c(0,1),
        legend.position = c(0,1.05),
        legend.text=element_text(size=11))+
  labs(colour=NULL)+
  guides(alpha="none")

p1<- p+ geom_jitter(data=data$Pegmatite)+
  stat_summary(data=data$Pegmatite, size=1, fun.y=median, geom="line")
p2<- p+ geom_jitter(data=data$Greisen)+
  stat_summary(data=data$Greisen, size=1, fun.y=median, geom="line")
p3<- p+ geom_jitter(data=data$Skarn)+
  stat_summary(data=data$Skarn, size=1, fun.y=median, geom="line")

print(p1)
h<-10
ggsave(p1, filename="REE_pegmatites.pdf", w=h*sqrt(2), h=h, units="cm")
ggsave(p2, filename="REE_greisen.pdf", w=h*sqrt(2), h=h, units="cm")
ggsave(p3, filename="REE_skarn.pdf", w=h*sqrt(2), h=h, units="cm")

