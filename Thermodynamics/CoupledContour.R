library(ggplot2)
library(openxlsx)
library(dplyr)
library(ggrepel)

source("Model.R")
source("coloursafe.R")

charges <- seq(0.5,7,0.1)
radii <- seq(0.1,1.25,0.01)

contour_grid <- data.frame()
for(charge in charges){
  for(radius in radii){
    row <- cbind(charge, radius, Di=Di(radius, charge, 800))
    contour_grid <- rbind(contour_grid, row)
  }
}
rm(row, charges, charge, radii, radius)

base <- ggplot(contour_grid, aes(x=radius, y=charge))+
  geom_contour(aes(z=Di, colour=stat(level)),
    breaks=c(1e-8, 1e-6, 1e-4, 1e-2, 1e-1, 5e-1))+
  scale_colour_gradientn(colours=coloursafe_grad[c(7:3)], 
    trans="log10")+
  annotate(geom="text", x = 0.69, y = 4, size=4,
           label=sprintf('\u2605'), colour=coloursafe[7])+
  scale_x_continuous(breaks=seq(0.2,1.2,0.1))+
  scale_y_continuous(breaks=c(1:6))+
  coord_cartesian(xlim=c(0.2,1.2), ylim=c(0.5,6.5))+
  theme_classic()+
  theme(legend.position = "none",
        strip.text.y = element_blank(),
        panel.background = element_rect(fill="grey95"),
        panel.grid.major.y = element_line(size=0.5, color="white"))+
  labs(x=NULL, y=NULL)

ions <- read.xlsx("ionic_radii.xlsx", sheet="trace_all")
ions <- filter(ions, spin=="H"|is.na(spin)==TRUE)
ions <- data.frame(cbind(ions, D=Di(ions$radius, ions$charge, 800)))
transmet <- ions[ions$facet_panel=="Transition Metals",]
rest <- ions[ions$facet_panel!="Transition Metals",]
rest$block <- factor(rest$block, levels=c("s-Block","p-Block","d-Block"))

cairo_pdf(filename = "LSM5.pdf", width=4.134, height = 5.905, family="Segoe UI Symbol")
p1 <- base +
  geom_point(data=transmet, aes(x=radius, y=charge), colour="grey50")+
  #geom_text_repel(data=transmet, aes(x=radius, y=charge, label=name), parse=TRUE)+
  facet_grid(rows=vars(row))
print(p1)
dev.off()

cairo_pdf(filename = "LSM6.pdf", width=4.134, height = 5.905, family="Segoe UI Symbol")
p2 <- base +
  geom_point(data=rest, aes(x=radius, y=charge), colour="grey50")+
  #geom_text_repel(data=rest, aes(x=radius, y=charge, label=name), parse=TRUE)+
  facet_grid(rows=vars(block))
print(p2)
dev.off()
