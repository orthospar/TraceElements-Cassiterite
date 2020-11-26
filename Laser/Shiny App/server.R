library(shiny)
library(plotly)
library(png)
library(grid)
library(ggplot2)
library(imager)

laser <- read.csv("BT_laser.csv")
laser <- laser[laser$SAMPLE=='BT-Main',]

#excluded below detection: Li, Si, Ca, Cu, Zn, As, Se, Rb, Sr, Tl, Pb, Bi, Th
#excluded too close to detection: Cr, Ge, Nd
#questionable: P, Kr
#second isotopes: Ga71, W183
elements <- c("RELDIST",
              "Al_ppm_m27","Sc_ppm_m45","Ti_ppm_m49","V_ppm_m51","Mn_ppm_m55",
              "Co_ppm_m59","Ni_ppm_m60","Ga_ppm_m69","Y_ppm_m89",
              "Zr_ppm_m90","Nb_ppm_m93","Mo_ppm_m97","In_ppm_m113","Sb_ppm_m121",
              "Cs_ppm_m133","Ba_ppm_m138","La_ppm_m139","Ce_ppm_m140","Lu_ppm_m175",
              "Hf_ppm_m179","Ta_ppm_m181","W_ppm_m182","U_ppm_m238")
simple_names <- c("Distance",
                  "Al","Sc","Ti","V","Mn",
                  "Co","Ni","Ga","Y",
                  "Zr","Nb","Mo","In","Sb",
                  "Cs","Ba","La","Ce","Lu",
                  "Hf","Ta","W","U")

#select only the columns listed in elements, replace all instances of "Below LOD" with 0, set to numeric
mydata <- laser[,colnames(laser) %in% elements]
mydata <- data.frame(lapply(mydata, function(x) gsub("Below LOD","0",x)),stringsAsFactors=FALSE)
mydata <- data.frame(lapply(mydata, as.numeric))
colnames(mydata) <- simple_names
mydata <- dplyr::mutate(mydata, NbTa = Nb+Ta)
rm(laser)

img0 <- load.image("Transect0.png")
img90 <- load.image("Transect90.png")

r <- 1-rowMeans(img0[,,,1])
g <- 1-rowMeans(img0[,,,2])
b <- 1-rowMeans(img0[,,,3])
sum <- r+b+g
av <- (r+b+g)/3
Distance <- c(4002+(1:length(r)*(2443/length(r))))
imgdata0 <- as.data.frame(cbind(Distance, r, g, b, sum, av))
rm(r, b, g, Distance, sum, av)

r <- 1-rowMeans(img90[,,,1])
g <- 1-rowMeans(img90[,,,2])
b <- 1-rowMeans(img90[,,,3])
sum <- r+b+g
av <- (r+b+g)/3
Distance <- c(4002+(1:length(r)*(2443/length(r))))
imgdata90 <- as.data.frame(cbind(Distance, r, g, b, sum, av))
imgdatadiff <- as.data.frame(cbind(Distance, imgdata90[,-1]-imgdata0[,-1]))
rm(r, b, g, Distance, sum, av)
                             
corr <- round(cor(mydata[-(111:118),2:24]),1)

function(input, output) {
  
  output$heat <- renderPlotly({
    
    plot_ly(x=simple_names[2:24], y=simple_names[2:24], z=corr, key = corr,type = "heatmap", source = "heatplot",
            colors=colorRamp(c("red","white","blue")),
            zauto = F, zmin = -1, zmax = 1 
            ) %>%
      layout(
        title = "Pearson Correlation Matrix",
        xaxis = list(title = ""), 
        yaxis = list(title = ""))
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(mydata[vars], c("x", "y"))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        layout(
          title = "Scatter Plot",
          xaxis = list(title = s[["x"]]), 
          yaxis = list(title = s[["y"]]), 
          showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })
  output$lineplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c("Distance", s[["x"]], s[["y"]])
      d <- setNames(mydata[vars], c("Distance", "y1", "y2"))
      plot_ly(d, x = ~Distance) %>%
        add_lines(y = ~y1, name = s[["x"]]) %>%
        add_lines(y = ~y2, name = s[["y"]], yaxis= "y2") %>%
        layout(
          title = "Transect Plot",
          xaxis = list(title = "Distance"), 
          yaxis = list(title = s[["x"]]),
          yaxis2 = list(title = s[["y"]], side="right", overlaying = "y"), 
          showlegend = TRUE)
    } else {
      plotly_empty()
    }
  })
  output$gglineplot <- renderPlot({
      s <- event_data("plotly_click", source = "heatplot")
      vars <- c("Distance", s[["x"]], s[["y"]])
      d <- setNames(mydata[vars], c("Distance", "y1", "y2"))
      d_noNA <- d[-(111:118),]
      h <- 0.1*max(d_noNA$y1)
      ggplot(data=d, aes(x=Distance+41))+
      annotation_raster(img0, 4002, 6445, -h,0)+
      annotation_raster(img90, 4002, 6445, -2*h,-h)+
      geom_path(aes(y=y1))+
      coord_cartesian(ylim=c(-2*h, max(d_noNA$y1)))+
      labs(y=s[["x"]])+
      theme_classic()
  })
  output$close_upA <- renderPlot({
      vars <- c("Distance", input$element)
      d <- setNames(mydata[vars], c("Distance", "y"))
      d_noNA <- d[-(111:118),]
      h <- 0.1*max(d_noNA$y)
      ggplot(data=d, aes(x=Distance+41))+
        annotation_raster(img0, 4002, 6445, -h,0)+
        geom_path(aes(y=y))+
        geom_path(data=imgdata0, aes(x=Distance, y=av*max(d_noNA$y)), colour="brown")+
        coord_cartesian(xlim=c(4002, 6445), ylim=c(-h, max(d_noNA$y)))+
        labs(y=input$element, x="Distance (um)")+
        theme_classic()
  })
  output$close_upB <- renderPlot({
    vars <- c("Distance", input$element)
    d <- setNames(mydata[vars], c("Distance", "y"))
    d_noNA <- d[-(111:118),]
    h <- 0.1*max(d_noNA$y)
    ggplot(data=d, aes(x=Distance+41))+
      annotation_raster(img90, 4002, 6445, -h,0)+
      geom_path(aes(y=y))+
      geom_path(data=imgdatadiff, aes(x=Distance, y=av*max(d_noNA$y)), colour="red")+
      coord_cartesian(xlim=c(4002, 6445), ylim=c(-h, max(d_noNA$y)))+
      labs(y=input$element, x="Distance (um)")+
      theme_classic()
  })
}
