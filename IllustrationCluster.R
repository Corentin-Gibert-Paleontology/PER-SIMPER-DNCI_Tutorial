IllustrationCluster <- function(ListeFaunique, etik = FALSE, cexEtik = 0.5)
{
  library(maps)
  library(mapdata)
  
  Plot.EU1 <- data.frame()
  LAT <- strsplit(dimnames(ListeFaunique$data.clust)[[1]], "_")
  
  Lat.EU1 <- c()
  Long.EU1 <- c()
  for(i in 1:length(LAT))
  {
    Lat.EU1 <- c(Lat.EU1, LAT[[i]][1])
    Long.EU1 <- c(Long.EU1, LAT[[i]][2])
  }
  Lat.EU1 <- as.numeric(Lat.EU1)
  Long.EU1 <- as.numeric(Long.EU1)
  
  Color_C <- c()
  for(i in 1:length(ListeFaunique$data.clust$clust))
  {
    if(ListeFaunique$data.clust$clust[i] == 1){Color_C <- c(Color_C, "black")}
    if(ListeFaunique$data.clust$clust[i] == 2){Color_C <- c(Color_C, "brown1")}
    if(ListeFaunique$data.clust$clust[i] == 3){Color_C <- c(Color_C, "chartreuse3")}
    if(ListeFaunique$data.clust$clust[i] == 4){Color_C <- c(Color_C, "blue")}
    if(ListeFaunique$data.clust$clust[i] == 5){Color_C <- c(Color_C, "cyan")}
    if(ListeFaunique$data.clust$clust[i] == 6){Color_C <- c(Color_C, "pink")}
    if(ListeFaunique$data.clust$clust[i] == 7){Color_C <- c(Color_C, "gold")}
    if(ListeFaunique$data.clust$clust[i] == 8){Color_C <- c(Color_C, "aquamarine")}
    if(ListeFaunique$data.clust$clust[i] == 9){Color_C <- c(Color_C, "bisque2")}
    if(ListeFaunique$data.clust$clust[i] == 10){Color_C <- c(Color_C, "blue2")}
    if(ListeFaunique$data.clust$clust[i] == 11){Color_C <- c(Color_C, "cornflowerblue")}
    if(ListeFaunique$data.clust$clust[i] == 12){Color_C <- c(Color_C, "darkgoldenrod1")}
    if(ListeFaunique$data.clust$clust[i] == 13){Color_C <- c(Color_C, "brown4")}
    if(ListeFaunique$data.clust$clust[i] == 14){Color_C <- c(Color_C, "azure2")}
  }
  #print(Color_C)
  PlotEUROPE1 <- data.frame(Lat.EU1, Long.EU1, Color_C)
  PlotEUROPE1$Color_C <- as.character(PlotEUROPE1$Color_C)
  plot(PlotEUROPE1$Long.EU1, PlotEUROPE1$Lat.EU1, col = PlotEUROPE1$Color_C, ylim = c(), xlim = c(), cex = 0.5, pch = 16)
  if(etik == TRUE)
    {
    text(PlotEUROPE1$Long.EU1, PlotEUROPE1$Lat.EU1, labels=dimnames(ListeFaunique$data.clust)[[1]], cex = cexEtik, pos = 3)
  }
  
  x11()
  map("worldHires", col='gray85', fill=T, xlim=c(min(Long.EU1-10),max(Long.EU1+10)), ylim=c(min(Lat.EU1-10),max(Lat.EU1+10)))
  points(PlotEUROPE1$Long.EU1, PlotEUROPE1$Lat.EU1, col = PlotEUROPE1$Color_C, pch = 16, cex = 0.75)

}