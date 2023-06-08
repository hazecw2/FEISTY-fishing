  library(sp)
  library(rworldmap) # this pkg has waaaay better world shapefiles
  library(ggplot2)
  library(RColorBrewer)
  library(plyr)
  
  # outp <- read.csv("var.csv",header=T)

  coords2 <- outp
  coords2$long <- ifelse(coords2$long > 179.5,coords2$long-360,coords2$long)
  coordinates(coords2) <- c("long", "lat")
  proj4string(coords2) <- CRS("+init=epsg:4326")
  coords2 <- spTransform(coords2, CRS("+proj=wintri"))
  test <- coordinates(coords2)
  colnames(test) <- c("long_wintri","lat_wintri")
  outp <- cbind(outp,test)
  # outp$BioFf <- unlist(outp$BioFf)
  # outp$BioPf <- unlist(outp$BioPf)
  # outp$BioDf <- unlist(outp$BioDf)
  outp[] <- lapply(outp, as.numeric)
 
### Yield --------------
  
  ### create map
  world <- fortify(spTransform(getMap(), CRS("+proj=wintri")))
  sealand = c("#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c")
  sid = 0.5
  
  Map <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=NULL),
                               shape=15,size=sid)
  Map <- Map +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Map <- Map +  theme(plot.background=element_blank(),
                      panel.background=element_blank(),
                      axis.text.y   =element_blank(),
                      axis.text.x   =element_blank(),
                      axis.ticks    = element_blank(),
                      axis.title.y  =element_blank(),
                      axis.title.x  =element_blank(),
                      panel.border  = element_blank(),
                      legend.text   = element_blank(),
                      legend.title  = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))
  
  outp$spelyield <- ifelse(outp$spelyield < 10^-4, 0, outp$spelyield)
  outp$spelyield <- ifelse(outp$spelyield < 10^-2 & outp$spelyield > 0, 10^-2, outp$spelyield)
  Mapspelyield <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(spelyield)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("Forage - yield")+ geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$bpelyield <- ifelse(outp$bpelyield < 10^-4, 0, outp$bpelyield)
  outp$bpelyield <- ifelse(outp$bpelyield < 10^-2 & outp$bpelyield > 0, 10^-2, outp$bpelyield)
  Mapbpelyield <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(bpelyield)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("Pelagic - yield")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$benyield <- ifelse(outp$benyield < 10^-4, 0, outp$benyield)
  outp$benyield <- ifelse(outp$benyield < 10^-2 & outp$benyield > 0, 10^-2, outp$benyield)
  Mapbenyield <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(benyield)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("Demersal - yield")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$totalyield <- ifelse(outp$totalyield < 10^-4, 0, outp$totalyield)
  outp$totalyield <- ifelse(outp$totalyield < 10^-2 & outp$totalyield > 0, 10^-2, outp$totalyield)
  Maptotalyield <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(totalyield)),shape=15,size=sid,na.rm=T)+
    scale_colour_gradientn(colours= sealand,na.value = "white",limits=c(-2,2),
                           labels=c("-4 to -2","-1","0","1","2"),
                           name="log10(biomass) \n (g WW m-2)")+  ggtitle("totalyield")
  Maptotalyield <- Maptotalyield +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Maptotalyield <- Maptotalyield +  theme(plot.background=element_blank(),
                                panel.background=element_blank(),
                                axis.text.y   =element_blank(),
                                axis.text.x   =element_blank(),
                                axis.ticks    = element_blank(),
                                axis.title.y  =element_blank(),
                                axis.title.x  =element_blank(),
                                panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                legend.text   = element_text(size=11),
                                legend.title  = element_text(size=11),
                                plot.title = element_text(hjust = 0.5)) 
  
  library(cowplot)
  jpeg(file = "Figure-sub_fish_biomass.jpeg", width=5, height=7.5,units ='in', res = 500)
  
  biomassfig <- ggdraw() + 
    draw_plot(Mapspelyield,   0, .75,  .5, .25) +
    draw_plot(Mapbpelyield,  0, .5,  .5, .25) +
    draw_plot(Mapbenyield,   0, .25,  .5, .25) +
    draw_plot(Maptotalyield, 0,   0.01, .8,  .25) 

  print(biomassfig)
  dev.off()
  
  
### Basic2 ------------------
  world <- fortify(spTransform(getMap(), CRS("+proj=wintri")))
  sealand = c("#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c")
  sid = 0.5
  
  Map <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=NULL),
                               shape=15,size=sid)
  Map <- Map +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Map <- Map +  theme(plot.background=element_blank(),
                      panel.background=element_blank(),
                      axis.text.y   =element_blank(),
                      axis.text.x   =element_blank(),
                      axis.ticks    = element_blank(),
                      axis.title.y  =element_blank(),
                      axis.title.x  =element_blank(),
                      panel.border  = element_blank(),
                      legend.text   = element_blank(),
                      legend.title  = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))
  
  outp$BioFf <- ifelse(outp$BioFf < 10^-4, 0, outp$BioFf)
  outp$BioFf <- ifelse(outp$BioFf < 10^-2 & outp$BioFf > 0, 10^-2, outp$BioFf)
  MapBioFf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioFf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("Forage - Biomass")+ geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  
  outp$BioPf <- ifelse(outp$BioPf < 10^-4, 0, outp$BioPf)
  outp$BioPf <- ifelse(outp$BioPf < 10^-2 & outp$BioPf > 0, 10^-2, outp$BioPf)
  MapBioPf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioPf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("Pelagic - Biomass")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$BioDf <- ifelse(outp$BioDf < 10^-4, 0, outp$BioDf)
  outp$BioDf <- ifelse(outp$BioDf < 10^-2 & outp$BioDf > 0, 10^-2, outp$BioDf)
  MapBioDf <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioDf)),shape=15,size=sid,na.rm=T)+
    scale_colour_gradientn(colours= sealand,na.value = "white",limits=c(-2,2),
                           labels=c("-4 to -2","-1","0","1","2"),
                           name="log10(biomass) \n (g WW m-2)")+  ggtitle("Demersal - Biomass")
  MapBioDf <- MapBioDf +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  MapBioDf <- MapBioDf +  theme(plot.background=element_blank(),
                                panel.background=element_blank(),
                                axis.text.y   =element_blank(),
                                axis.text.x   =element_blank(),
                                axis.ticks    = element_blank(),
                                axis.title.y  =element_blank(),
                                axis.title.x  =element_blank(),
                                panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                legend.text   = element_text(size=11),
                                legend.title  = element_text(size=11),
                                plot.title = element_text(hjust = 0.5)) 
  
  library(cowplot)
  jpeg(file = "Figure-sub_fish_biomass.jpeg", width=7, height=6.5,units ='in', res = 500)
  
  biomassfig <- ggdraw() + 
    draw_plot(MapBioFf,   0, .6,  .4, .3) +
    draw_plot(MapBioPf,   0, .3,  .4, .3) +
    draw_plot(MapBioDf,   0,   0, .6,  .3)
  
  print(biomassfig)
  dev.off()
### Fishing mortality ----------
  ### create map
  world <- fortify(spTransform(getMap(), CRS("+proj=wintri")))
  sealand = c("white", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c")
  sid = 0.5
  # "spelF", "bpelF", "benF", "totalF"
  Map <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=NULL),
                               shape=15,size=sid)
  Map <- Map +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Map <- Map +  theme(plot.background=element_blank(),
                      panel.background=element_blank(),
                      axis.text.y   =element_blank(),
                      axis.text.x   =element_blank(),
                      axis.ticks    = element_blank(),
                      axis.title.y  =element_blank(),
                      axis.title.x  =element_blank(),
                      panel.border  = element_blank(),
                      legend.text   = element_blank(),
                      legend.title  = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))
  

  MapspelF <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=spelF),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(0,20), na.value = "white") + ggtitle("Forage - F")+ geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  MapbpelF <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=bpelF),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(0,20), na.value = "white") + ggtitle("Pelagic - F")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  MapbenF <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=benF),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(0,20), na.value = "white") + ggtitle("benthic - F")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  MaptotalF <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=totalF),shape=15,size=sid,na.rm=T)+scale_colour_gradientn(colours= sealand,na.value = "white",limits=c(0,20),
                           labels=c("0","5","10","15","20"),
                           name="Fishing mortality")+  ggtitle("totalF")
  MaptotalF <- MaptotalF +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  MaptotalF <- MaptotalF +  theme(plot.background=element_blank(),
                                          panel.background=element_blank(),
                                          axis.text.y   =element_blank(),
                                          axis.text.x   =element_blank(),
                                          axis.ticks    = element_blank(),
                                          axis.title.y  =element_blank(),
                                          axis.title.x  =element_blank(),
                                          panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                          legend.text   = element_text(size=11),
                                          legend.title  = element_text(size=11),
                                          plot.title = element_text(hjust = 0.5)) 
  
  library(cowplot)
  jpeg(file = "Figure-sub_fish_biomass.jpeg", width=5, height=7.5,units ='in', res = 500)
  
  biomassfig <- ggdraw() + 
    draw_plot(MapspelF,   0, .75,  .5, .25) +
    draw_plot(MapbpelF,  0, .5,  .5, .25) +
    draw_plot(MapbenF,   0, .25,  .5, .25) +
    draw_plot(MaptotalF, 0,   0, .8,  .25) 
  
  print(biomassfig)
  dev.off()
  
### variables --------
  world <- fortify(spTransform(getMap(), CRS("+proj=wintri")))
  sealand = c("#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c")
  depth <- c("#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0", "#e5f5e0", "#f7fcf5")
  temp <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000", "#7f0000", "#4d0000")
  
  sid = 0.5
  
  Map <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=NULL),
                               shape=15,size=sid)
  Map <- Map +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Map <- Map +  theme(plot.background=element_blank(),
                      panel.background=element_blank(),
                      axis.text.y   =element_blank(),
                      axis.text.x   =element_blank(),
                      axis.ticks    = element_blank(),
                      axis.title.y  =element_blank(),
                      axis.title.x  =element_blank(),
                      panel.border  = element_blank(),
                      legend.text   = element_blank(),
                      legend.title  = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))
  
  outp$szprod <- ifelse(outp$szprod < 10^-4, 0, outp$szprod)
  outp$szprod <- ifelse(outp$szprod < 10^-2 & outp$szprod > 0, 10^-2, outp$szprod)
  Mapszprod <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(szprod)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-3,3), na.value = "white") + ggtitle("szprod")+ geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$lzprod <- ifelse(outp$lzprod < 10^-4, 0, outp$lzprod)
  outp$lzprod <- ifelse(outp$lzprod < 10^-2 & outp$lzprod > 0, 10^-2, outp$lzprod)
  Maplzprod <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(lzprod)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,3), na.value = "white", labels=c("-4 to -2","-1","0","1","2","3"), name="log10(biomass) \n (g WW m-2)") + ggtitle("lzprod")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Maplzprod <- Maplzprod +  theme(plot.background=element_blank(),
                                  panel.background=element_blank(),
                                  axis.text.y   =element_blank(),
                                  axis.text.x   =element_blank(),
                                  axis.ticks    = element_blank(),
                                  axis.title.y  =element_blank(),
                                  axis.title.x  =element_blank(),
                                  panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                  legend.text   = element_text(size=11),
                                  legend.title  = element_text(size=11),
                                  plot.title = element_text(hjust = 0.5))
  
  outp$bprod <- ifelse(outp$bprod < 10^-4, 0, outp$bprod)
  outp$bprod <- ifelse(outp$bprod < 10^-2 & outp$bprod > 0, 10^-2, outp$bprod)
  Mapbprod <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(bprod)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand, limits=c(-2,3),na.value = "white") + ggtitle("bprod")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  
  Mapbottom <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=bottom),shape=15,size=sid)+
    scale_colour_gradientn (colours= depth, na.value = "white", name="meter") + ggtitle("bottom")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Mapbottom <- Mapbottom +  theme(plot.background=element_blank(),
                                  panel.background=element_blank(),
                                  axis.text.y   =element_blank(),
                                  axis.text.x   =element_blank(),
                                  axis.ticks    = element_blank(),
                                  axis.title.y  =element_blank(),
                                  axis.title.x  =element_blank(),
                                  panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                  legend.text   = element_text(size=11),
                                  legend.title  = element_text(size=11),
                                  plot.title = element_text(hjust = 0.5))
  
  
  Maptemps <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=temps),shape=15,size=sid)+
    scale_colour_gradientn (colours= temp, na.value = "white") + ggtitle("temps")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  Maptempb <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=tempb),shape=15,size=sid)+
    scale_colour_gradientn (colours= temp,na.value = "white", name="degree celsius") + ggtitle("tempb")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Maptempb <- Maptempb +  theme(plot.background=element_blank(),
                                panel.background=element_blank(),
                                axis.text.y   =element_blank(),
                                axis.text.x   =element_blank(),
                                axis.ticks    = element_blank(),
                                axis.title.y  =element_blank(),
                                axis.title.x  =element_blank(),
                                panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                legend.text   = element_text(size=11),
                                legend.title  = element_text(size=11),
                                plot.title = element_text(hjust = 0.5))
  
  
  
  library(cowplot)
  jpeg(file = "Figure-sub_fish_biomass.jpeg", width=7, height=6.5,units ='in', res = 500)
  
  biomassfig <- ggdraw() + 
    draw_plot(Mapszprod,   0, .6,  .4, .3) +
    draw_plot(Maplzprod,  .4, .6,  .57,  .3) +
    draw_plot(Mapbprod, 0, .3,  .4, .3) +
    draw_plot(Mapbottom,  .4, .3, .54,  .3)+
    draw_plot(Maptemps, 0, 0,  .4, .3) +
    draw_plot(Maptempb,   .4,   0, .57,  .3)
  
  print(biomassfig)
  dev.off()
  
### Vertical --------
  world <- fortify(spTransform(getMap(), CRS("+proj=wintri")))
  sealand = c("#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#810f7c")
  sid = 0.5
  
  Map <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=NULL),
                               shape=15,size=sid)
  Map <- Map +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Map <- Map +  theme(plot.background=element_blank(),
                      panel.background=element_blank(),
                      axis.text.y   =element_blank(),
                      axis.text.x   =element_blank(),
                      axis.ticks    = element_blank(),
                      axis.title.y  =element_blank(),
                      axis.title.x  =element_blank(),
                      panel.border  = element_blank(),
                      legend.text   = element_blank(),
                      legend.title  = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))
  
  outp$BioFf <- ifelse(outp$BioFf < 10^-4, 0, outp$BioFf)
  outp$BioFf <- ifelse(outp$BioFf < 10^-2 & outp$BioFf > 0, 10^-2, outp$BioFf)
  MapBioFf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioFf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("epipelagic fish")+ geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$BioMf <- ifelse(outp$BioMf < 10^-4, 0, outp$BioMf)
  outp$BioMf <- ifelse(outp$BioMf < 10^-2 & outp$BioMf > 0, 10^-2, outp$BioMf)
  MapBioMf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioMf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("mesopelagic fish")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$BioPf <- ifelse(outp$BioPf < 10^-4, 0, outp$BioPf)
  outp$BioPf <- ifelse(outp$BioPf < 10^-2 & outp$BioPf > 0, 10^-2, outp$BioPf)
  MapBioPf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioPf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("large pelagic")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$BioBaP <- ifelse(outp$BioBaP < 10^-4, 0, outp$BioBaP)
  outp$BioBaP <- ifelse(outp$BioBaP < 10^-2 & outp$BioBaP > 0, 10^-2, outp$BioBaP)
  MapBioBaP <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioBaP)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,2), na.value = "white") + ggtitle("mid-water predator")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  outp$BioDf <- ifelse(outp$BioDf < 10^-4, 0, outp$BioDf)
  outp$BioDf <- ifelse(outp$BioDf < 10^-2 & outp$BioDf > 0, 10^-2, outp$BioDf)
  MapBioDf <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioDf)),shape=15,size=sid,na.rm=T)+
    scale_colour_gradientn(colours= sealand,na.value = "white",limits=c(-2,2),
                           labels=c("-4 to -2","-1","0","1","2"),
                           name="log10(biomass) \n (g WW m-2)")+  ggtitle("demersal fish")
  MapBioDf <- MapBioDf +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  MapBioDf <- MapBioDf +  theme(plot.background=element_blank(),
                                panel.background=element_blank(),
                                axis.text.y   =element_blank(),
                                axis.text.x   =element_blank(),
                                axis.ticks    = element_blank(),
                                axis.title.y  =element_blank(),
                                axis.title.x  =element_blank(),
                                panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                legend.text   = element_text(size=11),
                                legend.title  = element_text(size=11),
                                plot.title = element_text(hjust = 0.5)) 
  
  library(cowplot)
  jpeg(file = "Figure-sub_fish_biomass.jpeg", width=7, height=6.5,units ='in', res = 500)
  
  biomassfig <- ggdraw() + 
    draw_plot(MapBioFf,   0, .6,  .4, .3) +
    draw_plot(MapBioMf,  .4, .6,  .4, .3) +
    draw_plot(MapBioPf,   0, .3,  .4, .3) +
    draw_plot(MapBioBaP, .4, .3,  .4, .3) +
    draw_plot(MapBioDf,   0,   0, .57,  .3)
  
  print(biomassfig)
  dev.off()
### difference -------
  world <- fortify(spTransform(getMap(), CRS("+proj=wintri")))
  sealand = c(colors <- c("#0000FF", "#77CCFF", "#FFFFFF", "#FFCCCC","#FF7777", "#FF0000"))
  sid = 0.55
  
  Map <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=NULL),
                               shape=15,size=sid)
  Map <- Map +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  Map <- Map +  theme(plot.background=element_blank(),
                      panel.background=element_blank(),
                      axis.text.y   =element_blank(),
                      axis.text.x   =element_blank(),
                      axis.ticks    = element_blank(),
                      axis.title.y  =element_blank(),
                      axis.title.x  =element_blank(),
                      panel.border  = element_blank(),
                      legend.text   = element_blank(),
                      legend.title  = element_blank(),
                      legend.position = "none",
                      plot.title = element_text(hjust = 0.5))
  

  MapBioFf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioFf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,3), na.value = "white") + ggtitle("F - Biomass difference")+ geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  
  MapBioPf <- Map + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioPf)),shape=15,size=sid)+
    scale_colour_gradientn (colours= sealand,limits=c(-2,3), na.value = "white") + ggtitle("P - Biomass difference")+
    geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")

  MapBioDf <- ggplot() + geom_point(data=outp, aes(x=long_wintri, y=lat_wintri, colour=log10(BioDf)),shape=15,size=sid,na.rm=T)+scale_colour_gradientn(colours= sealand,na.value = "white",limits=c(-2,3),
                           labels=c("-2","-1","0","1","2","3"),
                           name="log10(%)")+  ggtitle("D - Biomass difference")
  MapBioDf <- MapBioDf +  geom_polygon(data = world, aes(x = long, y = lat, group = group),color="black",fill="black")
  MapBioDf <- MapBioDf +  theme(plot.background=element_blank(),
                                panel.background=element_blank(),
                                axis.text.y   =element_blank(),
                                axis.text.x   =element_blank(),
                                axis.ticks    = element_blank(),
                                axis.title.y  =element_blank(),
                                axis.title.x  =element_blank(),
                                panel.border  = element_rect(colour = "white", size=.5,fill=NA),
                                legend.text   = element_text(size=11),
                                legend.title  = element_text(size=11),
                                plot.title = element_text(hjust = 0.5)) 
  
  library(cowplot)
  jpeg(file = "Figure-sub_fish_biomass.jpeg", width=7, height=6.5,units ='in', res = 500)
  
  biomassfig <- ggdraw() + 
    draw_plot(MapBioFf,   0, .6,  .4, .3) +
    draw_plot(MapBioPf,   0, .3,  .4, .3) +
    draw_plot(MapBioDf,   0,   0, .5,  .3)
  
  print(biomassfig)
  dev.off()