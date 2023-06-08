# create the preliminary plot for Basic2
source("fixed/FEISTY.R")
source("fixed/plottools.R")
source("fixed/Plots.R")
source("FishBasic2Log.R")
load("systems_input.Rdata") #parameter value from different systems 
library(ggplot2)
library(patchwork)
testlist
i=1 # investigate shelf system Basic2 
p = setupBasic2(szprod = systems_input[i,1],
                lzprod = systems_input[i,2],
                bent = systems_input[i,3],
                temps = systems_input[i,4],
                tempb = systems_input[i,5],
                nSizeGroups = 6,
                etaMature = 0.25)
sim = simulate(p,tEnd=200)
# rp = basic2map(p=p) # find out the maxF for this grid
F = c(4.928,7.392,0.000)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)

Bio = matrix(nrow=length(sim$t), ncol=p$nGroups)
for (i in 1:p$nGroups){
  for (j in 1:length(sim$t)) {
    Bio[j,i] = sum(sim$B[j, p$ix[[i]]-max(p$ixR)])
  }
}
Biof = matrix(nrow=length(simf$t), ncol=p$nGroups)
for (i in 1:p$nGroups){
  for (j in 1:length(sim$t)) {
    Biof[j,i] = sum(simf$B[j, p$ix[[i]]-max(p$ixR)])
  }
}

# P1 Biomass/time -------
p1 <- ggplot() + scale_y_log10() + theme_bw() +
  geom_line(data = data.frame(x = sim$t, y = Bio[,1]), aes(x, y), color = "#87CEEB", size = 1)+
  geom_line(data = data.frame(x = sim$t, y = Bio[,2]), aes(x, y), color = "#4169E1", size = 1)+
  geom_line(data = data.frame(x = sim$t, y = Bio[,3]), aes(x, y), color = "#000080", size = 1)+
geom_line(data = data.frame(x = sim$t, y = Biof[,1]), aes(x, y), color = "#87CEEB", linetype = "dashed", size = 1)+
  geom_line(data = data.frame(x = sim$t, y = Biof[,2]), aes(x, y), color = "#4169E1", linetype = "dashed", size = 1)+
  geom_line(data = data.frame(x = sim$t, y = Biof[,3]), aes(x, y), color = "#000080", linetype = "dashed", size = 1)+
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(margin = margin(b = 20),size=18,hjust=0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 16), 
        axis.title.x = element_text(margin = margin(t = 15),size = 16),  # Adjust top margin of axis titles
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))+
  labs(x = "Time (yr)", y = "Biomass (gww  m-2)",
       title = "Biomass change over time (shelf)")

# P2 sspec with and without fishing --------
sspec = list()
for (i in 1:p$nGroups){
  sspec[[i]] = colMeans(sim$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}
p2 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = sspec[[1]]), aes(x, y, linetype = "Without fishing"), color = "#87CEEB", size = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = sspec[[2]]), aes(x, y), color = "#4169E1", size = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = sspec[[3]]), aes(x, y), color = "#000080", size = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = sspecf[[1]]), aes(x, y, linetype = "With fishing"), color = "#87CEEB", size = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = sspecf[[2]]), aes(x, y), color = "#4169E1", size = 1, linetype = "dotted")+
  geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = sspecf[[3]]), aes(x, y), color = "#000080", size = 1, linetype = "dotted")+
  theme(axis.text = element_text(size = 16),legend.position = c(0.8, 0.15), legend.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size=18,hjust=0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 16), 
        axis.title.x = element_text(margin = margin(t = 15),size = 16),  # Adjust top margin of axis titles
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))+
  labs(x = "Size (g)", y = "Biomass (gww  m-2)",
       title = "Biomass across size classes") +
  scale_linetype_manual(values = c("Without fishing" = "solid","With fishing" = "dotted"), name=NULL)

# P3 mortality -------
rates = calcDerivativesR(0, u=p$u0, p, bFullOutput = TRUE)
p3 <- ggplot() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = rates$mortpred[p$ix[[2]]]), aes(x, y), color = "#4169E1", linewidth = 1) +
  geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = rates$mortpred[p$ix[[3]]]), aes(x, y), color = "#000080", linewidth = 1) +
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = rates$mortpred[p$ix[[1]]]), aes(x, y), color = "#87CEEB", linewidth = 1) +
  geom_hline(aes(yintercept = 0.1), linetype = "dashed",color = "black") +
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(margin = margin(b = 20), size = 18, hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 16),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")) +
  labs(x = "Size (g)", y = "Mortality (1/year)", title = "Mortality across size classes") 

# if investigate feeding level, this is the critical feeding point: 
# geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = p$metabolism[p$ix[[3]]]/(p$epsAssim*p$Cmax[p$ix[[3]]])), aes(x, y), color = "black", size = 1)

# P4 mortality with fishing --------
rates = calcDerivativesR(0, u=pf$u0, pf, bFullOutput = TRUE)
p4 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = rates$mortpred[p$ix[[2]]]), aes(x, y, color = "P mp"), linewidth = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = rates$mortpred[p$ix[[3]]]), aes(x, y, color = "D mp"), linewidth = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = rates$mortpred[p$ix[[1]]]), aes(x, y, color = "F mp"), linewidth = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = pf$mortF[p$ix[[1]]]), aes(x, y, color = "F mf"), linewidth = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = pf$mortF[p$ix[[2]]]), aes(x, y, color = "P mf"), linewidth = 1)+
  geom_hline(aes(yintercept = 0.1, linetype = "mb"), color = "black") +
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(margin = margin(b = 20),size=18,hjust=0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 16), 
        axis.title.x = element_text(margin = margin(t = 15),size = 16),  
        legend.margin = margin(t = -8, r = 0, b = -8, l = 0, unit = "pt"),
        legend.position = c(0.85, 0.4), legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))+
  labs(x = "Size (g)", y = "Mortality (1/year)",
       title = "Mortality across size classes (with fishing)", color = "L")+
  scale_color_manual(values = c("F mp"="#87CEEB", "P mp"="#4169E1", "D mp"="#000080","F mf"="#FF9999", "P mf"="#FF3333"),breaks = fish_order, 
                     labels = fish_order, name="Mortality")+
  scale_linetype_manual(values = c("mb" = "dashed"), name=NULL)+
  guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))
fish_order=c("F mp", "P mp", "D mp","F mf", "P mf")

p1 + p2 + p3 + p4

##------

# repro
rates = calcDerivativesR(0, u=p$u0, p, bFullOutput = TRUE)
rate=rates$Repro
rate=ifelse(rate == 0, 1e-15, rate)
p5 <- ggplot() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = rate[p$ix[[2]]-4]), aes(x, y), color = "#4169E1", linewidth = 1) +
  geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = rate[p$ix[[3]]-4]), aes(x, y), color = "#000080", linewidth = 1) +
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = rate[p$ix[[1]]-4]), aes(x, y), color = "#87CEEB", linewidth = 1) +
  # geom_hline(aes(yintercept = 0.1), linetype = "dashed",color = "black") +
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(margin = margin(b = 20), size = 18, hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 15), size = 16),
        axis.title.x = element_text(margin = margin(t = 15), size = 16),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")) +
  labs(x = "Size (g)", y = "Reproduction (1/year)", title = "Reproduction across size classes") 

# abundance of each size class
p6 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  geom_line(data = data.frame(x = p$mc[p$ix[[1]]], y = sspec[[1]]/p$mc[p$ix[[1]]]), aes(x, y), color = "#87CEEB", size = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[2]]], y = sspec[[2]]/p$mc[p$ix[[2]]]), aes(x, y), color = "#4169E1", size = 1)+
  geom_line(data = data.frame(x = p$mc[p$ix[[3]]], y = sspec[[3]]/p$mc[p$ix[[3]]]), aes(x, y), color = "#000080", size = 1)+
  theme(axis.text = element_text(size = 16),legend.position = c(0.8, 0.15), legend.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size=18,hjust=0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 16), 
        axis.title.x = element_text(margin = margin(t = 15),size = 16),  # Adjust top margin of axis titles
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"))+
  labs(x = "Size (g)", y = "Biomass (gww  m-2)",
       title = "Abundance of size classes") 
p5 + p6
