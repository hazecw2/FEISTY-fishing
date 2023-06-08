# trophic cascade
# create the preliminary plot for Basic2
source("fixed/FEISTY.R")
source("FishBasic2Log.R")
load("systems_input.Rdata") #parameter value from different systems 
library(ggplot2)
library(patchwork)

i=1 # investigate shelf system Basic2 
p = setupBasic2(szprod = systems_input[i,1],
                lzprod = systems_input[i,2],
                bent = systems_input[i,3],
                temps = systems_input[i,4],
                tempb = systems_input[i,5],
                nSizeGroups = 6,
                etaMature = 0.25)
basic2plot(p=p, maxlogf=20)
sim = simulate(p,tEnd=200)

sspec = list()
for (i in 1:p$nGroups){
  sspec[[i]] = colMeans(sim$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}

# biomass plot-------
F = c(6,0,0)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}

p1 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(ylim = c(1e-4, 1e+2))+
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
       title = "F = 6,0,0") +
  scale_linetype_manual(values = c("Without fishing" = "solid","With fishing" = "dotted"), name=NULL)

F = c(0,6,0)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}
p2 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(ylim = c(1e-4, 1e+2))+
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
       title = "F = 0,6,0") +
  scale_linetype_manual(values = c("Without fishing" = "solid","With fishing" = "dotted"), name=NULL)

F = c(0,0,6)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}
p3 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(ylim = c(1e-4, 1e+2))+
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
       title = "F = 0,0,6") +
  scale_linetype_manual(values = c("Without fishing" = "solid","With fishing" = "dotted"), name=NULL)

F = c(6,6,6)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}
p4 <- ggplot() + scale_x_log10() + scale_y_log10() + theme_bw() +
  coord_cartesian(ylim = c(1e-4, 1e+2))+
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
       title = "F = 6,6,6") +
  scale_linetype_manual(values = c("Without fishing" = "solid","With fishing" = "dotted"), name=NULL)

p1 + p2 + p3 +p4


# difference barplot --------
h=2
F = c(h,0,0)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}

diff = (unlist(sspecf)-unlist(sspec))/unlist(sspec)*100
fmat <- data.frame(size = c(1:4,1:6,1:6),
                   fish = c(rep("F",4),rep("P",6),rep("D",6)),
                   value = diff)
fish_order = c("D","P","F")

p1 = ggplot(data = fmat, aes(y = value, x = size, fill = factor(fish, levels = fish_order))) + ylim(-200, 900)+
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size = 16,hjust = 0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 14), 
        axis.title.x = element_text(margin = margin(t = 15),size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size = 14))+ 
  scale_fill_manual(values = c("#000080", "#4169E1", "#87CEEB"), 
                    breaks = fish_order, 
                    labels = fish_order) +
  labs(y = "% difference", x = "Size", fill = "Fish") +
  ggtitle("F only")

F = c(0,h,0)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}

diff = (unlist(sspecf)-unlist(sspec))/unlist(sspec)*100
fmat <- data.frame(size = c(1:4,1:6,1:6),
                   fish = c(rep("F",4),rep("P",6),rep("D",6)),
                   value = diff)
fish_order = c("D","P","F")

p2 = ggplot(data = fmat, aes(y = value, x = size, fill = factor(fish, levels = fish_order))) + ylim(-200, 900)+
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size = 16,hjust = 0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 14), 
        axis.title.x = element_text(margin = margin(t = 15),size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size = 14))+ 
  scale_fill_manual(values = c("#000080", "#4169E1", "#87CEEB"), 
                    breaks = fish_order, 
                    labels = fish_order) +
  labs(y = "% difference", x = "Size", fill = "Fish") +
  ggtitle("P only")


F = c(0,0,h)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}

diff = (unlist(sspecf)-unlist(sspec))/unlist(sspec)*100
fmat <- data.frame(size = c(1:4,1:6,1:6),
                   fish = c(rep("F",4),rep("P",6),rep("D",6)),
                   value = diff)
fish_order = c("D","P","F")

p3 = ggplot(data = fmat, aes(y = value, x = size, fill = factor(fish, levels = fish_order))) + ylim(-200, 900)+
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size = 16,hjust = 0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 14), 
        axis.title.x = element_text(margin = margin(t = 15),size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size = 14))+ 
  scale_fill_manual(values = c("#000080", "#4169E1", "#87CEEB"), 
                    breaks = fish_order, 
                    labels = fish_order) +
  labs(y = "% difference", x = "Size", fill = "Fish") +
  ggtitle("D only")


F = c(h,h,h)
pf = setFishing2(p, F)
simf = simulate(pf, tEnd=200)
sspecf = list()
for (i in 1:p$nGroups){
  sspecf[[i]] = colMeans(simf$B[round(0.5*sim$nTime):sim$nTime,p$ix[[i]]-max(p$ixR)])
}

diff = (unlist(sspecf)-unlist(sspec))/unlist(sspec)*100
fmat <- data.frame(size = c(1:4,1:6,1:6),
                   fish = c(rep("F",4),rep("P",6),rep("D",6)),
                   value = diff)
fish_order = c("D","P","F")

p4 = ggplot(data = fmat, aes(y = value, x = size, fill = factor(fish, levels = fish_order))) + ylim(-200, 900)+
  geom_bar(position="stack", stat="identity") +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size = 16,hjust = 0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 14), 
        axis.title.x = element_text(margin = margin(t = 15),size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size = 14))+ 
  scale_fill_manual(values = c("#000080", "#4169E1", "#87CEEB"), 
                    breaks = fish_order, 
                    labels = fish_order) +
  labs(y = "% difference", x = "Size", fill = "Fish") +
  ggtitle("F+D+P")

p1+p2+p3+p4

