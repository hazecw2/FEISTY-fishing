## create the barplot of biomass and yield for each fish groups and systems
source("fixed/FEISTY.R")
source("FishBasic2Log.R")
load("systems_input.Rdata") #parameter value from different systems 
library(ggplot2)

#output matrix from different fishing mortality combination
testlist=NULL
for (i in 1:4){
  testlist[[i]] = basic2map(maxlogf=20, nF = 20, 
                            p = setupBasic2(szprod = systems_input[i,1],
                                           lzprod = systems_input[i,2],
                                           bent = systems_input[i,3],
                                           temps = systems_input[i,4],
                                           tempb = systems_input[i,5],
                                           nSizeGroups = 6,
                                           etaMature = 0.25))
} #fishing mortality 1-4, yield 5-8, SSB 9-11, bio 12-14

#obtain the biomass and yield at maxcatchF and sustainableF
sus_df = NULL
max_df = NULL
for (i in 1:4){
  my = testlist[[i]][[3]]
  susF = testlist[[i]][[1]]
  maxcatchF = testlist[[i]][[2]]
  fvec= my[,4]
  sus_df= rbind(sus_df,as.vector(t(my[which(fvec==susF),])))
  max_df= rbind(max_df,as.vector(t(my[which(fvec==maxcatchF),])))
} 

# barplot for sustainable F
susy <- data.frame(systems = rep(c("shelf","slope","eutro","meso"), ncol(sus_df[,5:7])),
                   fish = rep(c("F","P","D"), each = nrow(sus_df[,5:7])),
                   value = as.vector(sus_df[,5:7]))

susb <- data.frame(systems = rep(c("shelf","slope","eutro","meso"), ncol(sus_df[,12:14])),
                   fish = rep(c("F","P","D"), each = nrow(sus_df[,12:14])),
                   value = as.vector(sus_df[,12:14]))
by = rep(c("Y","B"), each=nrow(susy))
susmat = cbind(rbind(susy,susb),by)

fish_order = c("D","P","F")
p11 <- ggplot(data = susmat, aes(y = value, x = by, fill = factor(fish, levels = fish_order))) + ylim(0, 120)+
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size = 16,hjust = 0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 14), 
        axis.title.x = element_text(margin = margin(t = 15),size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size = 14))+ 
  facet_grid(. ~ reorder(systems, -value, sum)) +
  scale_fill_manual(values = c("#000080", "#4169E1", "#87CEEB"), 
                    breaks = fish_order, 
                    labels = fish_order) +
  labs(y = "Value (g m-2)", x = "Biomass/ Yield", fill = "Fish") +
  ggtitle("Biomass and Yield at susF")

# barplot for maximum F
maxy <- data.frame(systems = rep(c("shelf","slope","eutro","meso"), ncol(max_df[,5:7])),
                   fish = rep(c("F","P","D"), each = nrow(max_df[,5:7])),
                   value = as.vector(max_df[,5:7]))

maxb <- data.frame(systems = rep(c("shelf","slope","eutro","meso"), ncol(max_df[,12:14])),
                   fish = rep(c("F","P","D"), each = nrow(max_df[,12:14])),
                   value = as.vector(max_df[,12:14]))
by = rep(c("Y","B"), each=nrow(maxy))
maxmat = cbind(rbind(maxy,maxb),by)

p12 <- ggplot(data = maxmat, aes(y = value, x = by, fill = factor(fish, levels = fish_order))) + ylim(0, 120)+
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() + 
  theme(axis.text = element_text(size = 14),
        plot.title = element_text(margin = margin(b = 20),size = 16,hjust = 0.5),  # Adjust bottom margin of plot title
        axis.title.y = element_text(margin = margin(r = 15),size = 14), 
        axis.title.x = element_text(margin = margin(t = 15),size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size = 14))+ 
  facet_grid(. ~ reorder(systems, -value, sum)) +
  scale_fill_manual(values = c("#000080", "#4169E1", "#87CEEB"), 
                    breaks = fish_order, 
                    labels = fish_order) +
  labs(y = "Value (g m-2)", x = "Biomass/ Yield", fill = "Fish") +
  ggtitle("Biomass and Yield at maxF")

p11 +p12


