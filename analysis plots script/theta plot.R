# plot theta
source("FEISTY.R")
library(plotly)
p = setupBasic()
p2 = setupBasic2()
plotlytheta(p$theta,p, p$nGroups)
plotlytheta(p2$theta,p2, p2$nSizeGroups)

plotlytheta = function(zmat, p, nGroups){
  lim = round(nGroups*2.66+4.5)
  fig <- plot_ly() %>%
    add_trace(x = c(1:lim), y = c(1:lim), z = zmat, type = "heatmap", colorscale = "rwg") %>% #colors = "Blues"
    add_trace(x = c(0, lim+0.5), y = c(length(p$ix[[1]])+4.5, length(p$ix[[1]])+4.5), type = "scatter", mode = "lines", line = list(color = "black"))%>%
    add_trace(x = c(0, lim+0.5), y = c(length(p$ix[[1]])+4.5+nGroups, length(p$ix[[1]])+4.5+nGroups), type = "scatter", mode = "lines", line = list(color = "black"))%>%
    add_trace(x = c(0, lim+0.5), y = c(4.5, 4.5), type = "scatter", mode = "lines", line = list(color = "black"))%>%
    add_trace(y = c(0, lim+0.5), x = c(length(p$ix[[1]])+4.5, length(p$ix[[1]])+4.5), type = "scatter", mode = "lines", line = list(color = "black"))%>%
    add_trace(y = c(0, lim+0.5), x = c(length(p$ix[[1]])+4.5+nGroups, length(p$ix[[1]])+4.5+nGroups), type = "scatter", mode = "lines", line = list(color = "black"))%>%
    add_trace(y = c(0, lim+0.5), x = c(4.5, 4.5), type = "scatter", mode = "lines", line = list(color = "black"))%>% 
    layout(xaxis = list(title = list(text="Prey"), range = c(0.5, lim+0.5)), 
           yaxis = list(title = list(text="Predator"), range = c(4.5, lim+0.5)),showlegend = FALSE)
  fig
}


