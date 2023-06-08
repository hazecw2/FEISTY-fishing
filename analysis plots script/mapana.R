source("fixed/FEISTY.R")
source("fixed/Plots.R")
source("fixed/plottools.R")
#source("FishVerticalLog.R")
source("FishBasic2Log.R")

my = basic2map(p=setupBasic2(nSizeGroups=6))

load("basic2input.RData") 
#input <- list(grid[["smzmort"]],grid[["lgzmort"]], gridbent[["detflux"]], gridtemp[["Tprof"]][,,1], gridtemp[["btemp"]], grid[["lon"]], grid[["lat"]],grid[["nlon"]], grid[["nlat"]], grid[["bottom"]]) #10
#[lon,lat]
long = round(input[[6]],digits = 1)
lati = round(input[[7]],digits = 1)
# north sea 57.009167, 2.679225
# continental slope 750m 62.034159, 0.561079 [lat,lon] 
# shelf region 50m 61.185479, -167.825016
# open ocean - oligotrophic -31.180370, -139.536578
# open ocean - mesotrophic 20.657983, -162.036574
# open ocean - eutrophic -23.216601, -100.513141
# negative lon: lon+360
list_idx = NULL
y=57.009167
x= 2.679225
x
lon = which.min(abs(long-x))
lat =  which.min(abs(lati-y))
long[[lon]]
lati[[lat]]
z = (lat-1)*100+lon
is.na(szprod_vec[[z]])
list_idx = c(list_idx,z)

testlist=NULL
for (i in 1:4){
testlist[[i]] = basic2plot(p = setupBasic2(szprod = szprod_vec[[list_idx[[i]]]],
              lzprod = lzprod_vec[[list_idx[[i]]]],
              bent = bent_vec[[list_idx[[i]]]],
              temps = temps_vec[[list_idx[[i]]]],
              tempb = tempb_vec[[list_idx[[i]]]],
              nSizeGroups = 6,
              etaMature = 0.25))
}

graph(testlist[[4]])

graph = function(x) {
my = x[[3]]
susF=x[[1]]
maxcatchF=x[[2]]
SSB = my[,9:11]
yieldm = my[,5:7]
bio = my[,12:14]
my <- mutate(my, fsp = X1/ftotal, fbp = X2/ftotal, fb = X3/ftotal)

#1. yield plot for each species 
defaultplot(mfcol=c(3,1))
for (j in 1:3) {
  semilogxpanel(xlim=my$ftotal, ylim=c(0,max(bio[[j]],yieldm[[j]],SSB[[j]])), #
                xlab="Fishing mortality (year$^{-1}$)",
                ylab="Yield (g/$m^2$/yr)")
  lines(my$ftotal,yieldm[[j]],lwd=3)
  lines(my$ftotal, SSB[[j]],col="red", lwd=3)
  lines(my$ftotal, bio[[j]],col="blue", lwd=3)
  abline(v=susF, col="blue") # Display the highest F value that ensures SSB of no species falls below 20%
  abline(v=maxcatchF, col="red")
}

#2. graph for total yield at each F combination
defaultplot(mfcol=c(2,1))
semilogxpanel(xlim=my$ftotal, ylim=c(my$yield), 
              xlab="ftotal",
              ylab="Total Yield (g/$m^2$/yr)")
lines(my$ftotal, my$yield)
abline(v=susF, col="blue") # Display the highest F value that ensures SSB of no species falls below 20%
abline(v=maxcatchF, col="red")

#3. F/Ftotal plot
semilogxpanel(xlim=my$ftotal, ylim=c(0,1), 
              xlab="ftotal",
              ylab="F/Ftotal")
lines(my$ftotal, my$fsp, lwd = 1)
lines(my$ftotal, my$fbp, lwd = 2)
lines(my$ftotal, my$fb, lwd = 3)
abline(v=susF, col="blue") # Display the highest F value that ensures SSB of no species falls below 20%
abline(v=maxcatchF, col="red")
}

save(testlist, file="testlist.Rdata")

