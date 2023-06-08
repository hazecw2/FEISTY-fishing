source("fixed/FEISTY.R")
source("fixed/Plots.R")
source("fixed/plottools.R")
source("FishBasic2Log.R")
i=1
p = setupBasic2(szprod = systems_input[i,1],
                lzprod = systems_input[i,2],
                bent = systems_input[i,3],
                temps = systems_input[i,4],
                tempb = systems_input[i,5],
                nSizeGroups = 6,
                etaMature = 0.25)

testlist
calcRentFtotal2 <- function(ff,pf,df,p,
                            etaTime = 0.5,
                            a,
                            b = 0.4){
F = c(ff,pf,df)
p = setFishing2(p, F)
sim = simulate(p, tEnd = 200)
ixTime = which(sim$t>=(etaTime*sim$t[sim$nTime]))
sspecb = exp(log(colSums(sim$B[ixTime,]+1e-10)))
sspecy = p$mortF[p$ixFish] * sspecb
revenue = 0.073*(p$mc[p$ixFish])^0.41*sspecy ####
r = rep(0, p$nGroups)
for (i in 1:p$nGroups){
ix = p$ix[[i]]
ixn = p$ix[[i]]-max(p$ixR)
cost = a[[i]]*F[i]*(max(p$mUpper[ix])^b) ####
rent = revenue[ixn]-cost
r[[i]]= sum(rent)
}
r[[4]]=sum(r)
yy=calcYield(sim)
yy[[4]] = sum(yy[[1]],yy[[2]], yy[[3]]) 
r = c(r,yy,sum(revenue))
return(r)
}

a = 34
i = 1
ix = p$ix[[i]]
ixn = p$ix[[i]]-max(p$ixR)
cost = a*F[i]*(max(p$mUpper[ix])^b) ####
rent = revenue[ixn]-cost
plot(ixn,revenue[ixn],log = "y", type = "l")
lines(ixn, rep(cost,length(ixn)) ,col="red")
lines(ixn,rent,col="blue")
sum(rent)

avalue=c(20,4.3,0.5)
# c(34,4.3,10.2)

minlogf = 0.2
maxlogf = 20
nF = 10
pm <- t(rbind(Fproportion2(), colSums(Fproportion2())))
pm <- dplyr::filter(data.frame(pm), pm[,4] == 1)
logf <- round(logspace(log10(minlogf), log10(maxlogf), nF), digit = 2) 
my = NULL
for (j in 1:length(logf)) {
  matf = pm*logf[j]
  for (i in 1:nrow(matf)) {
    matf[i,5:13] <- calcRentFtotal2(matf[i,1], matf[i,2], matf[i,3], p, a=avalue)
  }
  y <- dplyr::filter(matf, matf[,8] == max(na.omit(matf[,8])))
  y = if(nrow(y) > 1) {
    y[1, ]
  } else {
    y
  }
  my <- rbind(my, y)
}
my
my <- dplyr::rename(my, rent=V8, ftotal=X4) 

graph(my)

graph = function(x) {
  my = x
  yieldm = my[,9:11]
  rentm = my[,5:7]
  my <- mutate(my, fsp = X1/ftotal, fbp = X2/ftotal, fb = X3/ftotal)
  maxrentF = my$ftotal[[min(which(my$rent>max(my$rent)*0.99))]]
  
  #1. yield plot for each species 
  defaultplot(mfcol=c(3,1))
  for (j in 1:3) {
    semilogxpanel(xlim=my$ftotal, ylim=c(0,max(yieldm[[j]],rentm[[j]])), #
                  xlab="Fishing mortality (year$^{-1})",
                  ylab="rent (Euro/m^2/yr)")
    lines(my$ftotal,rentm[[j]],lwd=3)
    abline(v=maxrentF, col="blue")
  }
  
  #2. graph for total yield at each F combination
  defaultplot(mfcol=c(2,1))
  semilogxpanel(xlim=my$ftotal, ylim=c(my$rent), 
                xlab="ftotal",
                ylab="Total rent (Euro/m^2/yr)")
  lines(my$ftotal, my$rent)
  abline(v=maxrentF, col="blue")
  
  #3. F/Ftotal plot
  semilogxpanel(xlim=my$ftotal, ylim=c(0,1), 
                xlab="ftotal",
                ylab="F/Ftotal")
  lines(my$ftotal, my$fsp, lwd = 1)
  lines(my$ftotal, my$fbp, lwd = 2)
  lines(my$ftotal, my$fb, lwd = 3)
  abline(v=maxrentF, col="blue")
}


