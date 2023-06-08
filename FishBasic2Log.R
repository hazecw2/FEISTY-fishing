#
# Analysis of the fishing mortality - differ between groups
# output: F/Ftotal plot and yield plot for each species 
# log scale 

library(dplyr)

basic2map=function(minlogf = 0.2, maxlogf=20, nF = 10, p = setupBasic2()) { #szprod=45,lzprod=24,bent=26
  start_time = Sys.time()
  my <- MaxYieldLog2(minlogf, maxlogf, nF, p)
  
  sim = simulate(p, tEnd=200)
  SSBini = calcSSB(sim)
  SSB0 = 0.2*SSBini
  
  #sustainable F, max catch F
  sus = rep(0,p$nGroups)
  SSB = my[,9:11]
  for (i in seq_along(SSB)) {
    sus[[i]] = max(which(SSB[,i] > SSB0[i]))
  }
  susF=my$ftotal[[min(sus)]]
  ytotal = my$yield
  maxcatchF=my$ftotal[[min(which(ytotal>max(ytotal)*0.9))]]
  
  output=list(susF, maxcatchF, my)
  end_time = Sys.time()
  tt=end_time-start_time
  print(tt)
  return(output)
}

basic2plot <- function (minlogf = 0.2, maxlogf=12, nF = 10, p = setupBasic2()) { #szprod=45,lzprod=24,bent=26
  start_time = Sys.time()
  x <- basic2map(minlogf, maxlogf, nF, p)
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
                  xlab="Fishing mortality (year^-1)",
                  ylab="Yield (g/m^2/yr)")
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
                ylab="Total Yield (g/m^2/yr)")
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

  end_time = Sys.time()
  tt=end_time-start_time
  print(tt)
  return(x)
}

# create the F matrix (n=216)
Fproportion2 <- function(npF = 6){
  fp=seq(0,1,length.out=npF)
  a = rep(rep(fp,npF),npF)
  b = rep(rep(fp,each = npF),npF)
  c = rep(fp, each = npF^2)
  mat = rbind(a,b,c)
  rownames(mat)<-NULL
  return(mat)
}

MaxYieldLog2 <- function(minlogf, maxlogf, nF, p) {
  #start_time = Sys.time()
  pm <- t(rbind(Fproportion2(), colSums(Fproportion2())))
  pm <- dplyr::filter(data.frame(pm), pm[,4] == 1)
  logf <- round(logspace(log10(minlogf), log10(maxlogf), nF), digit = 2) 
  #fulllist = list()
  my = NULL
  for (j in 1:length(logf)) {
    matf = pm*logf[j]
    #response = rep(0, nrow(matf))
    for (i in 1:nrow(matf)) {
      matf[i,5:14] <- calcYieldFtotal2(matf[i,1], matf[i,2], matf[i,3], p)
    }
    #fulllist[[j]] = matf
    y <- dplyr::filter(matf, matf[,8] == max(na.omit(matf[,8])))
    y = if(nrow(y) > 1) {
      y[1, ]
    } else {
      y
    }
    my <- rbind(my, y)
  }
  my <- dplyr::rename(my, yield=V8, ftotal=X4) #123F, 4ftotal, 567yield, 8ytotal, 91011ssb, 12-14b
  return(my)
}

setFishing2 = function(p, F, etaF=0.05) {
  for (iGroup in 1:p$nGroups) {
    ix = p$ix[[iGroup]]
    mFishing = etaF*max(p$mUpper[ix]) # selectivity at 0.05 of maximum size 
    #?? the inflection point at the size with 50% retainment 
    psi = (1 + (p$mc[ix]/mFishing)^(-3))^(-1) # Standard trawl selectivity from Andersen (2019) Fig 5.2
    p$mortF[ix] = psi*F[iGroup]
  }
  return(p)
}

# Total yield for each F combination 
calcYieldFtotal2<- function(fsp, fbp, fb, p){ #pprod = 50, bprod=5,
  F = c(fsp, fbp, fb)
  p = setFishing2(p, F)
  sim = simulate(p, tEnd=200)
  yy = calcYield(sim)
  ss = calcSSB(sim)
  bb = calcB(sim)
  yy[[4]] = sum(yy[[1]],yy[[2]], yy[[3]]) 
  results = c(yy,ss,bb) #yy*4,ss*3, bb*3
  return(results)
}

#
# Return the yield of all function groups
#
calcYield = function(
    sim,          # The simulation object to analyse
    etaTime=0.5) {# The fraction of the time series to integrate (default the last half) 
  yieldMean = rep(data=0, p$nGroups)
  for (iGroup in 1:sim$p$nGroups) {
    ix = sim$p$ix[[iGroup]]
    ixTime = which(sim$t>=(etaTime*sim$t[sim$nTime]))
    #deltaM = sim$p$mUpper[ix]-sim$p$mLower[ix]
    B = matrix(0,length(ixTime),length(ix))
    for (i in 1:length(ixTime)) {
      B[i,] = sim$p$mortF[ix] * sim$B[ixTime[i],ix-max(sim$p$ixR)] 
    }
    #yieldMean[iGroup] = exp(mean(log(rowSums(B+1e-10 ))))
    yieldMean[iGroup] = mean(exp(log(rowSums(B+1e-10))))
  }
  return(yieldMean)
}
#
# Return the SSB of all function groups
#
calcSSB = function(
    sim,          # The simulation object to analyse
    etaTime=0.5) {# The fraction of the time series to integrate (default the last half) 
  
  SSBMean = rep(data=0, p$nGroups)

  for (iGroup in 1:sim$p$nGroups) {
    ix = sim$p$ix[[iGroup]]
    ixTime = which(sim$t>=(etaTime*sim$t[sim$nTime]))
    #deltaM = sim$p$mUpper[ix]-sim$p$mLower[ix]
    B = matrix(0,length(ixTime),length(ix))
    for (i in 1:length(ixTime)) {
      B[i,] = sim$p$psiMature[ix] * sim$B[ixTime[i],ix-max(sim$p$ixR)] 
    }
    #SSBMean[iGroup] = exp(mean(log(rowSums(B+1e-10 ))))
    SSBMean[iGroup] = mean(exp(log(rowSums(B+1e-10))))
  }
  
  return(SSBMean)
}

calcB = function(sim, etaTime=0.5) {
  Bio = rep(data=0, p$nGroups+1)
  for (iGroup in 1:sim$p$nGroups) {
    ix = sim$p$ix[[iGroup]]
    ixTime = which(sim$t>=(etaTime*sim$t[sim$nTime]))
    B = matrix(0,length(ixTime),length(ix))
    for (i in 1:length(ixTime)) {
      B[i,] = sim$B[ixTime[i],ix-max(sim$p$ixR)] 
    }
    Bio[iGroup] = mean(exp(log(rowSums(B+1e-10))))
  }
  return(Bio)
}
