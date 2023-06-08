## source and library ------
source("fixed/FEISTY.R")
source("fixed/Plots.R")
source("fixed/plottools.R")
source("fixed/extnc.R")
source("FishBasic2Log.R")
library(dplyr)

# grid <- extractnc("29400.01.01","O_zoopmort") # or "O_detrexp" "O_temp"
#input <- list(grid[["smzmort"]],grid[["lgzmort"]], gridbent[["detflux"]], gridtemp[["Tprof"]][,,1], gridtemp[["btemp"]], grid[["lon"]], grid[["lat"]],grid[["nlon"]], grid[["nlat"]], grid[["bottom"]]) #[lon,lat]

## preparation -------
load("basic2input.RData")

totalyield = matrix(0,nrow=input[[8]], ncol=input[[9]])
spelyield = totalyield
bpelyield = totalyield
benyield = totalyield
BioFf = totalyield
BioPf = totalyield
BioDf = totalyield
spelF = totalyield
bpelF = totalyield
benF = totalyield
totalF = totalyield

# Get vectors of each input parameter
szprod_vec = as.vector(input[[1]])
lzprod_vec = as.vector(input[[2]])
bent_vec = as.vector(input[[3]])
temps_vec = as.vector(input[[4]])
tempb_vec = as.vector(input[[5]])
bottom_vec = as.vector(input[[10]])

# Get indices of NA values
na_indices <- which(is.na(szprod_vec) | is.na(lzprod_vec) | is.na(bent_vec) , arr.ind = TRUE) 

# Set NA values in output matrices
totalyield[na_indices] = NA
spelyield[na_indices] = NA
bpelyield[na_indices] = NA
benyield[na_indices] = NA
BioFf[na_indices] = NA
BioPf[na_indices] = NA
BioDf[na_indices] = NA
spelF[na_indices] = NA
bpelF[na_indices] = NA
benF[na_indices] = NA
totalF[na_indices] = NA

# Get indices of non-NA values
nonna_indices <- which(!is.na(szprod_vec) & !is.na(lzprod_vec) & !is.na(bent_vec) , arr.ind = TRUE) 

## Formal run of basic2map ----------
# use a loop or lapply to run the simulation and catch errors 
error_idx <- c()
my_list = list()
for (i in 1:length(nonna_indices)) { #length(nonna_indices)
  tryCatch({
    my_list[[i]] <- basic2map(p=setupBasic2(szprod = szprod_vec[nonna_indices[i]],
                  lzprod = lzprod_vec[nonna_indices[i]],
                  bent = bent_vec[nonna_indices[i]],
                  temps = temps_vec[nonna_indices[i]],
                  tempb = tempb_vec[nonna_indices[i]],
                  nSizeGroups = 6,
                  etaMature = 0.25))
  }, error = function(e) {
    # Catch the error and store the index of the problematic element
    error_idx <<- c(error_idx, i)
    my_list[[i]] = NA
  })
}

#simple run for biomass only (fast) ----------
p_list = list()
for (i in 1:length(nonna_indices)) {
    p_list[[i]] <- setupBasic2(szprod = szprod_vec[nonna_indices[i]],
                                lzprod = lzprod_vec[nonna_indices[i]],
                                bent = bent_vec[nonna_indices[i]],
                                temps = temps_vec[nonna_indices[i]],
                                tempb = tempb_vec[nonna_indices[i]],
                                nSizeGroups = 6,
                                etaMature = 0.25)
}

sim_list = list()
for (i in 1:length(nonna_indices)) {
  sim_list[[i]] <- simulate(p_list[[i]],tEnd=200)
}

bio_list = list()
for (i in 1:length(nonna_indices)) {
  bio_list[[i]] <- calcB(sim_list[[i]])
}

## Polish the data -------
# take a look at the susF, usually there are abnormal amount of highest F
x=NULL
for (i in 1:length(nonna_indices)){
  y = my_list[[i]][[1]]
  x = c(x,y)
}
table(x)
#take a look at the maxcatchF
z=NULL
for (i in 1:length(nonna_indices)){
  y = my_list[[i]][[2]]
  z = c(z,y)
}
table(z)

error_idx = which(sapply(my_list, function(x) all(is.na(x))))

# rerun the error run with the common susF and maxcatchF
# as single run could be successful even though the complete matrix run failed
error_idx2 <- c()
for (i in 1:length(error_idx)){
    tryCatch({
      j = error_idx[[i]]
      p = setupBasic2(szprod = szprod_vec[nonna_indices[j]],
                      lzprod = lzprod_vec[nonna_indices[j]],
                      bent = bent_vec[nonna_indices[j]],
                      temps = temps_vec[nonna_indices[j]],
                      tempb = tempb_vec[nonna_indices[j]],
                      nSizeGroups = 6,
                      etaMature = 0.25)
      susY = c(1.24, 0.31, 0, 1.55,calcYieldFtotal2(1.24, 0.31, 0, p))
      maxcatchY = c(8, 12 , 0, 20,calcYieldFtotal2(8, 16 , 0, p))
      my_list[[j]][[1]]=1.55
      my_list[[j]][[2]]=20
      my_list[[j]][[3]]=rbind(susY,maxcatchY)
    }, error = function(e) {
      # Catch the error and store the index of the problematic element
      error_idx2 <<- c(error_idx2, i)
      my_list[[i]] = NA
    })
}

# check those with high susF - replace with the most common susF if no / low yield
# so that susF won't be too high in low productivity region
error_idx4 = c()
susF = rep(0,length(nonna_indices))
for (i in 1:length(nonna_indices)){
  tryCatch({
  x <- my_list[[i]][[1]]
  x <- as.numeric(x)
  susF[[i]] <- x
  } , error = function(e) {
  susF[[i]] = NA
  error_idx4 <<- c(error_idx4, i)
  })
}
df = data.frame(susF)
idx = which(susF==20)
for (i in 1:length(idx)){
  x = idx[[i]]
  y = max(my_list[[x]][[3]][[8]]) #max total yield
  if (y<1e-05) {my_list[[x]][[1]]<-1.55}
}

# extracting yield and biomass
sus_list = list()
max_list = list()
error_idx3 <- c()
for (i in 1:length(nonna_indices)){
  tryCatch({
my = my_list[[i]][[3]]
susF = my_list[[i]][[1]]
maxcatchF = my_list[[i]][[2]]
fvec= my[,4]
sus_list[[i]]= as.vector(t(my[which(fvec==susF),]))# yield5-8, bio12-14
max_list[[i]]= as.vector(t(my[which(fvec==maxcatchF),])) }, error = function(e) {
  error_idx3 <<- c(error_idx3, i)
  sus_list[[i]] = rep(0,14) 
  max_list[[i]] = rep(0,14)
})
}

# special map: look at the intermediate F 
med_list = list()
error_idx3 <- c()
for (i in 1:length(nonna_indices)){
  tryCatch({
    my = my_list[[i]][[3]]
    maxcatchF = my_list[[i]][[2]]
    susF = my_list[[i]][[1]]
    fvec= my[,4]
    x = which(fvec==susF)+round((which(fvec==maxcatchF)-which(fvec==susF))/2,digits = 0)
    med_list[[i]]= as.vector(t(my[7,]))}, error = function(e) {
      error_idx3 <<- c(error_idx3, i)
      med_list[[i]] = NA
    })
}
error_idx2 = which(sapply(med_list, function(x) all(is.na(x))))
for (i in 1:length(error_idx2)){
  x = error_idx2[[i]]
  med_list[[x]] <- rep(0,14)
}

# show the difference between susF and maxF 

diff_list = list()
for (i in 1:6386){
  s=sus_list[[i]]
  m=max_list[[i]]
  diff_list[[i]] = (m-s)/s*100
}

## Combine outputs into matrices, changes depends on which map you want to produce ------
list = bio_list # /sus_list
totalyield[nonna_indices] = sapply(list, `[[`, 8)
spelyield[nonna_indices] = sapply(list, `[[`, 5)
bpelyield[nonna_indices] = sapply(list, `[[`, 6)
benyield[nonna_indices] = sapply(list, `[[`, 7)
BioFf[nonna_indices] = sapply(list, `[[`, 12)
BioPf[nonna_indices] = sapply(list, `[[`, 13)
BioDf[nonna_indices] = sapply(list, `[[`, 14)
spelF[nonna_indices] = sapply(list, `[[`, 1)
bpelF[nonna_indices] = sapply(list, `[[`, 2)
benF[nonna_indices] = sapply(list, `[[`, 3)
totalF[nonna_indices] = sapply(list, `[[`, 4)

# variables
bprod_vec=0.1*bent_vec
vec_list = list(szprod_vec,lzprod_vec,
                bprod_vec,bottom_vec,
                temps_vec,tempb_vec)
# choose target object: biomass/ yield/ fishing mortality
# output = list(BioFf, BioPf, BioDf) 
# output = list(spelyield, bpelyield, benyield, totalyield)
# output = list(spelF, bpelF, benF, totalF)
# output = list(BioFf, BioPf, BioDf,spelyield, bpelyield, benyield, totalyield) 

## Convert matrix to data frame ---------------------------
#output = vec_list
lon <- input[[6]]
lat <- input[[7]]
dataglob = list()
for (i in 1:length(output)){
  dataglob[[i]] <- data.frame(lon = rep(input[[6]], input[[9]]),
                              lat = rep(input[[7]], each = input[[8]]))
  dataglob[[i]][[3]] <- as.vector(output[[i]])
}
for (i in 2:length(output)){
  dataglob[[i]]  <- select(dataglob[[i]], -lon, -lat)
}

outp <- Reduce(function(x, y) {cbind(x, y)}, dataglob)
# colnames(outp) <- c("long", "lat", "BioFf", "BioPf", "BioDf") 
# colnames(outp) <- c("long", "lat","spelyield", "bpelyield", "benyield", "totalyield")
# colnames(outp) <- c("long", "lat", "spelF", "bpelF", "benF", "totalF")
# colnames(outp) <- c("long", "lat", "szprod", "lzprod", "bprod","bottom", "temps", "tempb")

# outp can be used directly in Map.R

## data overview ---------------------------
output <- data.frame(do.call("cbind", output))
output <- sapply(output, unlist)
min = rep(0,ncol(output))
max = rep(0,ncol(output))
for (i in 1:ncol(output)){
  min[[i]] = min(na.omit(as.vector(output[,i])))
  max[[i]] = max(na.omit(as.vector(output[,i])))
}

max(na.omit(as.vector(output[[3]])))
output = list(BioFf, BioPf, BioDf,spelyield, bpelyield, benyield, totalyield) 

# if there is unreasonably high value
for(i in 1:length(output)){
y = unlist(output[,i])
x = which(y>1e+3) 
for(j in 1:length(x)){
  y[[x[[j]]]]=1e+3
  }
output[,i]=y
}

# create df of yield at susF or maxF
total = rep(0,length(output))
for (i in 1:length(output)){
  total[[i]] = sum(na.omit(as.vector(output[[i]])))
}
total = rep(0,ncol(output))
for (i in 1:ncol(output)){
  total[[i]] = sum(na.omit(as.vector(output[,i])))
}

name = c("BioFf", "BioPf", "BioDf", "spelyield", "bpelyield", "benyield", "totalyield")
sus = round(total, digits=3)
max = round(total, digits=3)
df = data.frame(name, sus, max)

write.csv(t(df),file = "/Users/Haze/Desktop/imbrsea/semthesis/code/github_FEISTY-main/R/finalby.csv")

save(my_list, file = "my_b0.025_theta0.75.Rdata")
save(sus_list, file = "s_b0.05_theta0.5.Rdata")
save(max_list, file = "mc_b0.05_theta0.5.Rdata")


