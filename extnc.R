

library('ncdf4')
#uf=extractnc ("02011.01.01","O_zoopmort")
# sFile : "XXXXX,XX,XX"
# var: variable e.g., "O_zoop" "O_phyto" "O_zoopmort" "O_detrexp" "O_temp"

extractnc = function(sFile, var) {
  ncf = paste("tavg.", sFile, ".nc", sep = "")
  ncin <- nc_open(ncf)
  
  time <- ncvar_get(ncin, "time")
  ntime = length(time)
  
  lon <- ncvar_get(ncin, "longitude")
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "latitude")
  nlat <- dim(lat)
  dep = ncvar_get(ncin, "depth")
  ndep = dim(dep)
  depb = ncvar_get(ncin, "depth_edges")
  
  vname = ncatt_get(ncin, var, "long_name")
  vname = vname$value
  vunit = ncatt_get(ncin, var, "units")
  vunit = vunit$value
  
  # get data (last time step)
  v <- ncvar_get(ncin, var)
  
  ########################## integrate depth for last timestep ########################
  vdata = matrix(data = 0,
                 nrow = nlon,
                 ncol = nlat) #create matrix
  btemp = matrix(data = NA,
                 nrow = nlon,
                 ncol = nlat)
  bottom= matrix(data = NA,
                 nrow = nlon,
                 ncol = nlat)
  Tprof <- array(0, c(nlon, nlat, ndep + 1))
  
  v <- v[, , , ntime]
  
  
  #replace NAs with 0
  
  v[is.na(v)] <- 0
  
  # integrate depth
  if (var == "O_detrexp") {
    for (ilon in 1:100) {
      for (ilat in 1:100) {
        if (v[ilon, ilat, 1] != 0) {
          # if the first layer has value then...
          idet = max(which (v[ilon, ilat, ] != 0))  #index of last detritus flux position
          
          vdata[ilon, ilat] = vdata[ilon, ilat] + v[ilon, ilat, idet] * (depb[idet +
                                                                                1] - depb[idet])
          
        }
      }
    }
    
  } else if (var == "O_temp") {
    for (ilon in 1:100) {
      for (ilat in 1:100) {
        if (v[ilon, ilat, 1] != 0) {
          # if the first layer has value then...
          itemp = max(which (v[ilon, ilat, ] != 0))  #index of last detritus flux position
          
          btemp[ilon, ilat] = v[ilon, ilat, itemp]
          
          vdata[ilon, ilat] = mean(v[ilon, ilat, 1:2]) #mean top 130m
          
          Tprof[ilon, ilat, (2:(ndep+1))] = v[ilon, ilat, ] 
          Tprof[ilon, ilat, 1] = v[ilon, ilat, 1]
        }
      }
    }
    
  } else{
    for (ilon in 1:100) {
      for (ilat in 1:100) {
        for (idep in 1:ndep) {
          if (v[ilon, ilat, idep] != 0) {
            if (idep == 1) {
              #first layer:  depth to the surface
              vdata[ilon, ilat] = vdata[ilon, ilat] + v[ilon, ilat, idep] * (depb[2] -
                                                                               depb[1])
            } else if (idep == 2) {
              vdata[ilon, ilat] = vdata[ilon, ilat] + v[ilon, ilat, idep] * (depb[3] - depb[2])
              
            } else{
              break
            }
          }
        }
      }
    }
    
  }
  
  vdata[vdata == 0] <- NA #recover
  #####################################################################################
  
  # extract photic depth from daniel
  dan <-read.csv("Daniel_input_parameters.csv", header = T)[, c(3, 4, 10)] #long lat photic
  
  photic = matrix(data = NA,
                  nrow = nlon,
                  ncol = nlat)

  
  for (ilon in 1:100) {
    for (ilat in 1:100) {
      if (v[ilon, ilat, 1] != 0) {
        iilon = which(abs(dan$long - lon[ilon]) == min(abs(dan$long - lon[ilon])))
        
        ipho = iilon [which(abs(dan$lat[iilon] - lat[ilat]) == min(abs(dan$lat[iilon] -
                                                                         lat[ilat])))]
        
        if (length(ipho) != 1) {
          photic[ilon, ilat] = mean(dan$photic[ipho])
        } else{
          photic[ilon, ilat] = dan$photic[ipho]
        }
        
        bottom[ilon,ilat] = depb[max(which (v[ilon, ilat, ] != 0))+1]
        
        
        
      }
    }
  }

###########################################################3
  #export results
  y = list()
  y$lon = lon
  y$lat = lat
  y$dep = dep
  y$depb = depb
  y$nlon = nlon
  y$nlat = nlat
  y$ndep = ndep
  y$time = time
  y$ncin = ncin
  y$vdata = vdata
  y$vname = vname
  y$vunit = vunit
  y$photic = photic
  y$bottom = bottom
  
  if (var == "O_zoop") {
    gww <- vdata * 6.625 * 12.011 * 9
    y$gww = gww
    
    y$smzgww = y$gww * 0.9
    y$lgzgww = y$gww * 0.1
  } else if (var == "O_zoopmort") {
    y$vdata = y$vdata * 6.625 * 12.011 * 9 * 86400 * 365
    
    #divide all zoop
    y$smzmort = y$vdata * 0.9
    y$lgzmort = y$vdata * 0.1
    
  } else if (var == "O_detrexp") {
    y$detflux = vdata * 6.625 * 12.011 * 9 * 86400 * 365
    
  } else if (var == "O_temp") {
    y$mtemp = vdata # top100m mean temp
    y$btemp = btemp      # bottom temp
    y$Tprof = Tprof
  }
  
  return(y)
}