# wind_ncdf_2_full_raster serves to write raster files of the form "RAST"
# For each netCDF file in the specified directory a single "RAST" .RData file with
# a variable named "masked.raster". 
#
# 08.09.2012 Robert Leaf and Kevin Friedland

AR.1MON.extract <- function(target.dir = "E:\\3_rs_data\\AR5 Climate Projections")  {
  
  require(chron)
  require(ncdf4)
  require(raster)

  # Determine the location of the .nc files
  target.nc <- paste(target.dir, dir(target.dir, pattern = ".nc"),sep="\\")
  
  for (z in 1:length(target.nc)) {
    
    print(paste("Rasterizing...", target.nc[z]))
    
    target.nc.rast.name <- paste("tos_",strsplit(strsplit(target.nc[z],"tos_")[[1]][2],".nc")[[1]][1], sep = "")
    target.rast.dir <- paste("FULL_NC",strsplit(strsplit(target.nc[z],"tos")[[1]][2],".nc")[[1]][1], sep = "")
    target.rast.dir <- paste(target.dir, target.rast.dir, sep = "\\")
    
    raster.ncdf <- nc_open(target.nc[z])  
    y.data <- ncvar_get(raster.ncdf,varid = "lat")[1,]   
    x.data <- ncvar_get(raster.ncdf,varid = "lon")[,1]
    lon.val <- order(x.data)
    z.data <- ncvar_get(raster.ncdf,varid = "tos")
    nc_close(raster.ncdf) # Close file
    
    for (k in 1:dim(z.data)[3]) {    
      
      if (k == 1) { dir.create(path = target.rast.dir) }
      
      t.data <- strsplit(target.rast.dir[1],"_")[[1]][length(strsplit(target.rast.dir[1],"_")[[1]])]
      st.nd.t <- strsplit(t.data,"-")[[1]]
      yr. <- substr(st.nd.t,1,4)
      data.mat <- cbind(sort(rep(seq(as.numeric(yr.[1]),as.numeric(yr.[2])),12)), rep(seq(1,12),length(seq(as.numeric(yr.[1]),as.numeric(yr.[2])))),1)
      data.mat <- data.frame(data.mat)
      names(data.mat) <- c("Year", "Month", "Day")
      
      # Obtain equation data for the netCDF file; if these are not available the values will default to zero
      raster.full <- as.matrix(t(z.data[,,k])) - 273.15                # Transpose matrix
      raster.full <- raster.full[,lon.val]
      raster.full <- raster(raster.full)                  # These data need to be inverted on the y-axis
      
      xmin(raster.full) <- min(x.data)                        # Assign the extent of the full raster
      xmax(raster.full) <- max(x.data)
      ymin(raster.full) <- min(y.data)
      ymax(raster.full) <- max(y.data)
      
      # Need to change raster x-dimension: Split the raster into two halves and rearrange them
      hemi.1 <- as.matrix(raster::crop(raster.full,c(180,360,-90,90)))
      hemi.2 <- as.matrix(raster::crop(raster.full,c(0,180,-90,90)))
      raster.full <- flip(raster(cbind(hemi.1,hemi.2)),'y')
      extent(raster.full) <- extent(-180,180,-90,90)
      projection(raster.full) <- NA                          # Set the coordinate reference system to "NA"
      
      masked.raster <- raster.full      
      
      year.label  <- data.mat[k,1]
      day.label   <- data.mat[k,3]
      month.label <- data.mat[k,2]
      jul.code  <- julian(as.numeric(month.label),as.numeric(day.label),as.numeric(year.label), c(1,0,1800))  
      date.code <- paste(sprintf("%04.0f",as.numeric(year.label)),
                         sprintf("%02.0f",as.numeric(month.label)),
                         sprintf("%02.0f",as.numeric(day.label)),
                         sep = ".")
      jul.code <- sprintf("%09i", jul.code) 
      
      # RAST_FULL_2003.02.14.MO.CHLR.MNTH.000074189.RData
      # RAST_FULL_2006.01.01.AR.TEMP.1MON.000075241.RData
      
      save.name <- paste(target.rast.dir,paste("RAST","FULL",paste(date.code,"AR.TEMP","MNTH",
                                                                   strsplit(target.rast.dir, "_")[[1]][7],
                                                                   strsplit(target.rast.dir, "_")[[1]][8],
                                                                   jul.code,"RData",sep = "."),sep = "_"),sep = "\\")
      save(masked.raster,file = save.name)
      rm(masked.raster)
    }
  }
}