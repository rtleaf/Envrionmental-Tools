# wind_ncdf_2_full_raster serves to write raster files of the form "RAST"
# For each netCDF file in the specified directory a single "RAST" .RData file with
# a variable named "masked.raster". 
#
# 08.09.2012 Robert Leaf and Kevin Friedland

UV.1DAY.extract <- function()  {
  
  require(chron)
  require(ncdf4)
  require(raster)
  require(maptools)
  require(spatial)
  require(sp)
  
  # Determine the location of the .nc files
  netCDF.wd <- choose.dir(caption = "Where are the .nc files to analyze?")
  NCDF.vect.1 <- dir(netCDF.wd,pattern = ".nc")
  NCDF.vect.uwnd <- dir(netCDF.wd,pattern = "uwnd")
  NCDF.vect.vwnd <- dir(netCDF.wd,pattern = "vwnd")
  
  ind <- c()
  ind[[1]] <- intersect(NCDF.vect.1,NCDF.vect.uwnd)
  ind[[2]] <- intersect(NCDF.vect.1,NCDF.vect.vwnd) 
  ind.name <- c("uwnd","vwnd")
  
  for (z in 1:length(ind)) {
    index.name.1 <- toupper(substr(ind[[z]],1,4)[1])
    dir.name <- paste("FULL_NC.",index.name.1,".1DAY",sep = "")
    dir.name <- paste(netCDF.wd,dir.name,sep="\\")
    
    # Create sub-directory
    suppressWarnings(dir.create(dir.name))
    nc.file.path <- paste(netCDF.wd,ind[[z]],sep = "\\")
    
    for (k in 1:length(nc.file.path)) {
      raster.ncdf <- nc_open(nc.file.path[k])
      
      t.data <- ncvar_get(raster.ncdf,varid = "time")
      x.data <- ncvar_get(raster.ncdf,varid = "lon")
      y.data <- ncvar_get(raster.ncdf,varid = "lat")
      z.data <- ncvar_get(raster.ncdf,varid = ind.name[z])
      
      nc_close(raster.ncdf) # Close file
      
      for (j in 1:dim(z.data)[4]) {
        
        year.label <- substr(strsplit(nc.file.path[k],".nc")[[1]],nchar(strsplit(nc.file.path[k],".nc")[[1]])-3,nchar(strsplit(nc.file.path[k],".nc")[[1]]))
        day.label <- month.day.year(j,c(1,0,as.numeric(year.label)))$day
        month.label <- month.day.year(j,c(1,0,as.numeric(year.label)))$month
        jul.code  <- julian(as.numeric(month.label),as.numeric(day.label),as.numeric(year.label), c(1,0,1800))  
        date.code <- paste(sprintf("%04.0f",as.numeric(year.label)),
                           sprintf("%02.0f",as.numeric(month.label)),
                           sprintf("%02.0f",as.numeric(day.label)),
                           sep = ".")
        jul.code <- sprintf("%09i", jul.code) 
        
        # Obtain equation data for the netCDF file; if these are not available the values will default to zero
        raster.full <- as.matrix(t(z.data[,,1,j]))                # Transpose matrix
        raster.full <- raster(raster.full)                  # These data need to be inverted on the y-axis
        
        xmin(raster.full) <- min(x.data)                        # Assign the extent of the full raster
        xmax(raster.full) <- max(x.data)
        ymin(raster.full) <- min(y.data)
        ymax(raster.full) <- max(y.data)
        
        # Need to change raster x-dimension: Split the raster into two halves and rearrange them
        hemi.1 <- as.matrix(raster::crop(raster.full,c(180,360,-90,90)))
        hemi.2 <- as.matrix(raster::crop(raster.full,c(0,180,-90,90)))
        raster.full <- raster(cbind(hemi.1,hemi.2))
        
        # Re-assign the extent of the raster
        x.increment <- (360/dim(x.data))/2
        xmin(raster.full) <- min(x.data) - 180 + x.increment     # Assign the extent of the full raster
        xmax(raster.full) <- max(x.data) - 180 + x.increment
        
        y.increment <- (180/dim(y.data)[2])/2
        ymin(raster.full) <- -90
        ymax(raster.full) <-  90
        projection(raster.full) <- NA                          # Set the coordinate reference system to "NA"
        
        masked.raster <- raster.full      
        print(paste("Rasterizing",ind.name[z],"for year",year.label,"--",j,"of",dim(z.data)[4],sep = " "))
        
        save.name <- paste(dir.name,paste("RAST","FULL",paste(date.code,"NC",toupper(ind.name[z]),"1DAY",jul.code,"RData",sep = "."),sep = "_"),sep = "\\")
        save(masked.raster,file = save.name)
        rm(masked.raster)
      }
    }
  }
}