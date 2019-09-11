IC.1MON.extract <- function(target.dir = "E:/3_rs_data/temperature/ICOADS") {
  
  # This function is not currently working - place holder only 

  require(chron)
  require(ncdf4)
  require(raster)
  
  index.name.1 <- "TEMP"
  index.name.2 <- "1MON"
  index.name.3 <- "IC"
  
  # Determine the location of the .nc files
  netCDF.wd <- target.dir
  NCDF.vect <- dir(netCDF.wd,pattern = ".nc")
  
  # Extract data from the file name
  year.label   <- as.numeric(substr(as.character(NCDF.vect),15,18))
  day.label    <- 1
  month.label  <- as.numeric(substr(as.character(NCDF.vect),20,21))
  jul.code  <- julian(month.label,day.label,year.label, c(1,0,1800))
  date.code <- paste(sprintf("%04.0f",year.label),
                     sprintf("%02.0f",month.label),
                     sprintf("%02.0f",day.label),sep = ".")
  
  # Insert leading zeros in julian code
  jul.code <- sprintf("%09i", jul.code) 
  
  # Create sub-directory                                                                                                                # Set and create new directories
  netCDF.wd.sub <- paste(netCDF.wd,paste("FULL",paste(index.name.3,index.name.1,index.name.2,sep = "."),sep = "_"), sep = "\\")        # Name a new directory to place the saved netCDF files
  suppressWarnings(dir.create(netCDF.wd.sub))
  
  NC.RAST <- paste("RAST","FULL",paste(date.code,index.name.3,index.name.1,index.name.2,jul.code,"RData",sep = "."),sep = "_")
  
  ind. <- which(!(NC.RAST %in% list.files(path = netCDF.wd.sub, pattern = "RAST_FULL_")))
  
  NC.RAST <- NC.RAST[ind.]
  NCDF.vect <- NCDF.vect[ind.]
  
  if (length(NCDF.vect) > 0)      {
    for (j in 1:length(NCDF.vect))  {
      
      print(paste("Converting .nc to raster... ",NCDF.vect[j],sep = " "))
      raster.ncdf <- ncdf4::nc_open(paste(netCDF.wd,NCDF.vect[j],sep = "\\"))

      # Initialize values
      raster.full <- raster(xmn=0, xmx=360, 
                            ymn=-90, ymx=90, 
                            crs = NA, resolution = 0.5, vals=NULL)
      
      vals <- cbind(ncdf4::ncvar_get(raster.ncdf,varid = "lat"),
                    ncdf4::ncvar_get(raster.ncdf,varid = "lon"),
                    ncdf4::ncvar_get(raster.ncdf,varid = "SST"))
      
      vals <- vals[complete.cases(vals),]
      vals <- vals[which(vals[,3] > -5),]
      vals <- cbind(vals,cellFromXY(object = raster.full, xy = vals[,c(2,1)]))
      vals <- as.data.frame(vals)
      names(vals) <- c("Lat", "Lon", "SST", "Cell")
      
      cell.sst <- as.data.frame(vals %>% group_by(Cell) %>% summarise(Mean.SST = mean(SST)))
      raster.full[cell.sst$Cell] <- cell.sst$Mean.SST
                       # Set the coordinate reference system to "NA"
      
      masked.raster <- raster.full
      
      # Need to change raster x-dimension: Split the raster into two halves and rearrange them
      ex.hemi <- extent(matrix(c(0,180,-90,90),2,2,byrow = T))
      hemi.1 <- as.matrix(raster::crop(masked.raster,ex.hemi))
      ex.hemi <- extent(matrix(c(180,360,-90,90),2,2,byrow = T))
      hemi.2 <- as.matrix(raster::crop(masked.raster,ex.hemi))
      raster.full <- raster(cbind(hemi.2,hemi.1))
      
      # Re-assign the extent of the raster
      raster.extent.new <- c(-180,180,-90,90)   # Set the extent of the raster and projection
      xmin(raster.full) <- raster.extent.new[1]
      xmax(raster.full) <- raster.extent.new[2]
      ymin(raster.full) <- raster.extent.new[3]
      ymax(raster.full) <- raster.extent.new[4]
      projection(raster.full) <- NA                          # Set the coordinate reference system to "NA"
      
      
      masked.raster <- raster.full    
      
      
      
      file.name.rast <- paste(netCDF.wd.sub,NC.RAST[j], sep = "\\")
       
      save(masked.raster,file = file.name.rast)
      
      nc_close(raster.ncdf)
      
    }
    
    if (length(NCDF.vect) == 0) {print("You have rasterized all .nc files")}    
  }
}  