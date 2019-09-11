# 07.08.2013 Robert Leaf and Kevin Friedland 

MO.temp.chlor.extract <- function(netCDF.wd = "C:\\3_rs_data")  {
  
  require(ncdf4)
  require(fgui)
  require(chron)
  
  apply.mask <- F
  nc.choose <- "ALL"
  index.name.3 <- "MO"
  
  # Determine the location of the .nc files
  netCDF.wd <- choose.dir(default = netCDF.wd, caption = "Where are the .nc files to analyze?")
  orig.directory <- getwd()
  nc.directory <- setwd(netCDF.wd)
  
  NCDF.vect <- dir(nc.directory,pattern = ".nc") 
  NCDF.vect <- paste(netCDF.wd,NCDF.vect,sep="\\") 
  NCDF.vect <- substr(NCDF.vect,nchar(netCDF.wd)+2,nchar(NCDF.vect))
  
  if (length(strsplit(NCDF.vect[1],"_sst_")[[1]]) == 2) { index.name.1 <- "TEMP"  } 
  if (length(strsplit(NCDF.vect[1],"chlor")[[1]]) == 2) { index.name.1 <- "CHLR"  } 
  
  year.label   <- as.numeric(substr(as.character(NCDF.vect),2,5))               # Using file name; extract year numbers                                          # Determine unique year
  day.label    <- floor(c(as.numeric(substr(as.character(NCDF.vect),6,8)) + as.numeric(substr(as.character(NCDF.vect),13,15)))/2)
  
  if (day.label[2] - day.label[1] > 10) { index.name.2 <- "MNTH"  }    
  if (day.label[2] - day.label[1] == 8) { index.name.2 <- "8DAY"  }
  
  month.label  <- month.day.year(day.label,c(1,0,year.label))$month             # Determine the month
  day.label    <- month.day.year(day.label,c(1,0,year.label))$day               # Determine the day of year
  jul.code  <- julian(month.label,day.label,year.label, c(1,0,1800))  
  date.code <- paste(sprintf("%04.0f",year.label),
                     sprintf("%02.0f",month.label),
                     sprintf("%02.0f",day.label),
                     sep = ".")    
  jul.code <- sprintf("%09i", jul.code) 
  
  netCDF.wd.sub <- paste(netCDF.wd,paste("FULL",paste(index.name.3,index.name.1,index.name.2,sep = "."),sep = "_"), sep = "\\")
  suppressWarnings(dir.create(netCDF.wd.sub))
  NC.RAST <- paste("RAST","FULL",paste(date.code,index.name.3,index.name.1,index.name.2,jul.code,"RData",sep = "."),sep = "_")
  
  for (j in 1:length(NCDF.vect))  {
    
    print(paste("Converting .nc to raster... ",NCDF.vect[j],sep = " "))
    raster.ncdf <- nc_open(paste(netCDF.wd,NCDF.vect[j],sep = "\\"))
    
    if (all((index.name.3 == "MO"),(index.name.1 == "CHLR"))) {
      
      netCDF.extent = c(-180, 180, -90, 90)
      netCDF.dim = c(2160,4320) 
      
      raster.full <- as.matrix(t(ncvar_get(raster.ncdf,varid="chlor_a")))       # Transpose matrix
      raster.full <- raster(raster.full)                                 
      
      # Set extent and projection
      xmin(raster.full) <- netCDF.extent[1]                         # Assign the extent of the full raster
      xmax(raster.full) <- netCDF.extent[2]
      ymin(raster.full) <- netCDF.extent[3]
      ymax(raster.full) <- netCDF.extent[4]
      projection(raster.full) <- NA                                 # Set the coordinate reference system to "NA"
    }
    
    if (all((index.name.3 == "MO"),(index.name.1 == "TEMP"))) {
      
      netCDF.extent = c(-180, 180, -90, 90)
      netCDF.dim = c(2160,4320)  
      
      raster.full <- as.matrix(t(ncvar_get(raster.ncdf,varid="sst")))       # Transpose matrix
      raster.full <- raster(raster.full)                           
      
      # Assign the extent of the full raster
      xmin(raster.full) <- netCDF.extent[1]                         
      xmax(raster.full) <- netCDF.extent[2]
      ymin(raster.full) <- netCDF.extent[3]
      ymax(raster.full) <- netCDF.extent[4]
      projection(raster.full) <- NA    }
    
    
    file.name.rast <- paste(netCDF.wd.sub,paste("RAST","FULL",
                                                paste(date.code[j],
                                                      index.name.3,
                                                      index.name.1,
                                                      index.name.2,
                                                      jul.code[j],"RData",sep = "."),sep = "_"), sep = "\\")
    
    masked.raster <- raster.full
    save(masked.raster,file = file.name.rast)
    nc_close(raster.ncdf)
  }
}  