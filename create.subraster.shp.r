# create_subraster_shp is a function that will create a nested directory of RAST files
# The geographic subset can be based on a user specified geographic region
# or can be determined from a shape file - these regions are smaller than the parent RAST
# files. 
#
# 01.20.2014 Robert Leaf and Kevin Friedland 

create.subraster.shp <- function(input.dir = "E:\\3_rs_data\\temperature\\ICOADS\\FULL_IC.TEMP.1MON",
                                 ShapeStem = "C:\\Users\\w906282\\Dropbox\\shp.files\\AtlMenRange\\AtlMenRange.shp") {                        
  
  require(maptools)
  require(raster)
  
  if (is.empty(input.dir)) {                                  
    input.dir <- choose.dir(default = "", caption = "Where are the raster files?")  }
  
  # Create extraction region
  if (is.empty(ShapeStem)) {                                  
    ShapeStem  <- choose.files(default = "", caption = "Choose the shapefile for region extraction.")  }
  
  # Extract shape file name
  OverlayFile.name <- unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/"))[length(unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/")))]
  shape.name <- substr(OverlayFile.name,1,c(nchar(OverlayFile.name) - 4))
  rm(OverlayFile.name)
  
  extract.shape <- readShapePoly(ShapeStem) 
  bbox.mat <- bbox(extract.shape)
  
  crop.bound <-extent(as.matrix(cbind(
    floor(bbox(extract.shape)[,1]) - (bbox.mat %% 2)[,1],
    ceiling(bbox(extract.shape)[,2]) + (bbox.mat %% 2)[,2])))
  rm(bbox.mat)
  
  # Import raster and meta data derived from netCDF.crop
  input.stack.name <- dir(path = input.dir,pattern = "RAST")
  raster.list  <- paste(input.dir,input.stack.name,sep="\\")
  rm(input.stack.name)
  
  # Create sub-directory if it does not exist
  extract.sub <- paste(input.dir,shape.name, sep = "\\")
  suppressWarnings(dir.create(extract.sub))
  
  # Output file name
  rast.file.name <- c()
  for (j in 1:length(raster.list)) {
    rast.stem <- strsplit(raster.list[j], split = "_")[[1]][length(strsplit(raster.list[j], split = "_")[[1]])]
    rast.file.name[j] <- paste(extract.sub,paste("RAST",shape.name,rast.stem,sep="_"),sep="\\") }
  
  ind <- which(!rast.file.name %in% paste(extract.sub,dir(extract.sub), sep = "\\"))
  if (length(ind) > 0) {
    
    for (j in 1:length(ind)) { 
      
      load(raster.list[ind[j]])
      
      # Create mask
      if (j == 1) {
        print("Creating raster mask")
        seq.raster <- masked.raster
        seq.raster[] <- seq(1,ncell(seq.raster[]))
        extract.mask <- raster::extract(seq.raster,extract.shape, weights = T)
        cell.index <- extract.mask[[1]][,1]
        extract.mask <- seq.raster
        extract.mask[1:ncell(extract.mask)]  <- NA
        extract.mask[cell.index] <- 1   }
      
      print(paste((j),"of",length(ind)))
      
      masked.raster <- mask(masked.raster,extract.mask)
      masked.raster <- crop(masked.raster, crop.bound)
      save(masked.raster,file = rast.file.name[ind[j]])
    }
  }
}