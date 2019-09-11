create.time.series <- function(input.dir = "E:\\3_rs_data\\temperature\\ICOADS\\FULL_IC.TEMP.1MON",
                               ShapeStem = "C:\\Users\\w906282\\Dropbox\\shp.files\\AtlMenRange\\AtlMenRange.shp") {
  
  if (is.empty(input.dir)) {                                  
    input.dir <- choose.dir(default = "", caption = "Where are the raster files?")  }
  
  if (is.empty(ShapeStem)) {                                  
    ShapeStem  <- choose.files(default = "", caption = "Choose the shapefile for region extraction.")  }
  
  require(raster)
  require(dplyr)
  require(sp)
  require(maptools)
  require(chron)
  
  OverlayFile.name <- unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/"))[length(unlist(strsplit(chartr(old = "\\",new = "/",ShapeStem),"/")))]
  shape.name <- substr(OverlayFile.name,1,c(nchar(OverlayFile.name) - 4))
  rm(OverlayFile.name)
  extract.shape <- readShapePoly(ShapeStem) 
  
  # Import raster and meta data derived from netCDF.crop
  input.stack.name <- dir(path = input.dir,pattern = "RAST")
  name.in <- strsplit(strsplit(input.stack.name[1],"RAST")[[1]][length(strsplit(input.stack.name[1],"RAST")[[1]])],".RData")[[1]]
  sensor.name <- substr(name.in,nchar(name.in)-21, nchar(name.in)-10)
  rm(name.in)
  
  ts.matrix <- matrix(NA,nrow = length(input.stack.name), ncol = 6)
  
  for (j in 1:length(input.stack.name)) {
  load(paste(input.dir,input.stack.name[j],sep = "\\"))
  val.extract <- extract(x = masked.raster, y = extract.shape, cellnumbers = T, weights = T)[[1]]
  w.val.extract <- weighted.mean(x = val.extract[,2], w = val.extract[,3], na.rm = T)
  
  jul.val <- as.numeric(substr(input.stack.name[j], nchar(input.stack.name[j]) - 14, nchar(input.stack.name[j]) - 6))
  y.val <- as.numeric(substr(input.stack.name[j], nchar(input.stack.name[j]) - 38, nchar(input.stack.name[j]) - 35))
  m.val <- as.numeric(substr(input.stack.name[j], nchar(input.stack.name[j]) - 33, nchar(input.stack.name[j]) - 32))
  d.val <- as.numeric(substr(input.stack.name[j], nchar(input.stack.name[j]) - 30, nchar(input.stack.name[j]) - 29))
  jul.dat <- julian(x = m.val, d = d.val, y = 1900, origin. = c(x = 1, d = 0, y = 1900))
  
  ts.matrix[j,] <- c(y.val, m.val, d.val, jul.val,jul.dat, w.val.extract)
  
  print(paste("Extracting data from raster",j,"of",length(input.stack.name)))
  }
  
  
  ts.file.name <- paste(input.dir,"\\",shape.name,".",sensor.name,".","time.series.csv", sep = "")
  time.series <- data.frame(Year = ts.matrix[,1],
                            Month = ts.matrix[,2],
                            Day = ts.matrix[,3],
                            Julian.Day = ts.matrix[,4],
                            Julian.Date = ts.matrix[,5],
                            Value = ts.matrix[,6]) 
  
  time.series <- time.series[complete.cases(time.series),]
  
  if (substr(sensor.name,nchar(sensor.name)-3,nchar(sensor.name)) == "1MON") {
    climatology <- as.data.frame(time.series %>% group_by(Month) %>% summarise(Climatology = mean(Value)))
    time.series <- left_join(time.series, climatology,by = "Month") }
  
  if (substr(sensor.name,nchar(sensor.name)-3,nchar(sensor.name)) != "1MON") {
    climatology <- as.data.frame(time.series %>% group_by(Julian.Date) %>% summarise(Climatology = mean(Value)))
    time.series <- left_join(time.series, climatology,by = "Julian.Date")  }

  time.series$Val.Relative.to.Climatology <- time.series$Value - time.series$Climatology
  write.csv(time.series, file = ts.file.name)  
  
}