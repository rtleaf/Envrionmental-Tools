IC.MON.download = function(target.dir = "E:/3_rs_data/temperature/ICOADS") {
  
  # Function to download ICOADS data
  # Note, when using download.file, the function argument mode = "wb" must be used for .nc files to be saved properly.
  
  require(lubridate)
  require(RCurl)
  
  # 1. Determine all candidate files available in OISST directory
  st <- as.Date("1880-01-01")
  en <- Sys.Date()
  month.seq <- seq(st, en, by = "1 month")
  
  y.vect <- format(month.seq,"%Y")
  m.vect <- format(month.seq,"%m")
  dec.vect <- as.numeric(y.vect) - as.numeric(substr(y.vect,4,4))

  # https://data.nodc.noaa.gov/icoads/1800s/1800s/
  # https://data.nodc.noaa.gov/icoads/1800s/1800s/ICOADS_R3.0.0_1800-01.nc
  
  url.file <- paste("https://data.nodc.noaa.gov/icoads/",dec.vect,"s/",dec.vect,"s/ICOADS_R3.0.0_",y.vect,"-",m.vect,".nc", sep = "")
  cand.files <- paste("ICOADS_R3.0.0_",y.vect,"-",m.vect,".nc", sep = "")
  
  ind <- which(y.vect %in% c(2015:max(y.vect)))
  url.file[ind] <- paste("https://data.nodc.noaa.gov/icoads/",dec.vect[ind],"s/",dec.vect[ind],"s/ICOADS_R3.0.1_",y.vect[ind],"-",m.vect[ind],".nc", sep = "")
  cand.files[ind] <- paste("ICOADS_R3.0.1_",y.vect[ind],"-",m.vect[ind],".nc", sep = "")
  
  
  # 2. Determine files in the target directory
  comp.files <- list.files(path = target.dir, pattern = ".nc")
  ind.rem <- which(cand.files %in% comp.files)
  if (length(ind.rem) > 0) { 
    cand.files <- cand.files[-ind.rem] 
    url.file <- url.file[-ind.rem] }
  
  for (j in 1:length(cand.files)) {
    url.add <- url.file[j]
    targ.file <- paste(target.dir, "/", cand.files, sep = "")[j]
    download.file(url.add, targ.file, mode = "wb") 
  }
}