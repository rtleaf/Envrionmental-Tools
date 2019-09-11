AV.1DAY.download = function(target.dir = "E:/3_rs_data/temperature/OISST") {
  
  # Function to download OISST-AVHRR www.ncei.noaa.gov
  # Written by R.Leaf and K.Friedland
  # Maintained by R.Leaf, robert.t.leaf@gmail.com
  # University of Southern Mississippi, Gulf Coast Research Lab
  # December 13, 2017
  
  
  # The target.dir argument specifies the local directory that the downloaded .nc files are placed in.
  # The OISST AVHRR-only files are found in: "https://www.ncei.noaa.gov/thredds/fileServer/OisstBase/NetCDF/AVHRR/"
  # If this location moves, then the object file.string.start (line 39) can be suitably altered.
  
  # Note, when using download.file, the function argument mode = "wb" must be used for .nc files to be saved properly.
  
  require(lubridate)
  require(RCurl)
  
  # 1. Determine all candidate files available in OISST directory
  st <- as.Date("1982-01-01")
  en <- Sys.Date()
  day.seq <- seq(st, en, by = "1 day")
  rm(st,en)
  
  directory.date <- paste(format(day.seq,"%Y"),
                          format(day.seq,"%m"),
                          sep = "")
  
  cand.files <- paste("avhrr-only-v2.",
                      format(day.seq,"%Y"),
                      format(day.seq,"%m"),
                      format(day.seq,"%d"),
                      ".nc", sep = "")
  
  rm(day.seq)
  
  # 2. Determine files in the target directory
  comp.files <- list.files(path = target.dir, pattern = ".nc")
  ind.rem <- which(cand.files %in% comp.files)
  if (length(ind.rem) > 0) { 
    cand.files <- cand.files[-ind.rem] 
    directory.date <- directory.date[-ind.rem] }
  
  file.string.start = "https://www.ncei.noaa.gov/thredds/fileServer/OisstBase/NetCDF/AVHRR/"
  suppressWarnings(dir.create(target.dir))
  for (j in 1:length(cand.files)) {
    url.add <- paste(file.string.start, directory.date, "/", cand.files, sep = "")[j]
    if (url.exists(url.add)) {
    targ.file <- paste(target.dir, "/", cand.files, sep = "")[j]
    download.file(url.add, targ.file, mode = "wb") }
  }
}