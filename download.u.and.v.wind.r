download.u.v.wind = function(target.dir = "E:/3_rs_data/wind/Daily") {
  
  # Function to download NCEP NCAR u- and v-wind data
  # Written by R.Leaf and K.Friedland
  # Maintained by R.Leaf, robert.t.leaf@gmail.com
  # University of Southern Mississippi, Gulf Coast Research Lab
  # December 13, 2017
  # The target.dir argument specifies the local directory that the downloaded .nc files are placed in.
  # The U- and V- wind vector .nc files are found in: "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/uwnd"
  # If this location moves, then the object cand.files.u and cand.files.v (line 25 and 26) can be suitably altered.
  
  # Note, when using download.file, the function argument mode = "wb" must be used for .nc files to be saved properly.
  
  require(lubridate)
  
  # 1. Determine all candidate files available in OISST directory
  st <- 1948
  en <- as.numeric(format(Sys.Date(),"%Y"))
  year.seq <- seq(st, en)
  rm(st,en)
  
  # ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/uwnd.1948.nc
  # ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/vwnd.1948.nc
  
  cand.files.u <- paste("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/uwnd.",year.seq,".nc", sep = "")
  cand.files.v <- paste("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/vwnd.",year.seq,".nc", sep = "")
  
  suppressWarnings(dir.create(target.dir))
  
  target.file <- c(paste("uwnd.",year.seq,".nc",sep = ""),paste("vwnd.",year.seq,".nc",sep = ""))
  cand.files <- c(cand.files.u, cand.files.v)
  ind <- which(!cand.files %in% dir(target.dir, pattern = ".nc"))
  
  cand.files <- cand.files[ind]
  target.file <- paste(target.dir,"/",target.file, sep = "")[ind]
  
  if (length(cand.files) > 0) {
  for (j in 1:length(cand.files)) {
    download.file(cand.files[j], target.file[j], mode = "wb") }}
}
