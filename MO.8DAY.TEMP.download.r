MO.8DAY.TEMP.download = function(target.dir = "E:/3_rs_data/temperature/MODIS/8_day_dat") {
  
  # Function to download OISST-AVHRR www.ncei.noaa.gov
  # Written by R.Leaf and K.Friedland
  # Maintained by R.Leaf, robert.t.leaf@gmail.com
  # University of Southern Mississippi, Gulf Coast Research Lab
  # December 13, 2017
  
  # https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20110092011016.L3m_8D_SST_sst_9km.nc
  # A2002  185  2002  192.L3m_8D_SST_sst_9km.nc
  # A2003  001  2003  008.L3m_8D_NSST_sst_9km.nc
  # A2003  337  2003  344.L3m_8D_SST_sst_9km.nc
  
  dir.create(target.dir)
  
  require(lubridate)
  require(RCurl)
  
  st <- 2003
  nd <- as.numeric(format(Sys.Date(), "%Y"))
  year.seq <- seq(as.numeric(st), as.numeric(nd), by = 1)
  
  for (z in 1:length(year.seq)) {
    
    for (k in 1:length(c(seq(1,360, by = 8), 361))) {  
      
      day.seq.1 <- c(seq(1,360, by = 8), 361)
      day.seq.2 <- c(seq(8,360, by = 8), 365)
      if (lubridate::leap_year(year.seq[z])) { day.seq.2 <- c(seq(8,360, by = 8), 366)   }
      
      file.url <- paste("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",
                        "A",
                        year.seq[z],
                        sprintf("%03i", day.seq.1[k]),
                        year.seq[z],
                        sprintf("%03i", day.seq.2[k]), 
                        ".L3m_8D_SST_sst_9km.nc",
                        sep = "")
      
      targ.file <- paste(target.dir,paste("A",strsplit(file.url,"A")[[1]][2], sep = ""), sep = "/")
      if (!file.exists(targ.file)) { download.file(file.url, targ.file, mode = "wb") }
    }
  }
}