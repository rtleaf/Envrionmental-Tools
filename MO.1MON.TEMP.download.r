MO.1MON.TEMP.download = function(target.dir = "E:/3_rs_data/temperature/MODIS/1_mon_dat") {
  
  # Function to download OISST-AVHRR www.ncei.noaa.gov
  # Written by R.Leaf and K.Friedland
  # Maintained by R.Leaf, robert.t.leaf@gmail.com
  # University of Southern Mississippi, Gulf Coast Research Lab
  # December 13, 2017
  
  # A20040012004031.L3m_MO_SST_sst_9km.nc
  # A20040322004060.L3m_MO_SST_sst_9km.nc
  # A20040612004091.L3m_MO_SST_sst_9km.nc
  # A20040922004121.L3m_MO_SST_sst_9km.nc
  # A20041222004152.L3m_MO_SST_sst_9km.nc
  # A20041532004182.L3m_MO_SST_sst_9km.nc
  # A20041832004213.L3m_MO_SST_sst_9km.nc
  # A20042142004244.L3m_MO_SST_sst_9km.nc
  # A20042452004274.L3m_MO_SST_sst_9km.nc
  # A20042752004305.L3m_MO_SST_sst_9km.nc
  # A20043062004335.L3m_MO_SST_sst_9km.nc
  # A20043362004366.L3m_MO_SST_sst_9km.nc

  dir.create(target.dir)
  
  require(lubridate)
  require(RCurl)
  
  st <- 2003
  nd <- as.numeric(format(Sys.Date(), "%Y"))
  year.seq <- seq(as.numeric(st), as.numeric(nd), by = 1)  
  
  for (z in 1:length(year.seq)) {
  
  start.day <- c(1, yday(as.Date(paste(year.seq[z],"-",sprintf("%02i", seq(1,12)),"-1",sep = "")))[-1])
  end.day <- c(start.day - 1)[-1]
  if (lubridate::leap_year(year.seq[z])) { end.day <- c(end.day, 366)   }   
  if (!lubridate::leap_year(year.seq[z])) { end.day <- c(end.day, 365)   }   
  
  # https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2004 032 2004 060.L3m_MO_NSST_sst_9km.nc
  
    for (k in 1:length(start.day)) {  
      file.url <- paste("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",
                        "A",
                        year.seq[z],
                        sprintf("%03i", start.day[k]),
                        year.seq[z],
                        sprintf("%03i", end.day[k]), 
                        ".L3m_MO_NSST_sst_9km.nc",
                        sep = "")
      
      targ.file <- paste(target.dir,paste("A",strsplit(file.url,"A")[[1]][2], sep = ""), sep = "/")
      if (!file.exists(targ.file)) { download.file(file.url, targ.file, mode = "wb") }
    }
  }
}