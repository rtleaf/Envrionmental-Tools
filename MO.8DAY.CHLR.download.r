MO.8DAY.CHLR.download = function(target.dir = "E:/3_rs_data/chlorophyll/MODIS/8_day_dat") {
  
  # Function to download OISST-AVHRR www.ncei.noaa.gov
  # Written by R.Leaf and K.Friedland
  # Maintained by R.Leaf, robert.t.leaf@gmail.com
  # University of Southern Mississippi, Gulf Coast Research Lab
  # December 13, 2017
  
  # https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030012003008.L3m_8D_CHL_chlor_a_9km.nc
  
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
                        ".L3m_8D_CHL_chlor_a_9km.nc",
                        sep = "")
      
      targ.file <- paste(target.dir,paste("A",strsplit(file.url,"A")[[1]][2], sep = ""), sep = "/")
      if (!file.exists(targ.file)) { download.file(file.url, targ.file, mode = "wb") }
    }
  }
}