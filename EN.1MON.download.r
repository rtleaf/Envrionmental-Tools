EN.1MON.download = function(target.dir = "E:/3_rs_data/ENSO") {
  
  require(lubridate)
  require(RCurl)
  require(ProgGUIinR)
  
  # 1. Determine all candidate files available in OISST directory
  st <- 1950
  en <- as.numeric(format(Sys.Date(),"%Y"))
  year.seq <- seq(st, en)
  
  val <- c()
  for (j in 1:length(year.seq)) {
    enso.in <- read.delim("https://www.esrl.noaa.gov/psd/data/correlation/mei.data", skip = 1, header = F, sep = "\t")
    
    year. <- as.numeric(strsplit(as.character(enso.in[j,])," ")[[1]])[which(!is.na(as.numeric(strsplit(as.character(enso.in[j,])," ")[[1]])))][1]
    month. <- seq(1,12)
    vals <- as.numeric(strsplit(as.character(enso.in[j,])," ")[[1]])[which(!is.na(as.numeric(strsplit(as.character(enso.in[j,])," ")[[1]])))][2:13]
    
    val <- rbind(val, cbind(year., month., vals)) }
  
  ind <- which(val == -999)
  if (length(ind) > 0) { val[ind] <- NA }
  
  suppressWarnings(dir.create(target.dir))
  
  enso.data <-  data.frame(Year = val[,1],
                           Month = val[,2],
                           Value = val[,3])
  
  targ.file <- paste("ENSO.",min(val[,1]),".",max(val[,1]),".csv", sep = "")
  
  write.csv(enso.data, file = paste(target.dir, targ.file, sep = "/"))
  
  ### Save file here
}