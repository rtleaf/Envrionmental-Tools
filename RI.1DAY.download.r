RI.1DAY.download = function(target.dir = "E:/3_rs_data/River discharge data") {
  
  # 1. Determine all candidate files available in OISST directory
  st <- 1950
  en <- as.numeric(format(Sys.Date(),"%Y"))-1
  year.seq <- seq(st, en)
  
  suppressWarnings(dir.create(target.dir))
  
  for (j in 1:length(year.seq)) {
    
    date.st <- paste("01/01/",year.seq[j],sep = "")
    date.nd <- paste("12/31/",year.seq[j],sep = "")
    cand.url <- paste("http://rivergages.mvr.usace.army.mil/WaterControl/yearly_tables2.cfm?sid=01100Q&from1=",
                      date.st,
                      "&to1=",
                      date.nd,
                      "&dt=S&param=QR", sep = "")
    
    val <- read.delim(file = cand.url, header = TRUE, sep = "\t")
    val <- as.character(val[,1])
    val <- val[nchar(val) >= 1]
    
    val[which(val == "M")] <- " -1"
    ind <- sort(c(which(val == "<font size = -2>"), which(val == "<font size = -2 color = red>"))) + 1
    val <- val[ind]
    val[which(val == "&nbsp;")] <- " -1"
    val <- as.numeric(val)
    val <- val[-which(is.na(val))]
    
    fill.mat <- matrix(NA, nrow = 31, ncol = 12)
    for (k in 1:31) {
      fill.mat[k,1:12] <-  val[1:12]
      val <- val[-seq(1,12)]  }
    fill.mat[which(fill.mat == -1)] <- NA
    
    st.val <- as.Date(paste(year.seq[j],"-01-01", sep = ""))
    en.val <- as.Date(paste(year.seq[j],"-12-31", sep = ""))
    day.seq <- seq(st.val, en.val, by = "1 day")
    
    val <- c()
    for (k in 1:12) {
      mo.max <- lubridate::days_in_month(k)
      if (leap_year(day.seq[1]) & k == 2) { mo.max <- 29 }
      val <- c(val, fill.mat[1:mo.max, k])  }
    
    y.vect <- format(day.seq,"%Y")
    m.vect <- format(day.seq,"%m")
    d.vect <- format(day.seq,"%d")  
    julian.seq <- seq(1,length(day.seq))
    
    ms.discharge.data = data.frame(Year = y.vect,
                                   Month = m.vect,
                                   Day = d.vect,
                                   Julian.Day = julian.seq,
                                   Value = val)
    
    targ.file <- paste("MS.River.Discharge.Tarbert.Landing.",y.vect[j],".csv", sep = "")
    
    write.csv(ms.discharge.data, file = paste(target.dir, targ.file, sep = "/"))
    print(paste("Saving file: ", paste(target.dir, targ.file, sep = "/")))
    ### Save file here
  }
  
  

  
  data.file <- paste(target.dir, dir("E:/3_rs_data/River discharge data", pattern = ".csv"), sep = "/")
  
  if (length(strsplit(data.file[1], "MS.RIV.Discharge" )[[1]]) == 2) {
    data.file <- data.file[-c(1,2)]
  }
  
  val <- c()
  for (j in 1:length(data.file)) {
    val <- rbind(val, read.csv(data.file[j]))   }
  
  jul.val <- as.data.frame(val %>% group_by(Julian.Day) %>% summarise(Day.val = mean(Value, na.rm = T)))
  val <- left_join(val, jul.val, by = "Julian.Day")
  val$Val.Relative.to.Climatology <- val$Value - val$Day.val
  names(val)[7] <- "Climatology"
  
  write.csv(val, file = paste(target.dir,"/MS.RIV.Discharge.Daily.",min(val$Year),".",max(val$Year),".csv", sep = ""))
  
  
  month.val <- as.data.frame(val %>% group_by(Year, Month) %>% summarise(Month.Val = mean(Value),
                                                                         Month.Clim = mean(Climatology),
                                                                         Month.Val.Relative.to.Climatology = mean(Val.Relative.to.Climatology)))
  write.csv(month.val, file = paste(target.dir,"/MS.RIV.Discharge.Month.",min(val$Year),".",max(val$Year),".csv", sep = ""))
  
}  
