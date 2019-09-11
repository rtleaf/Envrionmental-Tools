# convert.day.to.month.r

file.in <- 
  c("file:///E:/3_rs_data/wind/Daily/FULL_NC.UWND.1DAY/SouthAtl/SouthAtl.NC.UWND.1DAY.time.series.csv",
    "file:///E:/3_rs_data/wind/Daily/FULL_NC.VWND.1DAY/SouthAtl/SouthAtl.NC.VWND.1DAY.time.series.csv",
    "file:///E:/3_rs_data/wind/Daily/FULL_NC.UWND.1DAY/NorthAtl/NorthAtl.NC.UWND.1DAY.time.series.csv",
    "file:///E:/3_rs_data/wind/Daily/FULL_NC.VWND.1DAY/NorthAtl/NorthAtl.NC.VWND.1DAY.time.series.csv",
    "file:///E:/3_rs_data/wind/Daily/FULL_NC.UWND.1DAY/MidAtl/MidAtl.NC.UWND.1DAY.time.series.csv",
    "file:///E:/3_rs_data/wind/Daily/FULL_NC.VWND.1DAY/MidAtl/MidAtl.NC.VWND.1DAY.time.series.csv"  )

for (z in 1:length(file.in)) {
  
  data.in <- read.csv(file.in[z])
  
  data.out <- as.data.frame(data.in %>% group_by(Year, Month) %>% summarise(Day = mean(Day), 
                                                                            Julian.Day = mean(Julian.Day), 
                                                                            Value = mean(Value), 
                                                                            Climatology = mean(Climatology), 
                                                                            Val.Relative.to.Climatology = mean(Val.Relative.to.Climatology)))
  data.out$X <- seq(1:dim(data.out)[1])
  name.out <- paste("monthly.wind",strsplit(file.in[z],"/")[[1]][length(strsplit(file.in[z],"/")[[1]])], sep = ".")
  write.csv(data.out, file = name.out) }
