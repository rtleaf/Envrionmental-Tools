# determine.spring.phenology

file.in <- 
  c("file:///E:/3_rs_data/temperature/OISST/FULL_AV.TEMP.1DAY/SouthAtl/SouthAtl.AV.TEMP.1DAY.time.series.csv",
    "file:///E:/3_rs_data/temperature/OISST/FULL_AV.TEMP.1DAY/NorthAtl/NorthAtl.AV.TEMP.1DAY.time.series.csv",
    "file:///E:/3_rs_data/temperature/OISST/FULL_AV.TEMP.1DAY/MidAtl/MidAtl.AV.TEMP.1DAY.time.series.csv",
    "file:///E:/3_rs_data/temperature/OISST/FULL_AV.TEMP.1DAY/GOM_PCA_chlor_ecoregions-POLY-000000004/GOM_PCA_chlor_ecoregions-POLY-000000004.AV.TEMP.1DAY.time.series.csv")

par(mfrow = c(length(file.in),1))

for (z in 1:length(file.in)) {
  
  data.in <- read.csv(file.in[z])
  data.in$loess.val <- predict(loess(data.in$Value ~ data.in$Julian.Day, span = 0.015))
  
  spring.temp <- data.in$Climatology[intersect(which(data.in$Month == 5), which(data.in$Day == 1))[1]]
  un.year <- unique(data.in$Year)[-1]
  spring.onset <- c()
  for (j in 1:length(un.year)) {
    ind <- which(data.in$Year == un.year[j])
    sub.dat <- data.in[ind,]
    start.ind <- which.min(sub.dat$loess.val[1:which.max(sub.dat$loess.val)])
    sub.dat <- sub.dat[-c(1:start.ind),]
    spring.onset[j] <- sub.dat$Julian.Date[which(sub.dat$loess.val > spring.temp)[1]]
  }
  
  
  lab <- strsplit(file.in, ".time.series.csv")[[z]]
  lab <- strsplit(lab, "/")
  lab <- lab[[1]][9]
  
  plot(un.year, spring.onset, type = "b", xlab = "Year", ylab = "First Day Spring Temperature is Reached", main = lab)
  onset.dat <- data.frame(Year = un.year, Spring.Day = spring.onset)
  name.out <- paste("spring.onset",strsplit(file.in[z],"/")[[1]][length(strsplit(file.in[z],"/")[[1]])], sep = ".")
  write.csv(onset.dat, file = name.out) }
