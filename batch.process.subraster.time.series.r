# batch.create.subraster.shp

rast.dir <- c("E://3_rs_data//chlorophyll//MODIS//1_mon_dat//FULL_MO.CHLR.MNTH",
              "E://3_rs_data//chlorophyll//MODIS//8_day_dat//FULL_MO.CHLR.8DAY",
              "E://3_rs_data//temperature//ICOADS//FULL_IC.TEMP.1MON",
              "E://3_rs_data//temperature//Modis//1_mon_dat//FULL_MO.TEMP.MNTH",
              "E://3_rs_data//temperature//Modis//8_day_dat//FULL_MO.TEMP.8DAY",
              "E://3_rs_data//temperature//OISST//FULL_AV.TEMP.1DAY",
              "E://3_rs_data//wind//Daily//FULL_NC.UWND.1DAY",
              "E://3_rs_data//wind//Daily//FULL_NC.VWND.1DAY")

shp.in <- c("C://Users/w906282/Desktop/Google Drive/Turner response/Data/Atlantic/MidAtl.shp",
            "C://Users/w906282/Desktop/Google Drive/Turner response/Data/Atlantic/NorthAtl.shp",
            "C://Users/w906282/Desktop/Google Drive/Turner response/Data/Atlantic/SouthAtl.shp")

# Perform FULL extraction
for (q in 1:length(rast.dir)) {
  for (p in 1:length(shp.in)) {
    create.subraster.shp(input.dir = rast.dir[q],
                         ShapeStem = shp.in[p]) }}

dir.shp <- c()
for (j in 1:length(shp.in)) {
  shp.name <- unlist(strsplit(chartr(old = "\\",new = "/",shp.in[j]),"/"))[length(unlist(strsplit(chartr(old = "\\",new = "/",shp.in[j]),"/")))]
  shp.name <- substr(shp.name,1,c(nchar(shp.name) - 4))
  dir.shp <- rbind(cbind(paste(rast.dir,shp.name, sep = "//"),shp.in[j]),dir.shp) }

for (j in 1:dim(dir.shp)[1]) {
  create.time.series(input.dir = dir.shp[j,1],
                     ShapeStem = dir.shp[j,2])  }