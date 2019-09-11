extract.short.format.copepod <- function(target.dir = "C:/Users/w906282/Desktop/short format",
                                         shape.file = "file:/C:/Users/w906282/Dropbox/shp.files/GOM PCA-derived ecoregions/GOM_PCA_chlor_ecoregions-POLY-000000004.shp") {
  
  require(rgeos)
  require(sp)
  require(rgdal)
  require(maptools)
  require(dplyr)
  
  files.in <- dir(target.dir, pattern = ".csv")
  files.in <- paste(target.dir, files.in, sep = "/")
  
  full.data.df <- c()
  for (j in 1:length(files.in)) {
  
    data.mat <- read.csv(files.in[j],skip = 17, header = F)
    data.df  <- data.frame(ID = data.mat[,1],
                           SOURCE = data.mat[,38],
                           Y = data.mat[,2],
                           M = data.mat[,3],
                           D = data.mat[,4],
                           Latitude = data.mat[,7],
                           Longitude = data.mat[,8],
                           SET.V = data.mat[,24])
    
    full.data.df <- rbind(data.df, full.data.df) }
  
red.full.data.df <- full.data.df[,c('Longitude', 'Latitude', '')]
full.data.df$names <- as.character(seq(1,dim(full.data.df)[1]))

full.df.red <- full.data.df[,c(7,6,9)]
coordinates(full.df.red) <-  ~Longitude + Latitude
proj4string(full.df.red) <- proj4string(readShapePoly(shape.file))
ind <- which(complete.cases(over(full.df.red, readShapePoly(shape.file))))

full.data.df <- full.data.df[ind,]

sum.dat <- as.data.frame(full.data.df %>% group_by(Y, M) %>% summarise(Set.Vol = mean(SET.V)))
return(sum.dat)

}