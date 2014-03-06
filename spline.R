library(rgdal)
library(sp)
library(adehabitatLT)
library(adehabitat)

library(RJSONIO) # To parse JSON strings
library(RCurl) # To send GET requests
json <- getURL("http://giv-cyclop.uni-muenster.de/rest/index.php/measurements/after/2012-01-01")
x <- fromJSON(json) # Converts it to an R list
x$features[[1]]$properties
#y<-matrix (x$features[[1]]$geometry)
#coord<- (c(y[2,1]))

l<-length(x$features)
i<-1

y<-matrix (x$features[[1]]$geometry$coordinates)
coordx<- (c(y[1,1]))
coordy<- (c(y[2,1]))
coordz<- (c(y[3,1]))

t<-matrix (x$features[[1]]$properties)
timedata<- (c(t[10,1]))

repeat {
  i<-i+1
  if (i <= l-1) 
    y<-matrix (x$features[[i]]$geometry$coordinates)
  coordx<- (c(coordx,y[1,1]))
  coordy<- (c(coordy,y[2,1]))
  coordz<- (c(coordz,y[3,1]))
  t<-matrix (x$features[[i]]$properties)
  timedata<- (c(timedata,t[10,1]))
  timedata 
  if (i> l-1) break
}

coordxyz<-data.frame(coordx,coordy,coordz)
xy<-coordxyz

timedata<-data.frame(timedata)
#timedata

x<-xy[,1]
x<-data.frame(x)
y<-xy[,2]
y<-data.frame(y)


#gpspts = gpspts[1:5]
#gpspts$time = as.POSIXct(gpspts$time)
#gpspts = gpspts[-which(duplicated(gpspts$time)),] 
#gpspts <- gpspts[1:297,]
#gpspts = spTransform(gpspts, CRS=CRS("+init=epsg:31467"))

#gpspts.df <- as.data.frame(gpspts)#Casting
#gpspts.df$id <- c(1:1:nrow(gpspts.df))#Adding an Id
#xy <- cbind(gpspts.df$id,gpspts.df$coords.x1,gpspts.df$coords.x2)    
spline.x <- smooth.spline(x, df=100)                       
spline.y <- smooth.spline(y, df=100)                        
x.pred <- predict(spline.x, xy[,1])                                  
y.pred <- predict(spline.y, xy[,1])                                 

xy.pred <- data.frame(x.pred$y,y.pred$y)


plot(xy.pred)
xy.pred
sx<-x.pred$y
sy<-y.pred$y


#########load Data in PostgreSQL
library(RPostgreSQL)
## loads the PostgreSQL driver 
drv <- dbDriver("PostgreSQL")
## Open a connection
con <- dbConnect(drv, user = "analysis", password="cyclop-analysis", dbname="cyclop", port="5432", host="giv-cyclop.uni-muenster.de")


##dbExistsTable(con, TableName, ...) checks whether a particular table exists on the given connection. Returns a logical
dbExistsTable(con,"analysis_gps_data")
l1<-nrow(xy.pred)
check = 0
i<-1
z<-1
repeat {
  check<-check+1
  i<-i+1
  sx<-xy[i,1]
  z<-z+1
  sy<-xy[z,2]
  if (check < l1) 
    dbSendQuery(con, paste("insert into analysis_gps_data (spline_x_coordinates,spline_y_coordinates,time_stamp,) VALUES ( '",sx,"','",sy,"','",t,"');",sep=""))
  dbReadTable(con, "analysis_gps_data") 
  if (check> l1) break
}


#dbSendQuery(con, paste("insert into analysis_gps_data (spline_x_coordinates,spline_y_coordinates,time_stamp,) VALUES ( '",sx,"','",sy,"','",t,"');",sep=""))

