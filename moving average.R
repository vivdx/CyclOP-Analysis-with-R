# load packages
library(rgdal)
library(sp)
library(adehabitatLT)

library(RJSONIO) # To parse JSON strings
library(RCurl) # To send GET requests
json <- getURL("http://giv-cyclop.uni-muenster.de/rest/index.php/measurements/after/2012-01-01")
x <- fromJSON(json) # Converts it to an R list

y<-matrix (x$features[[1]]$geometry$coordinates)
coordx<- (c(y[1,1]))
coordy<- (c(y[2,1]))
coordz<- (c(y[3,1]))

t<-matrix (x$features[[1]]$properties)
timedata<- (c(t[10,1]))


l<-length(x$features)
i<-1

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
timedata

x<-xy[,1]
x<-data.frame(x)
y<-xy[,2]
y<-data.frame(y)

MA <- function(x,n){
  filter(x,rep(1/n,n), sides=2)
}
xy5 = MA(xy,5)


#Remove NA from xy5
xy5<-na.omit(xy5)
xy5<-data.frame(xy5)

############connect to server
library(RPostgreSQL)
## loads the PostgreSQL driver 
drv <- dbDriver("PostgreSQL")
## Open a connection
con <- dbConnect(drv, user = "analysis", password="cyclop-analysis", dbname="cyclop", port="5432", host="giv-cyclop.uni-muenster.de")

l1<-nrow(xy5)
check = 0
i<-1
z<-1
repeat {
  check<-check+1
  i<-i+1
  ij<-xy5[i,1]
  z<-z+1
  zf<-xy5[z,2]
  if (check < l1) 
    dbSendQuery(con, paste("insert into analysis_gps_data (ma_x_coordinates,ma_y_coordinates) VALUES ( '",ij,"','",zf,"');",sep="")) 
  dbReadTable(con, "analysis_gps_data") 
  if (check> l1) break
}

