library(RJSONIO) # To parse JSON strings
library(RCurl) # To send GET requests
json <- getURL("http://giv-cyclop.uni-muenster.de/rest/index.php/measurements/after/2012-01-10")
x <- fromJSON(json) # Converts it to an R list
x$features[[1]]$properties
y<-matrix (x$features[[1]]$properties)
ndata<- (c(y[6,1]))
ndata

midata<- (c(y[1,1]))
midata

l<-length(x$features)

i<-1

repeat {
  i<-i+1
  if (i <= l) 
    y<-matrix (x$features[[i]]$properties)
  ndata<- (c(ndata,y[7,1]))
  ndata
  midata<- (c(midata,y[1,1]))
  midata
  if (i> l) break
}


n<-matrix(ndata)
n
mid<-matrix(midata) 
mid

table<-data.frame(n)
table

#mean of no2 table (all values)
x<-as.numeric(c(n))
x
mean<-mean(x)

#standard deviation
sd(x)
sd<-sd(x)

#variane
var(x) 
v<-var(x)


#Time and date
time<-format(Sys.time(), "%a %b %d %X %Y %Z")
time


#########load Data in PostgreSQL
library(RPostgreSQL)
## loads the PostgreSQL driver 
drv <- dbDriver("PostgreSQL")
## Open a connection
con <- dbConnect(drv, user = "analysis", password="cyclop-analysis", dbname="cyclop", port="5432", host="giv-cyclop.uni-muenster.de")


##dbExistsTable(con, TableName, ...) checks whether a particular table exists on the given connection. Returns a logical
dbExistsTable(con,"analysis_no2")

dbSendQuery(con, paste("insert into analysis_no2 (standard_deviation,time,variance,mean,id) VALUES ( '",sd,"','",time,"','",v,"','",mean,"','",mid,"');",sep=""))
dbReadTable(con, "analysis_no2")