#Required
library(ggplot2)
library(reshape)

##Dowload the data from NOAA weather station network
#Davis weather station
if(!file.exists("/tmp/davis_ws.gz")){
	download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/720576-00174-2020.op.gz",destfile="/tmp/davis_ws.gz")
}
dawe<-read.table(gzfile('/tmp/davis_ws.gz'),sep="",skip=1,header=FALSE)[,c(3,4,16,18,19,20)]
dawe$location <- "Davis"

#Culdrose weather station
if(!file.exists("/tmp/culdrose_ws.gz")){
	download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/038090-99999-2020.op.gz",destfile="/tmp/culdrose_ws.gz")
}
cuwe<-read.table(gzfile('/tmp/culdrose_ws.gz'),sep="",skip=1,header=FALSE)[,c(3,4,16,18,19,20)]
cuwe$location <- "Helston"

#Circeo weather station
if(!file.exists("/tmp/pratica_ws.gz")){
	download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/162450-99999-2020.op.gz",destfile="/tmp/pratica_ws.gz")
}
prawe<-read.table(gzfile('/tmp/pratica_ws.gz'),sep="",skip=1,header=FALSE)[,c(3,4,16,18,19,20)]
prawe$location <- "Fondi"

##Reshape the value and convert to SI units
dgs <- rbind.data.frame(dawe,cuwe,prawe)
names(dgs) <- c("date","avgt","maxW","mint","maxt","prcp","location")

dgs$date <- as.Date(as.character(dgs$date),"%Y%m%d")
dgs$avgt <- round((dgs$avgt-32)/9*5,1)
dgs$mint[which(dgs$mint%in%"9999.9")] <- 0
dgs$maxt[which(dgs$maxt%in%"9999.9")] <- 0
dgs$mint <- round((as.numeric(dgs$mint)-32)/9*5,1)
dgs$maxt <- round((as.numeric(dgs$maxt)-32)/9*5,1)
dgs$maxW[which(dgs$maxW%in%"999.9")] <- 0
dgs$maxW <- round(dgs$maxW*3.6,1)
dgs$prcp[which(dgs$prcp%in%"99.99")] <- 0
dgs$prcp <- round(as.numeric(gsub("[A-Z]","",dgs$prcp))*25.4,2)

##Visualization
#Long format for ggplot
dgsm<-melt(dgs,id.vars=c("date","location"))
dgsm$value<-as.numeric(dgsm$value)

##Trend of daily values
ggplot(dgsm,aes(x=date,y=value,col=location)) +
geom_line() +
facet_wrap(~variable,ncol=2,scale="free_y")


