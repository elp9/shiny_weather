#Data pre-processing
#Required
library(ggplot2)
library(reshape)

##Preprocessing
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
names(dgs) <- c("date","avgt","maxW","maxt","mint","prcp","location")

dgs$date <- as.Date(as.character(dgs$date),"%Y%m%d")
dgs$avgt <- round((dgs$avgt-32)/9*5,1) #°C
dgs$mint[which(dgs$mint%in%"9999.9")] <- NA
dgs$maxt[which(dgs$maxt%in%"9999.9")] <- NA
dgs$mint <- round((as.numeric(gsub("[\\*]","",as.character(dgs$mint)))-32)/9*5,1) #°C
dgs$maxt <- round((as.numeric(gsub("[\\*]","",as.character(dgs$maxt)))-32)/9*5,1) #°C
dgs$maxW <- round(dgs$maxW*3.6,1) #m/s
dgs$prcp[which(dgs$prcp%in%"99.99")] <- NA
dgs$prcp <- round(as.numeric(gsub("[A-Z]","",dgs$prcp))*25.4,2)

dgs1 <- dgs[,c("Average Temperature (ºC)","date","location")]
dgs2 <- melt(dgs1,id.vars=c("date","location"))

###################
#Derive historical statistics
##Yesterday
yesterday <- as.Date(format(Sys.time(), '%Y-%m-%d'))-1
lastweek <- seq(yesterday-6,yesterday,"1 days")

#Yesterday data
#datayes <- dgs[which(dgs$date%in%yesterday),]
#datalastw <- aggregate(.~location,tempw[,-c(1)],"mean")
#Last week data
tempw <- dgs[which(dgs$date%in%lastweek),]
check <- aggregate(tempw$date,by=list(tempw$date),"length")
tempw <- tempw[-which(tempw$date%in%check$Group.1[which(check$x<3)]),]

#tempw <- tempw[order(tempw$date,decreasing=T),]
tempw$week <- format(tempw$date, "%W")
tempw$day <- as.factor(format(tempw$date, "%A"))
tempw$day <- factor(tempw$day,levels=unique(tempw$day),ordered=T)
##################

#Pretty names
names(dgs)[names(dgs) == "mint"] <- "Minimum Temperature (ºC)"
names(dgs)[names(dgs) == "avgt"] <- "Average Temperature (ºC)"
names(dgs)[names(dgs) == "maxt"] <- "Maximum Temperature (ºC)"
names(dgs)[names(dgs) == "prcp"] <- "Precipitation (mm)"         
names(dgs)[names(dgs) == "maxW"] <- "Maximum wind speed (m/s)"         

g2<-ggplot(dgs2,aes(x=date,y=value,col=location)) +
geom_line() +
facet_wrap(~variable,ncol=1) +
xlab("Date") +
ylab("Value") +
theme(legend.title=element_blank(), 
	text = element_text(size=22), 
	legend.key = element_rect(fill='white'), 
	axis.text=element_text(size=18),
	axis.title=element_text(size=20,face="bold"),
	panel.grid.major = element_line(colour="gray80"),
	panel.background = element_rect(fill = 'white'))+
scale_color_manual(values = c("orange1","forestgreen","dodgerblue3")) 
print(g2)

ggplot(tempw,aes(x=week, y=day, fill = `Average Temperature (ºC)`)) + 
    geom_tile(colour = "white") + 
    geom_text(aes(label = `Average Temperature (ºC)`), size=10) +
    facet_grid(~location) + 
    scale_fill_gradient(low="yellow", high="red") +
    labs(x="Week of Month", y=NULL)