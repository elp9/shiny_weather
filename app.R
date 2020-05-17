#Data pre-processing
#Required
library(shiny)
library(ggplot2)
library(reshape)
library(Rmisc)

#####################################
##Preprocessing
##Dowload the data from NOAA weather station network
#Davis weather station
if(file.exists("/tmp/davis_ws.gz")){
	if(format(file.info("/tmp/davis_ws.gz")$mtime,"%Y-%m-%d")==format(Sys.time(), "%Y-%m-%d")){
		message("Data are up to date.\n")
	}else{
		download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/720576-00174-2020.op.gz",destfile="/tmp/davis_ws.gz")
	}
}else{
	download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/720576-00174-2020.op.gz",destfile="/tmp/davis_ws.gz")
}
dawe<-read.table(gzfile('/tmp/davis_ws.gz'),sep="",skip=1,header=FALSE)[,c(3,4,16,18,19,20)]
dawe$location <- "Davis"

#Culdrose weather station
if(!file.exists("/tmp/culdrose_ws.gz")){
	if(format(file.info("/tmp/davis_ws.gz")$mtime,"%Y-%m-%d")==format(Sys.time(), "%Y-%m-%d")){
		message("Data are up to date.\n")
	}else{
		download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/038090-99999-2020.op.gz",destfile="/tmp/culdrose_ws.gz")
	}
}else{
	download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/038090-99999-2020.op.gz",destfile="/tmp/culdrose_ws.gz")
}
cuwe<-read.table(gzfile('/tmp/culdrose_ws.gz'),sep="",skip=1,header=FALSE)[,c(3,4,16,18,19,20)]
cuwe$location <- "Helston"

#Circeo weather station
if(!file.exists("/tmp/pratica_ws.gz")){
	if(format(file.info("/tmp/davis_ws.gz")$mtime,"%Y-%m-%d")==format(Sys.time(), "%Y-%m-%d")){
		message("Data are up to date.\n")
	}else{
		download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/162450-99999-2020.op.gz",destfile="/tmp/pratica_ws.gz")
	}
}else{
	download.file("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/2020/162450-99999-2020.op.gz",destfile="/tmp/pratica_ws.gz")
}

prawe<-read.table(gzfile('/tmp/pratica_ws.gz'),sep="",skip=1,header=FALSE)[,c(3,4,16,18,19,20)]
prawe$location <- "Fondi"

##Reshape values and convert it to SI units
dgs <- rbind.data.frame(dawe,cuwe,prawe)
names(dgs) <- c("date","avgt","maxW","maxt","mint","prcp","location")
dgs$date <- as.Date(as.character(dgs$date),"%Y%m%d")
dgs$avgt <- round((dgs$avgt-32)/9*5,1)
dgs$mint[which(dgs$mint%in%"9999.9")] <- NA
dgs$maxt[which(dgs$maxt%in%"9999.9")] <- NA
dgs$mint <- round((as.numeric(gsub("[\\*]","",as.character(dgs$mint)))-32)/9*5,1)
dgs$maxt <- round((as.numeric(gsub("[\\*]","",as.character(dgs$maxt)))-32)/9*5,1)
dgs$maxW <- round(dgs$maxW*3.6,1)
dgs$prcp[which(dgs$prcp%in%"99.99")] <- NA
dgs$prcp <- round(as.numeric(gsub("[A-Z]","",dgs$prcp))*25.4,2)
#Pretty names
names(dgs)[names(dgs) == "mint"] <- "Minimum Temperature / Temperatura Minima (°C)"
names(dgs)[names(dgs) == "avgt"] <- "Average Temperature / Temperatura Media (°C)"
names(dgs)[names(dgs) == "maxt"] <- "Maximum Temperature / Temperatura Massima (°C)"
names(dgs)[names(dgs) == "prcp"] <- "Precipitation / Precipitazione (mm)"          
names(dgs)[names(dgs) == "maxW"] <- "Maximum wind speed / Velocità massima del vento (m/s)"         

dgs1 <- dgs[,c("Average Temperature / Temperatura Media (°C)","date","location")]
dgs2 <- melt(dgs1,id.vars=c("date","location"))

#################################
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
#tempw <- tempw[-which(tempw$date%in%check$Group.1[which(check$x<1)]),]

#tempw <- tempw[order(tempw$date,decreasing=T),]
tempw$week <- format(tempw$date, "%W")
tempw$day <- as.factor(format(tempw$date, "%a"))
tempw$dayn <- as.factor(format(tempw$date, "%d-%b"))
tempw$day <- factor(tempw$day,levels=unique(tempw$day),ordered=T)
levels(tempw$day) <- paste(paste(levels(tempw$day),c("Dom","Lun","Mar","Mer","Gio","Ven","Sab"),sep="/"),tempw$dayn,sep=" ")

#################################
###Shiny
## Graphical interface
# Define UI for miles per gallon app ----
ui <- fluidPage(
	  # App title ----
	titlePanel("Compare the Weather / Confronta il meteo 2020"),

  # Sidebar layout with input and output definitions ----
	sidebarLayout(

    # Sidebar panel for inputs ----
		sidebarPanel(width = 3,

      # Input: Selector for variable to plot against mpg ----
			selectInput(inputId="yvar", label="Variable / Variabile:",
				choices=list(
					'Temperature/Temperatura' = list("Minimum Temperature / Temperatura Minima (°C)","Average Temperature / Temperatura Media (°C)","Maximum Temperature / Temperatura Massima (°C)"),
					'Precipitation/Precipitazione' = list("Precipitation / Precipitazione (mm)"),
					'Wind speed/Velocità del vento' = list("Maximum wind speed / Velocità massima del vento (m/s)")),
				selected=list("Average Temperature / Temperatura Media (°C)"),
				multiple=T)
			),

    # Main panel for displaying outputs ----
		mainPanel(

      # Output: Plot of the requested variable against mpg ----
			plotOutput("ts",height="2400px")

			)
		)
	)

## Server calculation
# Define server logic to plot various variables against mpg ----
server.plot <- function(input, output) {

	var <- reactive({
		paste(input$yvar)
	})
    #Time series
	output$ts <- renderPlot({

		dgs1 <- dgs[,c(var(),"date","location")]
		dgs2 <- melt(dgs1,id.vars=c("date","location"))
		df_ls <- tempw[,c(var(),"date","location","week","day")]
		df_ls2 <- melt(df_ls,id.vars=c("date","location","week","day"))

		p.ts <- ggplot(dgs2,aes(x=date,y=value,col=location)) +
		geom_point(cex=4,pch=1) +
		geom_line(size=3) +
		facet_wrap(~variable,ncol=1,scale="free_y") +
		labs(x="", y=NULL) +
		guides(colour=guide_legend(title="City / Città")) +
		scale_x_date(date_labels="%d %b",date_breaks="2 week") +
		theme( 
			text = element_text(size=22, colour="black"),
			plot.title = element_text(face="bold", colour="black", size=28, hjust = 0.5),
			strip.text.x = element_text(size = 18, color = "black"),
			legend.key = element_rect(fill='white'),
			legend.position= "top",
			legend.title=element_text(size=30),
			legend.text=element_text(size=25),
			axis.text=element_text(size=20, colour="black"),
			axis.title=element_text(size=20,face="bold", colour="black"),
			panel.grid.major = element_line(colour="gray80"),
			panel.background = element_rect(fill = 'white'))+
		scale_color_manual(values = c("orange1","forestgreen","dodgerblue3"))

		p.lw <- ggplot(df_ls2,aes(x=week, y=day, fill = value)) + 
		geom_tile(colour = "white",show.legend=FALSE) + 
		geom_text(aes(label = value), size=10, fontface = "bold",colour="white") +
		facet_grid(variable~location) + 
		scale_fill_gradient(low="blue", high="red") +
		labs(x="Week of the year / Settimana dell'anno (2020)", y=NULL) +
		theme(legend.title=element_blank(), 
			text = element_text(size=22, colour="black"), 
			plot.title = element_text(face="bold", colour="black", size=28,hjust = 0.5),
			strip.text.x = element_text(size = 25, color = "black"),
			strip.text.y = element_text(size = 25, color = "black"),
			axis.text=element_text(size=18, colour="black"),
			axis.title=element_text(size=20,face="bold", colour="black"),
			panel.grid.major = element_line(colour="gray90"),
			panel.background = element_rect(fill = 'gray90'),
			legend.position= "top"
			)

		mgg <- multiplot(p.ts+ggtitle("\n\nData for 2020 / Dati giornalieri per il 2020"), p.lw+ggtitle("\n\nData from last 7 Days / Dati degli ultimi 7 giorni"), cols=1, rows=2)
		print(mgg)
	})
}

shinyApp(ui, server.plot)