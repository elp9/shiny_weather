#Data pre-processing
#Required
library(shiny)
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

## Graphical interface
# Define UI for miles per gallon app ----
ui <- fluidPage(

  # App title ----
	titlePanel("Compare the weather"),

  # Sidebar layout with input and output definitions ----
	sidebarLayout(

    # Sidebar panel for inputs ----
		sidebarPanel(

      # Input: Selector for variable to plot against mpg ----
			selectInput("yvar", "Variable:",
				c("temperature" = "avgt",
					"precipitation" = "prcp"))
			),

    # Main panel for displaying outputs ----
		mainPanel(

      # Output: Formatted text for caption ----
			h3(textOutput("caption")),

      # Output: Plot of the requested variable against mpg ----
			plotOutput("weplot")

			)
		)
	)

## Server calculation
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

	var <- reactive({
		paste(input$yvar)
	})

	output$weplot <- renderPlot({
		g1<-ggplot(dgs[,c(var(),"date","location")],aes(x=date,y=get(var()),col=location)) +
		geom_line()
		print(g1) 
	}
	)
}

shinyApp(ui, server)