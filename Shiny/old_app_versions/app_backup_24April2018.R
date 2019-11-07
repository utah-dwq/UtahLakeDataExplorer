
#setwd("F:\\Shiny\\UtahLake")
library(lubridate)
library(reshape2)
library(gplots)
library(sciplot)

source("functions/calcTSI.R")
source("functions/plot3dTSI.R")

####Data pre-processing


######
######Need to check unit consistency for desired parameters


#Read in datasets
wq_data=read.csv(file="data/UtahLake_WQ_data.csv")
wq_data$Date=as.Date(wq_data$Activity.Start.Date,format="%m/%d/%Y")
wq_data$Year=year(wq_data$Date)
wq_data$Month=month(wq_data$Date)
dim(wq_data)

lake_elev_data=read.csv(file="data/UL_Elevation_Avg_Monthly.csv")

translate_params=read.csv(file="data/translate_params.csv")

#Subset wq data to final only
wq_data=wq_data[wq_data$QACQ.Status=="Final",]
dim(wq_data)

#Fill blank Sample.Fraction and Activity.Relative.Depth with N/A
levels(wq_data$Sample.Fraction)=c(levels(wq_data$Sample.Fraction),"N/A")
wq_data$Sample.Fraction[wq_data$Sample.Fraction==""]="N/A"
levels(wq_data$Activity.Relative.Depth)=c(levels(wq_data$Activity.Relative.Depth),"N/A")
wq_data$Activity.Relative.Depth[wq_data$Activity.Relative.Depth ==""]="N/A"
wq_data=droplevels(wq_data)

#Merge w/ parameter translation table
wq_data=merge(wq_data,translate_params)

##Split chemistry and profile data (profiles DataLoggerLine !="")
profile_data=wq_data[wq_data$Data.Logger.Line!="",]
wq_data=wq_data[wq_data$Data.Logger.Line=="",]
dim(profile_data)
dim(wq_data)

#Set non-detects to 1/2 detection limit
wq_data$Result.Value[is.na(wq_data$Result.Value)]=wq_data$Detection.Quantitation.Limit.Value1[is.na(wq_data$Result.Value)]/2

#Create trophic data subset for TSI plots
trophic_data=wq_data[
					wq_data$Sample.Fraction=="Total"&
					wq_data$Activity.Relative.Depth=="Surface",
					]
trophic_data=trophic_data[
					trophic_data$Parameter=="Phosphate-phosphorus"|
					trophic_data$Parameter=="Chlorophyll a"
					,]
trophic_data=trophic_data[trophic_data$Result.Unit!="mg",]
secchi=wq_data[wq_data$Parameter=="Depth, Secchi disk depth",]
trophic_data=rbind(trophic_data,secchi)

summary(trophic_data)
	#dropped chl a w/ Result.Unit=="mg", subsetting to surface only
	#In future, may want to build in unit conversions, units in current data set consistent (other than 2 samples w/ chl a mg issue)
dim(trophic_data)
				
#Calculate TSI values for trophic_data
trophic_data$Result.Value[trophic_data$Result.Value==0]=0.01 #Set 0 values to 0.01 for TSI calcs
trophic_data=trophic_data[,c("Date","Monitoring.Location.ID","Parameter","Result.Value","Year","Month")] #Subset columns
dim(trophic_data)
trophic_data=unique(trophic_data) #Remove any duplicates
dim(trophic_data)
	#cast to matrix
trophic_data=dcast(trophic_data,Date+Monitoring.Location.ID+Year+Month~Parameter,value.var="Result.Value",fun.aggregate=mean) #Found 4 samples on same site/day July 31, 1991 - all others unique (after removing dups)
	#calculate TSIs
TSI=calcTSI(trophic_data,in_format="matrix",chl="Chlorophyll a",TP="Phosphate-phosphorus",SD="Depth, Secchi disk depth") 
trophic_data=cbind(trophic_data,TSI)
summary(trophic_data)


# Define UI for app ----
ui <- fluidPage(
   
	tags$style("
		body {
		-moz-transform: scale(0.85, 0.85); /* Moz-browsers */
		zoom: 0.85; /* Other non-webkit browsers */
		zoom: 85%; /* Webkit browsers */
		}"
	),
	
	headerPanel( title=div(img(src="deq_dwq_logo1.png", height = 125, width = 125*2.89/1.47)),
		windowTitle="Utah Lake data explorer"),
	
	tabsetPanel(id="tabs",
		tabPanel("Lake elevation",value=1),
		tabPanel("Water chemistry",value=2),
		tabPanel("Trophic state",value=3),
		tabPanel("NLA comparison",value=4),
		tabPanel("Water quality map",value=5),
		tabPanel("Lake profiles",value=6)
	),

		
	#App title ----
	titlePanel("",
		tags$head(tags$link(rel = "icon", type = "image/png", href = "dwq_logo_small.png"),
		tags$title("Utah Lake data explorer"))
	),
	

	sidebarLayout(
		sidebarPanel(
			###Help texts
			conditionalPanel(condition="input.tabs==1",helpText("This tool shows long-term and seasonal variation in Utah Lake surface water elevations by plotting against year and month.")),
			conditionalPanel(condition="input.tabs==2",helpText("Tool description.")),
			conditionalPanel(condition="input.tabs==3",helpText("This tool plots Carlson TSI values for Utah Lake as time series, boxplots, or pairwise comparisons following Havens and Carlson (2011).")),
			conditionalPanel(condition="input.tabs==4",helpText("Tool description.")),
			conditionalPanel(condition="input.tabs==5",helpText("Tool description.")),
			conditionalPanel(condition="input.tabs==6",helpText("Tool description.")),
			br(),

			###Global inputs
			sliderInput(inputId="plot_years","Year range:",min=min(lake_elev_data$Year),max=max(lake_elev_data$Year),value=c(1990,max(lake_elev_data$Year)),sep=""),
			conditionalPanel(
				condition="input.tabs!=1",
				sliderInput(inputId="plot_months","Month range:",min=min(lake_elev_data$Month),max=max(lake_elev_data$Month),value=c(min(lake_elev_data$Month),max(lake_elev_data$Month)),sep="",step=1),
				checkboxGroupInput("include","Include:",choices=c("Utah Lake","Provo Bay"),selected=c("Utah Lake","Provo Bay"))
			),

			###Lake elevation inputs:
			conditionalPanel(
				condition="input.tabs==1",
						radioButtons(inputId = "elev_rulers",
							label = "Rulers:",
							choiceNames=c("On","Off"),
							choiceValues=c(1,0),selected=0,inline=TRUE),
						conditionalPanel(
							condition="input.elev_rulers==1",
							sliderInput(inputId="elev_ruler1","Elevation ruler 1:",
								min=floor(min(lake_elev_data$Elevation..ft.)),
								max=ceiling(max(lake_elev_data$Elevation..ft.)),
								value=4489,
								sep=""
							),
							sliderInput(inputId="elev_ruler2","Elevation ruler 2:",
								min=floor(min(lake_elev_data$Elevation..ft.)),
								max=ceiling(max(lake_elev_data$Elevation..ft.)),
								value=4489,
								sep=""
							)
						)
			),
			

			####Water chemistry tab:
			conditionalPanel(
				condition="input.tabs==2",
						radioButtons(inputId="chem_plot_type",label="Plot type:",
									choiceNames=c("Time series","Boxplot","Pairwise"),
									choiceValues=c(1,2,3),selected=1,inline=TRUE),
				conditionalPanel(condition="input.chem_plot_type==3",
					h4("Parameter 1:")
				),
				uiOutput("chem_param1"),
				uiOutput("chem_frac1"),
				uiOutput("reldepth1"),
				
				conditionalPanel(				
					condition="input.chem_plot_type==3",
					h4("Parameter 2:"),
					uiOutput("chem_param2"),
					uiOutput("chem_frac2"),
					uiOutput("reldepth2")
				)
			),

			####TSI tab:
			conditionalPanel(
				condition="input.tabs==3",
					radioButtons(inputId="TSI_plot_type",label="Plot type:",
									choiceNames=c("Time series","Boxplot","Pairwise"),
									choiceValues=c(1,2,3),selected=1,inline=TRUE)
					),
			br(),
			helpText("For help with this tool, or to report a bug, please contact Jake Vander Laan, UDWQ, jvander@utah.gov, (801) 536-4350.")

		),
		
		
		
		mainPanel(
			conditionalPanel(
				condition="input.tabs==1",
				plotOutput(outputId = "elev_plot",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==3",
				plotOutput(outputId = "tsi_plot",width="800px",height="800px")
			)
		)
		
	)

)



server <- function(input, output){

	#Reactive inputs:
	
	output$chem_param1 <- renderUI({
		choice_data=wq_data[
								wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
								wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]
							  ,]
		param_choices=unique(choice_data[,"Parameter"])
		req(param_choices)
		selectInput("chem_param1", "Parameter:", param_choices)
	})
    
	output$chem_param2 <- renderUI({
		choice_data=wq_data[
								wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
								wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]
							  ,]
		param_choices=unique(choice_data[,"Parameter"])
		selectInput("chem_param2", "Parameter:", param_choices)
	})
    
	output$chem_frac1 <- renderUI({
		req(input$chem_param1)
		choice_data=wq_data[
								wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
								wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]&
								wq_data$Parameter==input$chem_param1
							  ,]
		frac_choices=unique(choice_data[,"Sample.Fraction"])
		selectInput("chem_frac1", "Fraction:", frac_choices)
	})
	
	output$reldepth1 <- renderUI({
		req(input$chem_frac1)
		choice_data=wq_data[
								wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
								wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]&
								wq_data$Parameter==input$chem_param1&
								wq_data$Sample.Fraction==input$chem_frac1
							  ,]
		reldep_choices=unique(choice_data[,"Activity.Relative.Depth"])
		selectInput("reldepth1", "Sample depth:", reldep_choices)
	})

	output$chem_frac2 <- renderUI({
		req(input$chem_param2)
		choice_data=wq_data[
								wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
								wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]&
								wq_data$Parameter==input$chem_param2
							  ,]
		frac_choices=unique(choice_data[,"Sample.Fraction"])
		selectInput("chem_frac2", "Fraction:", frac_choices)
	})
	
	output$reldepth2 <- renderUI({
		req(input$chem_frac2)
		choice_data=wq_data[
								wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
								wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]&
								wq_data$Parameter==input$chem_param2&
								wq_data$Sample.Fraction==input$chem_frac2
							  ,]
		reldep_choices=unique(choice_data[,"Activity.Relative.Depth"])
		selectInput("reldepth2", "Sample depth:", reldep_choices)
	})
	
	
	

	#Tab 1: Elevation plot outputs
	output$elev_plot=renderPlot({
		par(mfrow=c(2,1),mar=c(4.1,6.1,2.1,4.1))
		
		elev_plot_data=lake_elev_data[lake_elev_data$Year>=input$plot_years[1]&lake_elev_data$Year<=input$plot_years[2],]
		suppressWarnings(
			plotmeans(Elevation..ft.~Year,elev_plot_data,n.label=F,barcol="blue",barwidth=2,lwd=2,col="blue",pch=19,cex=2,
			ylab="Lake elevation (ft)",xlab="Year",cex.lab=2,cex.axis=2)
		)
		if(input$elev_rulers==1){
			abline(h=input$elev_ruler1,lwd=2,lty=2,col="orange")
		}
		suppressWarnings(
			plotmeans(Elevation..ft.~Month,elev_plot_data,n.label=F,barcol="blue",barwidth=2,lwd=2,col="blue",pch=19,cex=2,
			ylab="Lake elevation (ft)",xlab="Month",cex.lab=2,cex.axis=2)
		)
		if(input$elev_rulers==1){
			abline(h=input$elev_ruler2,lwd=2,lty=2,col="orange")
		}
	})

	
	

	
	
	
	
	
	
	#Tab 3: TSI plot outputs
	output$tsi_plot=renderPlot({
		if(length(input$include)==0){include=""
			}else{
				if(length(input$include)==2){include=unique(trophic_data$Monitoring.Location.ID)
					}else{
						if(input$include[1]=="Utah Lake"){include=unique(trophic_data$Monitoring.Location.ID[trophic_data$Monitoring.Location.ID!=4917450&trophic_data$Monitoring.Location.ID!=4917470])}
						if(input$include[1]=="Provo Bay"){include=c(4917450,4917470)}
					}
				}

		tsi_plot_data=trophic_data[
								trophic_data$Year>=input$plot_years[1]&trophic_data$Year<=input$plot_years[2]&
								trophic_data$Month>=input$plot_months[1]&trophic_data$Month<=input$plot_months[2]&
								trophic_data$Monitoring.Location.ID%in%include
								,]
								
		
		TSI_flat=tsi_plot_data[,c("Date","Monitoring.Location.ID","Year","Month","TSIchl","TSItp","TSIsd")]
		TSI_flat=melt(TSI_flat,id.vars=c("Date","Monitoring.Location.ID","Year","Month"))

		if(dim(tsi_plot_data)[1]>0){
		
			#Time series
			if(input$TSI_plot_type==1){
				par(mar=c(20,6,1,1))
				suppressWarnings(lineplot.CI(Year,value,group=variable,data=TSI_flat,legend=F,pch=c(NA,NA,NA),err.width=0.05,err.col=NA,type='p',xlab="Year",ylab="TSI",x.cont=TRUE,xaxt='n',cex.axis=1.5,cex.lab=2))
				agg=aggregate(value~Year+variable,TSI_flat,FUN='mean')
				lines(value~Year,agg[agg$variable=="TSIchl",],col="green",lwd=2)
				lines(value~Year,agg[agg$variable=="TSIsd",],col="blue",lwd=2)
				lines(value~Year,agg[agg$variable=="TSItp",],col="orange",lwd=2)
				suppressWarnings(lineplot.CI(as.factor(Year),value,group=variable,data=TSI_flat,legend=F,pch=c(21,21,21),col=c("green","orange","blue"),err.width=0.05,type='p',add=TRUE,xaxt='n',yaxt='n',lwd=2,x.cont=TRUE,cex=2))
				axis(1,cex.axis=1.5,cex.lab=2)
				legend("bottomright",pch=21,col=c("blue","orange","green"),legend=c("Secchi TSI","TP TSI","ChlA TSI"),lty=c(1,1,1),lwd=2,bty='n',cex=2)
			}
			
			#Boxplot
			if(input$TSI_plot_type==2){
				par(mar=c(6,6,1,1))
				boxplot(tsi_plot_data$TSIchl,tsi_plot_data$TSItp,tsi_plot_data$TSIsd,border=c("green","orange","blue"),
						names=c("ChlA","TP","Secchi"),ylab="TSI",cex.axis=2,cex.lab=2.5,lwd=2)
			}
		
			#Pair-wise
			if(input$TSI_plot_type==3){
				par(mar=c(6,6,1,1))
				plot3dTSI(tsi_plot_data)
			}
			
		}
		
	})
	
	


}


shinyApp(ui = ui, server = server)









