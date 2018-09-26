
#setwd("F:\\Shiny\\UtahLakeDataExplorer")
library(lubridate)
library(reshape2)
library(gplots)
library(sciplot)
library(sp)
library(sf)
library(leaflet)
library(RColorBrewer)
library(raster)
library(rgdal)
library(gdalUtils)
library(mapview)
library(Matrix)
library(gdistance)
library(ipdw)
library(scales)

source("functions/calcTSI.R")
source("functions/plot3dTSI.R")
source("functions/ipdwMap.R")

options(scipen=999)


######
######Double check for unit consistency - looks good currently - consider building autocheck or conversion feature if necessary in the future

####Data pre-processing

#Provisional data rejected
#Non-detects at ½ sample detection limit
#All chla included (corrected, uncorrected, and unspecified all translated to Chlorophyll a)
#Samples of chemical parameters missing fraction or depth not included.
#ChlA and TP TSI analyses conducted only on total/surface samples.
#Fraction == Acid Soluble = Total
#All sample depths for Secchi set to N/A
#All pH fractions set to N/A
#All total dissolved solids fractions marked dissolved
#TFS, TSS, and TVS fractions all marked total
#Turbidity fractions all marked N/A
#Settleable solids fraction marked total.
#All fecal coliform sample depths assumed surface.

#Read in datasets
wq_data=read.csv(file="data/UtahLake_WQ_data.csv")
wq_data$Date=as.Date(wq_data$Activity.Start.Date,format="%m/%d/%Y")
wq_data$Year=year(wq_data$Date)
wq_data$Month=month(wq_data$Date)
#wq_data$Monitoring.Location.ID=as.factor()
dim(wq_data)

lake_elev_data=read.csv(file="data/UL_Elevation_Avg_Monthly.csv")

translate_params=read.csv(file="data/translate_params.csv")

nla_data=read.csv(file="data/NLA2012_TrophicData.csv")

ul_poly=st_read("polyrast","UtahLake_poly_wgs84")
ul_poly=st_zm(ul_poly) #Removing 'z' coordinates for leaflet







#Phytoplankton pre-processing
phyto_data=read.csv(file="data/phytoplankton.csv")
#phyto_data$CellperML=as.numeric(phyto_data$CellperML)
phyto_data$Date2=as.Date(phyto_data$Date,format="%m/%d/%Y")
phyto_data$Year=year(phyto_data$Date2)
phyto_data$Month=month(phyto_data$Date2)
phyto_data=phyto_data[phyto_data$SampleType!="",]
levels(phyto_data$SampleType)=c(levels(phyto_data$SampleType),"Total phytoplankton")
phyto_data$SampleType[phyto_data$SampleType=="Total Phytoplankton" | phyto_data$SampleType=="Total Plankton Sample"]="Total phytoplankton"
head(phyto_data)
phyto_data$Division=as.character(phyto_data$Division)
phyto_data$Division[is.na(phyto_data$Division) | phyto_data$Division==""]="Other"
phyto_data$Genus=as.character(phyto_data$Genus)
phyto_data$Genus[is.na(phyto_data$Genus) | phyto_data$Genus==""]=NA







#Subset wq data to final only
wq_data=wq_data[wq_data$QACQ.Status=="Final",]
dim(wq_data)

#Merge w/ parameter translation table
wq_data=merge(wq_data,translate_params,all.x=T)
dim(wq_data)

#Drop samples where fraction or depth are blank
wq_data=wq_data[wq_data$Fraction!=""&wq_data$Depth!="",]
dim(wq_data)

##Split chemistry and profile data (profiles have DataLoggerLine !="")
profile_data=wq_data[wq_data$Data.Logger.Line!="",]
wq_data=wq_data[wq_data$Data.Logger.Line=="",]
dim(profile_data)
dim(wq_data)

#Set non-detects to 1/2 detection limit
wq_data$Result.Value[is.na(wq_data$Result.Value)]=wq_data$Detection.Quantitation.Limit.Value1[is.na(wq_data$Result.Value)]/2

#Calculate N:P ratios
np_flat=wq_data[wq_data$Parameter=="Nitrogen" | wq_data$Parameter=="Phosphate-phosphorus",]
np_mat=dcast(np_flat, Date+Year+Month+Fraction+Depth+Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Result.Unit~Parameter, value.var="Result.Value", fun.aggregate=mean) #Handful of duplicate value uploads under different activity IDs - requires aggregation function
np_mat$NP_mass=np_mat$Nitrogen/np_mat[,"Phosphate-phosphorus"]
np_mat$NP_mol=np_mat$NP_mass*(30.974/14.007)
#write.csv(file="data//np_matrix.csv",np_mat,row.names=F)

#Flatten N:P ratios
np_flat=na.omit(melt(np_mat,id.vars=c("Date","Year","Month","Fraction","Depth","Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude","Result.Unit"),measure.vars=c("NP_mass","NP_mol"),variable.name="Parameter",value.name="Result.Value"))
levels(np_flat$Parameter)=c(levels(np_flat$Parameter),"N:P ratio (mass)","N:P ratio (molar)")
np_flat$Parameter[np_flat$Parameter=="NP_mass"]="N:P ratio (mass)"
np_flat$Parameter[np_flat$Parameter=="NP_mol"]="N:P ratio (molar)"

#Append N:P ratios back to wq_dat
wq_data=wq_data[,names(np_flat)]
dim(wq_data)
wq_data=rbind(wq_data,np_flat)
dim(wq_data)


#Create trophic data subset for TSI plots (total & surface factors only)
trophic_data=wq_data[
					wq_data$Fraction=="Total"&
					wq_data$Depth=="Surface",
					]
trophic_data=trophic_data[
					trophic_data$Parameter==as.character("Phosphate-phosphorus")|
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

TSI=calcTSI(nla_data,in_format="matrix",chl="Chlorophyll_a_ugL",TP="Total_phosphorus_mgL",SD="Secchi_m") 
nla_data=cbind(nla_data,TSI)
head(nla_data)

#Define NLA comparison parameter choices:
nla_comp_choices=c("Phosphate-phosphorus","Chlorophyll a","Depth, Secchi disk depth","Total_nitrogen_mgL")
names(nla_comp_choices)=c("Total phosphorus (mg/L)","Chlorophyll a (ug/L)","Secchi depth (m)","Total nitrogen (mg/L)")


# Define UI for app ----
ui <- fluidPage(
   
	tags$style("
		body {
		-moz-transform: scale(0.85, 0.85); /* Moz-browsers */
		zoom: 0.85; /* Other non-webkit browsers */
		zoom: 85%; /* Webkit browsers */
		}"
	),
	tags$style(type = "text/css", "html, body {width:100%;height:100%}",
		".leaflet .legend i{
		border-radius: 50%;
		width: 10px;
		height: 10px;
		margin-top: 4px;
		}
		"
	),
	headerPanel( title=div(img(src="deq_dwq_logo1.png", height = 125, width = 125*2.89/1.47)),
		windowTitle="Utah Lake data explorer"),
	
	tabsetPanel(id="tabs",
		tabPanel("Lake elevation",value=1),
		tabPanel("Water chemistry",value=2),
		tabPanel("Trophic state",value=3),
		tabPanel("NLA comparison",value=4),
		tabPanel("Water quality map",value=5),
		tabPanel("Phytoplankton",value=6)
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
			conditionalPanel(condition="input.tabs==2",helpText("This tool shows time series (yearly & monthly), boxplots by site, and pairwise scatter plots of selected water quality parameters .")),
			conditionalPanel(condition="input.tabs==3",helpText("This tool plots Carlson TSI values for Utah Lake as time series, boxplots, or pairwise comparisons following Havens and Carlson (2011).")),
			conditionalPanel(condition="input.tabs==4",helpText("This tool allows comparison of nutrient and chlorophyll a relationships between Utah Lake and EPA's National Lakes Assessment (NLA, 2012).")),
			conditionalPanel(condition="input.tabs==5",
				helpText("This tool estimates lake-wide values for selected parameters, fractions, depths, and time periods via inverse path distance weighted interpolation."),
				helpText("Please be patient - this type of interpolation accounts for land-barriers between sites, but is  data intensive and therefore slow."),
				helpText("Select desired parameters, then click 'Interpolate' to generate the map."),
				helpText("Numbers plotted on the map show available sample sizes at each site used for interpolation.")
			),
			conditionalPanel(condition="input.tabs==6 & input.phyto_plot_type==1",
				helpText("This tool shows long-term and seasonal variation in Utah Lake phytoplankton abundances and relative abundances against year and month."),
				helpText("HAB sample types were only analyzed for potentially harmful taxa. Total phytoplankton sample types were analyzed for all taxa present.")
			),	
			conditionalPanel(condition="input.tabs==6 & input.phyto_plot_type==2",
				helpText("This tool shows abundances or relative abundances of selected phytoplankton at sampling locations over the selected years & months."),
				helpText("Marker sizes are scaled by the average abundance or relative abundance observed at each location."),
				helpText("Click on a marker to see the monitoring location ID and value associated with that point (rounded to 2 significant digits)."),
				helpText("HAB sample types were only analyzed for potentially harmful taxa. Total phytoplankton sample types were analyzed for all taxa present."),
				helpText("Dynamic z-scaling scales marker sizes based on selected data. Fixed z-scaling defines a single scale of marker sizes for all plots; enabling visual comparisons among different taxa or time periods.")
			),

			
			#conditionalPanel(condition="input.tabs==6",helpText("Tool description.")),
			br(),

			###Global inputs
			conditionalPanel(
				condition="input.tabs!=1 & input.tabs!=6",			
				sliderInput(inputId="plot_years","Year range:",min=min(wq_data$Year),max=max(wq_data$Year),value=c(1990,max(wq_data$Year)),sep="")
			),
			conditionalPanel(
				condition="input.tabs==1",			
				sliderInput(inputId="elev_plot_years","Year range:",min=min(lake_elev_data$Year),max=max(lake_elev_data$Year),value=c(min(lake_elev_data$Year),max(lake_elev_data$Year)),sep="")
			),
			
			conditionalPanel(
				condition="input.tabs!=1 & input.tabs!=6",
				sliderInput(inputId="plot_months","Month range:",min=min(lake_elev_data$Month),max=max(lake_elev_data$Month),value=c(min(lake_elev_data$Month),max(lake_elev_data$Month)),sep="",step=1),
				#checkboxGroupInput("sites","Include:",choices=c("Utah Lake","Provo Bay"),selected=c("Utah Lake","Provo Bay"))
				conditionalPanel(
					condition="input.tabs!=5 & input.tabs!=6",
					radioButtons("sites","Include:",choiceNames=c("Utah Lake", "Provo Bay", "All"),choiceValues=c(1,2,3),selected=3)
				)
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
									choiceNames=c("Time series","Site boxplot","Pairwise"),
									choiceValues=c(1,2,3),selected=1,inline=TRUE),
				conditionalPanel(condition="input.chem_plot_type==3",
					h4("Parameter 1:")
				),
				uiOutput("chem_param1"),
				uiOutput("chem_frac1"),
				uiOutput("reldepth1"),
				conditionalPanel(
					condition="input.chem_plot_type==3 | input.chem_plot_type==2",
					checkboxInput("chem_param1_log","log(y)",value=FALSE)
				),
				
				conditionalPanel(				
					condition="input.chem_plot_type==3",
					h4("Parameter 2:"),
					uiOutput("chem_param2"),
					uiOutput("chem_frac2"),
					uiOutput("reldepth2"),
					checkboxInput("chem_param2_log","log(x)",value=FALSE)
				)
			),

			####TSI tab:
			conditionalPanel(
				condition="input.tabs==3",
					radioButtons(inputId="TSI_plot_type",label="Plot type:",
									choiceNames=c("Time series","Boxplot","Site means","Scatterplot"),
									choiceValues=c(1,2,4,3),selected=1,inline=TRUE)
					),
					
			
			###NLA comp tab:
			conditionalPanel(
				condition="input.tabs==4",
				radioButtons("nla_comp_plot_type", "Plot type:", choiceNames=c("Scatterplot","Boxplot"), choiceValues=c(1,2),inline=T),
				conditionalPanel(
					condition="input.nla_comp_plot_type==1",
					selectInput("comp_param_x","Parameter x:",choices=nla_comp_choices,selected="Phosphate-phosphorus"),
					selectInput("comp_param_y","Parameter y:",choices=nla_comp_choices,selected="Chlorophyll a")
				),
				conditionalPanel(
					condition="input.nla_comp_plot_type==2",
					radioButtons("comp_bp_ruler_onoff","Ruler",choiceNames=c("On","Off"),choiceValues=c(1,2),selected=2,inline=T),
					conditionalPanel(
						condition="input.comp_bp_ruler_onoff==1",
						sliderInput("comp_bp_ruler","Ruler:",min=0,max=120,value=50)
					)
				)
			),
			
			###WQ map tab:
			conditionalPanel(
				condition="input.tabs==5",
				uiOutput("map_param"),
				uiOutput("map_frac"),
				uiOutput("map_reldepth"),
				#checkboxInput("wq_map_uniform_scale","Maintain uniform z-scale (for time-series comparisons)",value=FALSE),
				actionButton("interpolate", "Interpolate", style='height:50px; padding:4px; font-size:150%',width="200px")
			),
			
			
			
			
			
			###Phytoplankton tab
			conditionalPanel(
				condition="input.tabs==6",
				sliderInput(inputId="phyto_plot_years","Year range:",min=min(phyto_data$Year,na.rm=T),max=max(phyto_data$Year,na.rm=T),value=c(2002,max(phyto_data$Year,na.rm=T)),sep="",animate=T,
					animationOptions(interval = 250, loop = TRUE), step=1),
				sliderInput(inputId="phyto_plot_months","Month range:",min=min(phyto_data$Month,na.rm=T),max=max(phyto_data$Month,na.rm=T),value=c(min(phyto_data$Month,na.rm=T),max(phyto_data$Month,na.rm=T)),sep="",animate=T,
					animationOptions(interval = 250, loop = TRUE), step=1),
				radioButtons("phyto_samp_type","Sample type:",choices=unique(phyto_data$SampleType),selected="Total phytoplankton",inline=T),
				radioButtons("phyto_plot_type","Plot type:",choiceNames=c("Time series","Map"),choiceValues=c(1,2),selected=1,inline=T),
				#conditionalPanel(
					#condition="input.phyto_plot_type==1",
					radioButtons("genus_or_division","Group taxa by:",choiceNames=c("Genus","Division"),choiceValues=c(1,2),selected=2,inline=T),
					conditionalPanel(
						condition="input.genus_or_division==1",
						uiOutput("genus")
					),
					conditionalPanel(
						condition="input.genus_or_division==2 & input.phyto_plot_type==1",
						radioButtons("stack_divs", "Stacked or single division:",choiceNames=c("Stack","Single"), choiceValues=c(1,0), selected=1,inline=T)
					),
					conditionalPanel(
						condition="(input.genus_or_division==2 & input.stack_divs==0) | (input.genus_or_division==2 & input.phyto_plot_type==2)",
						uiOutput("division")
					),
					radioButtons("abd_bv","Response type:",choiceNames=c("Abundance","Biovolume"),choiceValues=c(1,2),selected=1,inline=T),
					conditionalPanel(
						condition="input.abd_bv==1",
						radioButtons("abund_relabund","Abundance type:",choices=c("Abundance","Relative abundance"),selected="Abundance",inline=T)
					),
					conditionalPanel(
						condition="input.abd_bv==2",
						radioButtons("bv_rbv","Biovolume type:",choices=c("Biovolume","Relative biovolume"),selected="Biovolume",inline=T)
					),
					conditionalPanel(
						condition="input.phyto_plot_type==2",
						radioButtons("phyto_z_fixed_dynamic", "Z-scaling:", choices=c("Dynamic","Fixed"),selected="Dynamic",inline=T)
					)
				#)
			),



			###Help text
			br(),
			helpText("For help with this tool, or to report a bug, please contact Jake Vander Laan, UDWQ, jvander@utah.gov, (801) 536-4350.")

		),
		
		
		
		mainPanel(
			conditionalPanel(
				condition="input.tabs==1",
				plotOutput(outputId = "elev_plot",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==2",
				plotOutput(outputId = "wq_plot",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==3",
				plotOutput(outputId = "tsi_plot",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==4",
				plotOutput(outputId = "nla_comp",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==5",
				plotOutput(outputId = "wq_map",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==6 & input.phyto_plot_type==1",
				plotOutput(outputId = "phyto_output",width="800px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==6 & input.phyto_plot_type==2",
				leafletOutput("phyto_map_output",width="800px",height="800px")
			)

		)
		
	)

)



server <- function(input, output){

	#Reactive inputs:
	
	reactive_objects=reactiveValues()
	observe({
		if(input$sites==3){reactive_objects$selected_sites=unique(wq_data$Monitoring.Location.ID)}
		if(input$sites==2){reactive_objects$selected_sites=c(4917450,4917470)}	
		if(input$sites==1){reactive_objects$selected_sites=unique(na.omit(wq_data$Monitoring.Location.ID[wq_data$Monitoring.Location.ID!=4917450&wq_data$Monitoring.Location.ID!=4917470]))}
	})
	
	observe({
		reactive_objects$wq_plot_data=wq_data[
												wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
												wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]&
												wq_data$Monitoring.Location.ID%in%reactive_objects$selected_sites
												,]

		reactive_objects$param_choices=unique(reactive_objects$wq_plot_data$Parameter)[order(unique(as.character(reactive_objects$wq_plot_data$Parameter)))]

		reactive_objects$wq_map_data=wq_data[
												wq_data$Year>=input$plot_years[1]&wq_data$Year<=input$plot_years[2]&
												wq_data$Month>=input$plot_months[1]&wq_data$Month<=input$plot_months[2]
												,]

		reactive_objects$trophic_plot_data=trophic_data[
								trophic_data$Year>=input$plot_years[1]&trophic_data$Year<=input$plot_years[2]&
								trophic_data$Month>=input$plot_months[1]&trophic_data$Month<=input$plot_months[2]&
								trophic_data$Monitoring.Location.ID%in%reactive_objects$selected_sites
								,]
	})


	output$chem_param1 <- renderUI({
		selectInput("chem_param1", "Parameter:", reactive_objects$param_choices, selected=input$chem_param1)
	})
    
	output$chem_param2 <- renderUI({
		selectInput("chem_param2", "Parameter:", reactive_objects$param_choices, selected=input$chem_param2)
	})
   
	observe({
		req(input$chem_param1)
		req(input$chem_param2)
		reactive_objects$chem_frac1_choices=unique(reactive_objects$wq_plot_data$Fraction[reactive_objects$wq_plot_data$Parameter==input$chem_param1])
		reactive_objects$chem_frac2_choices=unique(reactive_objects$wq_plot_data$Fraction[reactive_objects$wq_plot_data$Parameter==input$chem_param2])
	})

	output$chem_frac1 <- renderUI({
		selectInput("chem_frac1", "Fraction:", reactive_objects$chem_frac1_choices, selected=input$chem_frac1)
	})


	output$chem_frac2 <- renderUI({
		selectInput("chem_frac2", "Fraction:", reactive_objects$chem_frac2_choices, selected=input$chem_frac2)
	})

	observe({
		req(input$chem_frac1)
		reactive_objects$chem_reld1_choices=unique(reactive_objects$wq_plot_data$Depth[reactive_objects$wq_plot_data$Parameter==input$chem_param1&
																										 reactive_objects$wq_plot_data$Fraction==input$chem_frac1])
		req(input$chem_frac2)
		reactive_objects$chem_reld2_choices=unique(reactive_objects$wq_plot_data$Depth[reactive_objects$wq_plot_data$Parameter==input$chem_param2&
																										 reactive_objects$wq_plot_data$Fraction==input$chem_frac2])
	})

	output$reldepth1 <- renderUI({
		selectInput("reldepth1", "Sample depth:", reactive_objects$chem_reld1_choices, selected=input$reldepth1)
	})

	output$reldepth2 <- renderUI({
		selectInput("reldepth2", "Sample depth:", reactive_objects$chem_reld2_choices, selected=input$reldepth2)
	})


	output$map_param <- renderUI({
		selectInput("map_param", "Parameter:", reactive_objects$param_choices, selected=input$map_param)
	})

	observe({
		req(input$map_param)
		reactive_objects$map_frac_choices=unique(reactive_objects$wq_plot_data$Fraction[reactive_objects$wq_plot_data$Parameter==input$map_param])
	})

	output$map_frac <- renderUI({
		selectInput("map_frac", "Fraction:", reactive_objects$map_frac_choices, selected=input$map_frac)
	})

	observe({
		req(input$map_frac)
		reactive_objects$map_reldepth_choices=unique(reactive_objects$wq_plot_data$Depth[reactive_objects$wq_plot_data$Parameter==input$map_param&
																										 reactive_objects$wq_plot_data$Fraction==input$map_frac])
	})

	
	observe({
		phyto_plot_data=phyto_data[!is.na(phyto_data$Year) & !is.na(phyto_data$Month) & phyto_data$SampleType==input$phyto_samp_type,]
		phyto_plot_data=phyto_plot_data[phyto_plot_data$Year>=input$phyto_plot_years[1] & phyto_plot_data$Year<=input$phyto_plot_years[2] & phyto_plot_data$Month>=input$phyto_plot_months[1] & phyto_plot_data$Month<=input$phyto_plot_months[2],]
		reactive_objects$phyto_plot_data=phyto_plot_data
	})
	
	observe({
		req(reactive_objects$phyto_plot_data)
					
			phyto_plot_data=reactive_objects$phyto_plot_data
	
			if(dim(phyto_plot_data)[1]>1){
				#ID all unique samples
				samples=data.frame(unique(phyto_plot_data[,c("Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude","Date")]))
				
				#ID all divisions
				divisions=data.frame(unique(phyto_plot_data[,c("Division")]))
				names(divisions)="Division"
				
				#ID all genera
				genera=data.frame(unique(phyto_plot_data[,c("Genus")]))
				names(genera)="Genus"
				
				#Rename selected response var (abundance or biovolume)
				if(input$abd_bv==1){ #abd
					names(phyto_plot_data)[names(phyto_plot_data)=="CellperML"]="raw_response"
				}else{ #bv
					names(phyto_plot_data)[names(phyto_plot_data)=="CellVolume_u3mL"]="raw_response"	
				}
	
				#Aggregate data by genus or division
				if(input$genus_or_division==1){ #genus
					#Fill implicit zeros
					samps_groups=merge(samples, genera, all=T)
					phyto_plot_data=merge(samps_groups, phyto_plot_data, all.x=T)
					phyto_plot_data$raw_response[is.na(phyto_plot_data$raw_response)]=0
					
					#Aggregate
					agg_phyto_plot_data=aggregate(raw_response~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Genus, phyto_plot_data, FUN='sum')
					
					#subset to selected genus
					agg_phyto_plot_data=agg_phyto_plot_data[agg_phyto_plot_data$Genus==input$genus,]
					
				}else{ #division
					#Fill implicit zeros
					samps_groups=merge(samples, divisions, all=T)
					phyto_plot_data=merge(samps_groups, phyto_plot_data, all.x=T)
					phyto_plot_data$raw_response[is.na(phyto_plot_data$raw_response)]=0
					
					#Aggregate
					agg_phyto_plot_data=aggregate(raw_response~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Division, phyto_plot_data, FUN='sum')
					
					#Subset to selected division (if stack_divs==0)
					if(input$stack_divs==0){
						agg_phyto_plot_data=agg_phyto_plot_data[agg_phyto_plot_data$Division==input$division,]	
					}
				}
							
				#Calc sample totals
				totals=aggregate(raw_response~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date, phyto_plot_data, FUN='sum')
				names(totals)[names(totals)=="raw_response"]="total"
				
				#Merge sample totals to agg'd data
				agg_phyto_plot_data=merge(agg_phyto_plot_data, totals, all.x=T)
				
				
				#Calculate relative response
				agg_phyto_plot_data$rel_response=agg_phyto_plot_data$raw_response/agg_phyto_plot_data$total
			
			}else{
				agg_phyto_plot_data=phyto_plot_data[,c("CellperML","Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude","Date","Year","Month","Genus")]
			}
			
			#Set response
			if((input$abd_bv==1 & input$abund_relabund=="Abundance") | (input$abd_bv==2 & input$bv_rbv=="Biovolume")){
				agg_phyto_plot_data$response=agg_phyto_plot_data$raw_response
			}else{
				agg_phyto_plot_data$response=agg_phyto_plot_data$rel_response
			}
			
			#Rename grouping var (Genus or Division)
			names(agg_phyto_plot_data)[names(agg_phyto_plot_data)=="Genus"]="group"
			names(agg_phyto_plot_data)[names(agg_phyto_plot_data)=="Division"]="group"
			
			
			#Append into reactive_objects
			reactive_objects$agg_phyto_plot_data=agg_phyto_plot_data
			
			#Generate label

			#genus/abd
			if(input$genus_or_division==1){
				ycomp1=input$genus
			}else{
				if(input$stack_divs==1){
						ycomp1="Division"
				}else{
					ycomp1=input$division
				}
			}
			
			if(input$abd_bv==1){
				if(input$abund_relabund=="Abundance"){
					ycomp2="abundance (cells/mL)"
				}else{
					ycomp2="relative abundance"
				}
			}else{
				if(input$bv_rbv=="Biovolume"){
					ycomp2="biovolume (u3/mL)"
				}else{
					ycomp2="Relative biovolume"
				}
			}

			ylabel=paste(ycomp1,ycomp2)

			reactive_objects$phyto_ylab=ylabel
			
			
			
			print(ylabel)
			print(head(agg_phyto_plot_data))
			
	})
	

	output$map_reldepth <- renderUI({
		selectInput("map_reldepth", "Sample depth:", reactive_objects$map_reldepth_choices, selected=input$map_reldepth)
	})

	output$genus <- renderUI({
		selectInput("genus","Genus:",choices=unique(phyto_data$Genus)[order(unique(phyto_data$Genus))], selected=input$genus)
	})
	
	output$division <- renderUI({
		selectInput("division","Algal division:",choices=unique(phyto_data$Division)[order(unique(phyto_data$Division))], selected=input$division)
	})
	
	
		
	#Tab 1: Elevation plot outputs
	output$elev_plot=renderPlot({
		par(mfrow=c(2,1),mar=c(4.1,6.1,4.1,5.1))
		elev_plot_data=lake_elev_data[lake_elev_data$Year>=input$elev_plot_years[1]&lake_elev_data$Year<=input$elev_plot_years[2],]
		
		suppressWarnings(
			lineplot.CI(Year,Elevation..ft.,data=elev_plot_data,x.cont=TRUE,xlim=c(input$elev_plot_years[1],input$elev_plot_years[2]),cex=1.5,
				ylab="Lake elevation (ft)",xlab="Year",cex.lab=2,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
		)
		axis(1,cex.axis=1.5,cex.lab=2)
		
		if(input$elev_rulers==1){
			abline(h=input$elev_ruler1,lwd=2,lty=2,col="orange")
		}
		
		suppressWarnings(		
			lineplot.CI(Month,Elevation..ft.,data=elev_plot_data,x.cont=TRUE,xlim=c(1,12),cex=1.5,
				ylab="Lake elevation (ft)",xlab="Month",cex.lab=2,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
		)
		axis(1,cex.axis=1.5,cex.lab=2,at=seq(1,12,1))
		
		if(input$elev_rulers==1){
			abline(h=input$elev_ruler2,lwd=2,lty=2,col="orange")
		}
	})

	
	
	#Tab 2: Water quality plot outputs
	output$wq_plot=renderPlot({
		req(reactive_objects$wq_plot_data)
		req(input$reldepth1)
		
		if(input$chem_param1_log==FALSE & input$chem_param2_log==FALSE){plot_log=''}
		if(input$chem_param1_log==TRUE & input$chem_param2_log==FALSE){plot_log='y'}
		if(input$chem_param1_log==FALSE & input$chem_param2_log==TRUE){plot_log='x'}
		if(input$chem_param1_log==TRUE & input$chem_param2_log==TRUE){plot_log='xy'}
	
		#Plot type 1: time series
		if(input$chem_plot_type==1 | input$chem_plot_type==2){
			plot_data=reactive_objects$wq_plot_data
			plot_data=plot_data[
								plot_data$Parameter==input$chem_param1&
								plot_data$Fraction==input$chem_frac1&
								plot_data$Depth==input$reldepth1
								,]
				units=plot_data$Result.Unit[1]
				ylabel=paste0(input$chem_param1," (",units,")")
				title=paste(input$chem_frac1,"-",input$reldepth1)		
		}

		plot_data=reactive_objects$wq_plot_data
		plot_data=plot_data[
							plot_data$Parameter==input$chem_param1&
							plot_data$Fraction==input$chem_frac1&
							plot_data$Depth==input$reldepth1
							,]
			units=plot_data$Result.Unit[1]
			ylabel=paste0(input$chem_param1," (",units,")")
			title=paste(input$chem_frac1,"-",input$reldepth1)		

		if(input$chem_plot_type==3){
			
			plot_data1=plot_data[,c("Date","Monitoring.Location.ID","Year","Month","Result.Value")]
			
			
			plot_data2=reactive_objects$wq_plot_data
			plot_data2=plot_data2[
								plot_data2$Parameter==input$chem_param2&
								plot_data2$Fraction==input$chem_frac2&
								plot_data2$Depth==input$reldepth2
								,]
			units2=plot_data2$Result.Unit[1]
			xlabel=paste0(input$chem_param2," (",units2,")")
			plot_data2=plot_data2[,c("Date","Monitoring.Location.ID","Year","Month","Result.Value")]
			names(plot_data2)[names(plot_data2) == 'Result.Value'] <- 'Result.Value2'
			sp_data=merge(plot_data1,plot_data2)
					
		}
		
		if(input$chem_plot_type==1){
			par(mfrow=c(2,1),mar=c(4.1,6.1,4.1,5.1))
			if(dim(plot_data)[1]>0){
				suppressWarnings(
					lineplot.CI(Year,Result.Value,data=plot_data,x.cont=TRUE,xlim=c(input$plot_years[1],input$plot_years[2]),cex=1.5,
						ylab=ylabel,xlab="Year",cex.lab=2,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n',main=title,cex.main=2)
				)
				axis(1,cex.axis=1.5,cex.lab=2)
				
				suppressWarnings(
				lineplot.CI(Month,Result.Value,data=plot_data,x.cont=TRUE,xlim=c(1,12),cex=1.5,
					ylab=ylabel,xlab="Month",cex.lab=2,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n',main=title,cex.main=2)
				)
				axis(1,cex.axis=1.5,cex.lab=2)
				
				
				#suppressWarnings(
				#	plotmeans(Result.Value~Year,plot_data,n.label=F,barcol="blue",barwidth=2,lwd=2,col="blue",pch=19,cex=2,
				#		xlab="Year",cex.lab=2,cex.axis=2,ylab=ylabel,main=title,cex.main=2,xlim=c(input$plot_years[1],input$plot_years[2]))
				#)
				#suppressWarnings(
				#	plotmeans(Result.Value~Month,plot_data,n.label=F,barcol="blue",barwidth=2,lwd=2,col="blue",pch=19,cex=2,
				#		xlab="Month",cex.lab=2,cex.axis=2,ylab=ylabel,main=title,cex.main=2,xlim=c(1,12))
				#)
			}
		}
	
		if(input$chem_plot_type==2){
			par(mar=c(16,8,4.1,3))
			if(dim(plot_data)[1]>0){
				if(plot_log=="y" | plot_log=="xy"){
					boxplot(Result.Value~Monitoring.Location.ID,plot_data,cex.axis=2,cex.lab=2.5,ylab=ylabel,main=title,cex.main=2,yaxt='n',las=2,log='y')
				}else{
					boxplot(Result.Value~Monitoring.Location.ID,plot_data,cex.axis=2,cex.lab=2.5,ylab=ylabel,main=title,cex.main=2,yaxt='n',las=2)
				}
				axis(2,cex.axis=2)
			}
		}
		
		if(input$chem_plot_type==3){
			par(mar=c(8,8,2,3))
			sp_data=na.omit(sp_data)
			if(dim(sp_data)[1]>2){
				plot(Result.Value~Result.Value2,sp_data,
					 ylab=ylabel, xlab=xlabel, cex.lab=2.5,cex.axis=2,log=plot_log)
				if(plot_log==''){lm_n=lm(Result.Value~Result.Value2,sp_data)}
				if(plot_log=='x'){lm_n=lm(Result.Value~log10(Result.Value2),sp_data)}
				if(plot_log=='y'){lm_n=lm(log10(Result.Value)~Result.Value2,sp_data)}
				if(plot_log=='xy'){lm_n=lm(log10(Result.Value)~log10(Result.Value2),sp_data)}
				req(lm_n)
				abline(lm_n$coefficients[1],lm_n$coefficients[2],lwd=2,lty=2,col="purple")
				r2=round(summary(lm_n)$adj.r.squared,2)
				pval=round(summary(lm_n)$coefficients[2,4],2)
				legend('bottomright',bty='n',legend=c(paste0("r2 = ",r2),paste0("p = ",pval)),cex=2.5)		
			
			}else{
				frame()
				box()
				text(0.5,0.5,"Two or fewer paired samples", cex=3)}
		}
		
		
	})
	
	
	#Tab 3: TSI plot outputs
	output$tsi_plot=renderPlot({
		tsi_plot_data=reactive_objects$trophic_plot_data
		TSI_flat=tsi_plot_data[,c("Date","Monitoring.Location.ID","Year","Month","TSIchl","TSItp","TSIsd")]
		TSI_flat=melt(TSI_flat,id.vars=c("Date","Monitoring.Location.ID","Year","Month"))

		if(dim(tsi_plot_data)[1]>0){
		
			#Time series
			if(input$TSI_plot_type==1){
				par(mfrow=c(2,1),mar=c(4.1,6.1,4.1,5.1))
				suppressWarnings(lineplot.CI(Year,value,group=variable,data=TSI_flat,legend=F,pch=c(NA,NA,NA),err.width=0.05,err.col=NA,type='p',xlab="Year",ylab="TSI",x.cont=TRUE,xaxt='n',cex.axis=1.5,cex.lab=2,xlim=c(input$plot_years[1],input$plot_years[2])))
				agg=aggregate(value~Year+variable,TSI_flat,FUN='mean')
				lines(value~Year,agg[agg$variable=="TSIchl",],col="green",lwd=2)
				lines(value~Year,agg[agg$variable=="TSIsd",],col="blue",lwd=2)
				lines(value~Year,agg[agg$variable=="TSItp",],col="orange",lwd=2)
				suppressWarnings(lineplot.CI(as.factor(Year),value,group=variable,data=TSI_flat,legend=F,pch=c(21,21,21),col=c("green","orange","blue"),err.width=0.05,type='p',add=TRUE,xaxt='n',yaxt='n',lwd=2,x.cont=TRUE,cex=2))
				axis(1,cex.axis=1.5,cex.lab=2)
				legend("bottomleft",pch=21,col=c("blue","orange","green"),legend=c("Secchi TSI","TP TSI","ChlA TSI"),lty=c(1,1,1),lwd=2,bty='n',cex=1.5)
				
				suppressWarnings(lineplot.CI(Month,value,group=variable,data=TSI_flat,legend=F,pch=c(NA,NA,NA),err.width=0.05,err.col=NA,type='p',xlab="Month",ylab="TSI",x.cont=TRUE,xaxt='n',cex.axis=1.5,cex.lab=2,ylim=c(30,100),xlim=c(1,12)))
				agg=aggregate(value~Month+variable,TSI_flat,FUN='mean')
				lines(value~Month,agg[agg$variable=="TSIchl",],col="green",lwd=2)
				lines(value~Month,agg[agg$variable=="TSIsd",],col="blue",lwd=2)
				lines(value~Month,agg[agg$variable=="TSItp",],col="orange",lwd=2)				
				suppressWarnings(lineplot.CI(as.factor(Month),value,group=variable,data=TSI_flat,legend=F,pch=c(21,21,21),col=c("green","orange","blue"),err.width=0.05,type='p',add=TRUE,xaxt='n',yaxt='n',lwd=2,x.cont=TRUE,cex=2))
				axis(1,cex.axis=1.5,cex.lab=2,at=seq(1,12,1))
				legend("bottomleft",pch=21,col=c("blue","orange","green"),legend=c("Secchi TSI","TP TSI","ChlA TSI"),lty=c(1,1,1),lwd=2,bty='n',cex=1.5)
			}
			
			#TSI boxplot
			if(input$TSI_plot_type==2){
				par(mar=c(6,6,1,1))
				boxplot(tsi_plot_data$TSIchl,tsi_plot_data$TSItp,tsi_plot_data$TSIsd,border=c("green","orange","blue"),
						names=c("ChlA","TP","Secchi"),ylab="TSI",cex.axis=2,cex.lab=2.5,lwd=2,ylim=c(0,120))
			}
		
			#Scatterplot
			if(input$TSI_plot_type==3){
				par(mar=c(6,6,1,1))
				plot3dTSI(tsi_plot_data)
			}
			
			#Site boxplot
			if(input$TSI_plot_type==4){
				par(mfrow=c(2,2),mar=c(8,8,1,1))
				site_bp_data=tsi_plot_data[tsi_plot_data$Monitoring.Location.ID%in%
											c(4917310,4917370,4917390,4917450,4917500,4917520,4917600,4917770)
											,]				
				if(all(is.na(site_bp_data$TSIchl))){
					plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='Chl A TSI',xlab='',cex.lab=2)
					text(1,0,"No data for selected period.",cex=1.5)				
				}else{
					plotmeans(TSIchl~Monitoring.Location.ID,site_bp_data,las=2,connect=F,xlab="",col="blue",cex=3, ylab="Chl A TSI",cex.lab=2,cex.axis=1.5,barwidth=2,n.label=F)
				}
				if(all(is.na(site_bp_data$TSItp))){
					plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='TP TSI',xlab='',cex.lab=2)
					text(1,0,"No data for selected period.",cex=1.5)				
				}else{
					plotmeans(TSItp~Monitoring.Location.ID,site_bp_data,las=2,connect=F,xlab="",col="blue",cex=3, ylab="TP TSI",cex.lab=2,cex.axis=1.5,barwidth=2,n.label=F)
				}
				if(all(is.na(site_bp_data$TSIsd))){
					plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='SD TSI',xlab='',cex.lab=2)
					text(1,0,"No data for selected period.",cex=1.5)				
				}else{
					plotmeans(TSIsd~Monitoring.Location.ID,site_bp_data,las=2,connect=F,xlab="",col="blue",cex=3, ylab="SD TSI",cex.lab=2,cex.axis=1.5,barwidth=2,n.label=F)
				}
			}
			
		}
		
	})
	
	
	#Tab 4: Trophic NLA comparison
	output$nla_comp=renderPlot({
		ul_data=reactive_objects$trophic_plot_data
				
		names(nla_data)[names(nla_data) == 'Total_phosphorus_mgL'] <- 'Phosphate-phosphorus'
		names(nla_data)[names(nla_data) == 'Chlorophyll_a_ugL'] <- 'Chlorophyll a'
		names(nla_data)[names(nla_data) == 'Secchi_m'] <- 'Depth, Secchi disk depth'
		
		if(input$nla_comp_plot_type==1){
			ylabel=names(nla_comp_choices[nla_comp_choices==input$comp_param_y])
			xlabel=names(nla_comp_choices[nla_comp_choices==input$comp_param_x])
			
			par(mar=c(12,12,3,1))
			plot(nla_data[,input$comp_param_y]~nla_data[,input$comp_param_x],log='xy',pch=21,col="black",bg="white",cex=1.5,
				ylab=ylabel, xlab=xlabel, cex.lab=2, cex.axis=1.5)
			lm_nla=lm(log10(nla_data[,input$comp_param_y])~log10(nla_data[,input$comp_param_x]))
			abline(lm_nla$coefficients[1],lm_nla$coefficients[2],lwd=4,lty=2,col="black")
			if(lm_nla$coefficients[2]>0){leg_location="bottomright"}else{leg_location="bottomleft"}
			
			if(input$comp_param_x!="Total_nitrogen_mgL"&input$comp_param_y!="Total_nitrogen_mgL"){
				points(ul_data[,input$comp_param_y]~ul_data[,input$comp_param_x],col="black",bg="orange",pch=21,cex=2.5)
				lm_ul=lm(log10(ul_data[,input$comp_param_y])~log10(ul_data[,input$comp_param_x]))
				abline(lm_ul$coefficients[1],lm_ul$coefficients[2],lwd=4,lty=2,col="orange")
					legend(leg_location,bty='n',pch=21,col=c("black","black"),pt.bg=c("orange","white"),cex=2.5, pt.cex=c(2.5,1.5),legend=c("Utah Lake","NLA"))
			}else{
				legend(leg_location,bty='n',pch=21,col="black",pt.bg="white",cex=2.5, pt.cex=1.5,legend="NLA")
			}
		}
		
		if(input$nla_comp_plot_type==2){
			par(mar=c(12,12,3,3))
			boxplot(ul_data$TSIchl,ul_data$TSItp,ul_data$TSIsd,nla_data$TSIchl,nla_data$TSItp,nla_data$TSIsd, boxwex=0.375, at=c(0,0.4,0.8,1.6,2,2.4),xaxt='n',
				border=c("green","orange","blue"),ylab="TSI",cex.axis=2,cex.lab=2.5,lwd=2,ylim=c(0,120))
			axis(1,at=c(0,0.4,0.8,1.6,2,2.4),outer=F,labels=c("ChlA","TP","Secchi","ChlA","TP","Secchi"),cex.axis=1.5)
			axis(1,at=c(0.4,2),outer=T,lty=0,labels=c("Utah Lake","NLA"),line=-9,cex.axis=2.5)
			if(input$comp_bp_ruler_onoff==1){
				abline(h=input$comp_bp_ruler,lty=2,lwd=3,col="purple")
			}
		}
		
		
		
		
	})
		
		
		
	#Tab 5: WQ Map	
	
	costraster=raster("polyrast/costrast2")
	mask_poly=readOGR(paste0(getwd(),"/polyrast"),"UtahLake_poly_wgs84")
	projection(mask_poly)=c("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	projection(costraster)=c("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

	output$wq_map <- renderPlot({
		input$interpolate
		isolate({
		par(mar=c(6,4,6,2))
		req(reactive_objects$wq_map_data)
		req(input$map_reldepth)
			map_data=reactive_objects$wq_map_data
			map_data=map_data[
							map_data$Parameter==input$map_param&
							map_data$Fraction==input$map_frac&
							map_data$Depth==input$map_reldepth
						,]
		units=map_data$Result.Unit[1]
		req(units)
		maptitle=paste0(input$map_frac," ",input$map_reldepth," ",input$map_param," (",units,")")
			ipdwMap(mapdata=map_data,costraster=costraster,maptitle=maptitle,mask_poly=mask_poly)
			#plot(mask_poly,add=T)
		})

	})

	
	
	
	
	#Tab 6: Phytoplankton
	
	#Genus/Division time series (single taxon)
	output$phyto_output<-renderPlot({
	#	req(reactive_objects$agg_phyto_plot_data)
	#	agg_phyto_plot_data=reactive_objects$agg_phyto_plot_data
	#	ylabel=reactive_objects$phyto_ylab
	#	par(mfrow=c(2,1),mar=c(4.1,6.1,4.1,5.1))
	#	if(dim(reactive_objects$phyto_plot_data)[1]>0){
	#		if(input$phyto_plot_type==1){
	#			if(input$abund_relabund=="Abundance"){
	#				suppressWarnings(
	#					lineplot.CI(Year,CellperML,data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_years[1],input$phyto_plot_years[2]),cex=1.5,
	#						ylab=ylabel,xlab="Year",cex.lab=1.5,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
	#				)
	#				axis(1,cex.axis=1.5,cex.lab=2)
	#				suppressWarnings(
	#					lineplot.CI(Month,CellperML,data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_months[1],input$phyto_plot_months[2]),cex=1.5,
	#						ylab=ylabel,xlab="Month",cex.lab=1.5,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
	#				)
	#				axis(1,cex.axis=1.5,cex.lab=2)
	#			}else{
	#				suppressWarnings(
	#					lineplot.CI(Year,RA,data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_years[1],input$phyto_plot_years[2]),cex=1.5,
	#						ylab=ylabel,xlab="Year",cex.lab=1.5,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
	#				)
	#				axis(1,cex.axis=1.5,cex.lab=2)
	#				suppressWarnings(
	#					lineplot.CI(Month,RA,data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_months[1],input$phyto_plot_months[2]),cex=1.5,
	#						ylab=ylabel,xlab="Month",cex.lab=1.5,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
	#				)
	#				axis(1,cex.axis=1.5,cex.lab=2)
	#			}
	#		}
	#	}else{
	#		frame()
	#		box()
	#		text(0.5,0.5,"No data for selected years & months.", cex=1.75)
	#		frame()
	#		box()
	#		text(0.5,0.5,"No data for selected years & months.", cex=1.75)
	#	}
	})
		
		
	output$phyto_map_output <- renderLeaflet({
		#req(reactive_objects$agg_phyto_plot_data)
		#req(reactive_objects$phyto_plot_data)
		#agg_phyto_plot_data=reactive_objects$agg_phyto_plot_data
        #
		#addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){ ###From: https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
		#	colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
		#	labelAdditions <- paste0("<div class = 'legendCircle' style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
		#	return(addLegend(map, title = legend_title, colors = colorAdditions, labels = labelAdditions))
		#}
        #
		#
		#if(input$genus_or_division==1){legend_title_gd=input$genus}else{legend_title_gd=input$division}
		#if(input$abund_relabund=="Abundance"){
		#	agg_phyto_plot_data_site=aggregate(CellperML~Monitoring.Location.ID,agg_phyto_plot_data,FUN='mean')
		#	legend_title=paste(legend_title_gd,"abundance (cells/mL)")
		#}else{
		#	agg_phyto_plot_data_site=aggregate(RA~Monitoring.Location.ID,agg_phyto_plot_data,FUN='mean')
		#	legend_title=paste(legend_title_gd,"relative abundance")
		#}
		#names(agg_phyto_plot_data_site)[2]="z_val"
		#sites=unique(agg_phyto_plot_data[,c("Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude")])
		#agg_phyto_plot_data_site=merge(agg_phyto_plot_data_site,sites)
		#
		#####Fixed z-scaling
		#if(input$phyto_z_fixed_dynamic=="Fixed"){
		#	if(input$abund_relabund=="Relative abundance"){
		#		agg_phyto_plot_data_site$size=(agg_phyto_plot_data_site$z_val+0.1)*0.5
		#		legend_labs=c(0,0.25,0.5,0.75,1)
		#		legend_sizes=((legend_labs+0.1))*75
		#		
		#	}else{
		#		fixed_scale<-data.frame(phyto_data[,"CellperML"],rescale(log10(phyto_data[,"CellperML"]+1),c(0.1,0.5)))
		#		names(fixed_scale)=c("CellperML","fixed_scale")
		#		#print(head(fixed_scale))
		#		fixed_scale_lm=lm(fixed_scale~log10(CellperML+1),fixed_scale)
		#		agg_phyto_plot_data_site$CellperML=agg_phyto_plot_data_site$z_val
		#		agg_phyto_plot_data_site$size=predict(fixed_scale_lm,newdata=agg_phyto_plot_data_site)
		#		legend_labs=c(min(fixed_scale$CellperML),quantile(fixed_scale$CellperML,0.5),quantile(fixed_scale$CellperML,0.9),quantile(fixed_scale$CellperML,0.95),quantile(fixed_scale$CellperML,0.99),quantile(fixed_scale$CellperML,0.999))
		#		legend_labs=signif(legend_labs,2)
		#		legend_sizes=c(min(fixed_scale$fixed_scale),quantile(fixed_scale$fixed_scale,0.5),quantile(fixed_scale$fixed_scale,0.9),quantile(fixed_scale$fixed_scale,0.95),quantile(fixed_scale$fixed_scale,0.99),quantile(fixed_scale$fixed_scale,0.999))*75
		#		legend_sizes=legend_sizes*2
        #
		#	}
		#agg_phyto_plot_data_site$radius=agg_phyto_plot_data_site$size*75
		#}else{
		#	###Dynamic z-scaling
		#	agg_phyto_plot_data_site$size=rescale(agg_phyto_plot_data_site$z_val,c(0.1,0.5))
        #
		#	agg_phyto_plot_data_site$radius=agg_phyto_plot_data_site$size*75
		#	#print(head(agg_phyto_plot_data_site))
	    #
		#	legend_labs=c(min(agg_phyto_plot_data_site$z_val),quantile(agg_phyto_plot_data_site$z_val,0.25),quantile(agg_phyto_plot_data_site$z_val,0.5),quantile(agg_phyto_plot_data_site$z_val,0.75),max(agg_phyto_plot_data_site$z_val))
		#	legend_labs=signif(legend_labs,2)
		#	legend_sizes=c(min(agg_phyto_plot_data_site$radius),quantile(agg_phyto_plot_data_site$radius,0.25),quantile(agg_phyto_plot_data_site$radius,0.5),quantile(agg_phyto_plot_data_site$radius,0.75),max(agg_phyto_plot_data_site$radius))
		#	legend_sizes=legend_sizes*2
		#	
		#}
		#
		#legend=unique(data.frame(legend_sizes,legend_labs))
		#phyto_points=st_as_sf(agg_phyto_plot_data_site, coords = c("Monitoring.Location.Longitude", "Monitoring.Location.Latitude"), crs = 4326, remove=FALSE) # crs 4326 is WGS84
        #
		#
		#if(dim(reactive_objects$phyto_plot_data)[1]>0){
		#	phyto_map=leaflet(phyto_points) %>%
		#		addTiles() %>%
		#		addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
		#		addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
		#		addPolygons(data=ul_poly,smoothFactor=2,fillOpacity = 0,weight=3,color="lightskyblue") %>%
		#		addFeatures(phyto_points,color="orange",radius=phyto_points$radius, stroke=F,fillOpacity=0.5,
		#		) %>%
		#		addFeatures(phyto_points,color="orange",stroke=F,fillOpacity=0,
		#			popup = paste0(
		#				"MLID: ", phyto_points$Monitoring.Location.ID,
		#				"<br> Value: ", signif(phyto_points$z_val,2))
		#		) %>%			
	    #
		#		addLegendCustom(colors = rep("orange", dim(legend)[1]), labels = legend$legend_labs, sizes = legend$legend_sizes) %>%
	    #
		#		addLayersControl(
		#			position ="topleft",
		#			baseGroups = c("Topo","Satellite"),
		#			options = layersControlOptions(collapsed = FALSE, autoZIndex=FALSE))
		#}else{
		#	phyto_map=leaflet(phyto_points) %>%
		#		addTiles() %>%
		#		addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
		#		addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
		#		addPolygons(data=ul_poly,smoothFactor=2,fillOpacity = 0,weight=3,color="lightskyblue") %>%
		#		addLayersControl(
		#			position ="topleft",
		#			baseGroups = c("Topo","Satellite"),
		#			options = layersControlOptions(collapsed = FALSE, autoZIndex=FALSE))
		#
		#}	

	})
	
	
	
	
	
	
	
	
	
}


shinyApp(ui = ui, server = server)









