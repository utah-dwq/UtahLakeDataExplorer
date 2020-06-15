
###################################*
#### Utah Lake Data Explorer - Shiny App ####
###################################*

# setwd('F:\\Shiny\\UtahLakeDataExplorer')

# install.packages('data.table')
# install.packages('lubridate')
# install.packages('reshape2')
# install.packages('gplots')
# install.packages('sciplot')
# install.packages('sp')
# install.packages('sf')
# install.packages('leaflet')
# install.packages('RColorBrewer')
# install.packages('raster')
# install.packages('rgdal')
# install.packages('gdalUtils')
# install.packages('mapview')
# install.packages('Matrix')
# install.packages('gdistance')
# install.packages('ipdw')
# install.packages('scales')


# Publish Shiny app (run in console)
# library(shiny)
# library(rsconnect)
# deployApp(appDir=getwd(),appFiles=NULL,forceUpdate=T,account='markfernandez',appName='TEST_UtahLakeDataExplorer2')
# https://markfernandez.shinyapps.io/TEST_UtahLakeDataExplorer2/




# library(data.table)
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
library(rsconnect)
library(shiny)
library(tidyverse)
library(viridis)

options(scipen=999,digits=5)

# Clean workspace
# rm(list=ls()); gc()

source('functions/plot3dTSI.R')
source('functions/ipdwMap.R')





###################################*
#### Load Processed Datasets ####
###################################*

# Get filenames
tmp=list.files('RDataFiles',pattern='*.RData',full.names=T)
length(tmp) # 10 files

# Load all .RData files
lapply(tmp,load,.GlobalEnv)

# Convert all to data.frames - Optional
# lake_elev_data=as.data.frame(lake_elev_data)
# nla_data=as.data.frame(nla_data)
# phyto_data=as.data.frame(phyto_data)
# trophic_data=as.data.frame(trophic_data)
# wq_data=as.data.frame(wq_data)

# Define NLA comparison parameter choices:
nla_comp_choices=c('Phosphate-phosphorus','Chlorophyll a','Depth, Secchi disk depth','Total_nitrogen_mgL')
names(nla_comp_choices)=c('Total phosphorus (mg/L)','Chlorophyll a (ug/L)','Secchi depth (m)','Total nitrogen (mg/L)')

# Define Sonde data parameter choices: 
sonde_data_choices = c("Temperature", "Chl", "BGA", "DO", "DOsat", "pH", "Turbidity")
names(sonde_data_choices) = c("Temperature (Celsius)", "Chlorophyll a fluorescence (RFU)", 
                              "Phycocyanin fluorescence (RFU)", "Dissolved oxygen (mg/L)",
                              "Dissolved oxygen saturation (%)", "pH", "Turbidity (NTU)")

# Define Wind data parameter choices: 
wind_data_choices1 = c("windspeed.m.s", "tau.wind")
names(wind_data_choices1) = c( "Wind speed (m/s)", "Wave shear (N/m2)")

wind_data_choices2 = c("Turbidity.ntu", "tau.wind")
names(wind_data_choices2) = c("Turbidity (NTU)", "Wave shear (N/m2)")

# Define Clarity data parameter choices: 
clarity_data_choices_x = c("Turbidity", "chlorophyll.pheophytincorrected", 
                         "organic.carbon", "total.suspended.solids")
names(clarity_data_choices_x) = c("Turbidity (NTU)", "Chlorophyll a (ug/L)", 
                                "Dissolved organic carbon (mg/L)", "Total suspended solids (mg/L)")

clarity_data_choices_y = c("k", "secchidepth.m")
names(clarity_data_choices_y) = c("k", "Secchi depth (m)")

###################################*
#### UI ####
###################################*

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
		tabPanel("Phytoplankton",value=6),
		tabPanel("Sonde data", value=7), 
		tabPanel("Wind and turbidity", value=8), 
		tabPanel("Macrophytes and turbidity", value=9),
		tabPanel("Water clarity", value=10)
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
				helpText("Dynamic z-scaling scales marker sizes based on selected data. Fixed z-scaling defines a single scale of marker sizes for all plots; enabling visual comparisons among different taxa, time periods, or sample types.")
			),

			
			#conditionalPanel(condition="input.tabs==6",helpText("Tool description.")),
			br(),

			###Global inputs
			conditionalPanel(
				condition="input.tabs!=1 & input.tabs!=6 & input.tabs!=7 & input.tabs!=8 & input.tabs!=9 & input.tabs!=10",			
				sliderInput(inputId="plot_years","Year range:",min=min(wq_data$Year),max=max(wq_data$Year),value=c(1990,max(wq_data$Year)),sep="")
			),
			conditionalPanel(
				condition="input.tabs==1 & input.tabs!=7 & input.tabs!=8 & input.tabs!=9 & input.tabs!=10",			
				sliderInput(inputId="elev_plot_years","Year range:",min=min(lake_elev_data$Year),max=max(lake_elev_data$Year),value=c(min(lake_elev_data$Year),max(lake_elev_data$Year)),sep="")
			),
			
			conditionalPanel(
				condition="input.tabs!=1 & input.tabs!=6 & input.tabs!=7 & input.tabs!=8 & input.tabs!=9 & input.tabs!=10",
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
				), 
				downloadButton("downloadchemData", "Download"),
				
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
					
					conditionalPanel(
						condition="input.phyto_samp_type=='Total phytoplankton'",
						radioButtons("abd_bv","Response type:",choiceNames=c("Abundance","Biovolume"),choiceValues=c(1,2),selected=1,inline=T)
					),
					conditionalPanel(
						condition="input.abd_bv==1 | input.phyto_samp_type=='HAB'",
						radioButtons("abund_relabund","Abundance type:",choices=c("Abundance","Relative abundance"),selected="Abundance",inline=T)
					),
					conditionalPanel(
						condition="input.abd_bv==2 & input.phyto_samp_type!='HAB'",
						radioButtons("bv_rbv","Biovolume type:",choices=c("Biovolume","Relative biovolume"),selected="Biovolume",inline=T)
					),
					conditionalPanel(
						condition="input.phyto_plot_type==2",
						radioButtons("phyto_z_fixed_dynamic", "Z-scaling:", choices=c("Dynamic","Fixed"),selected="Dynamic",inline=T)
					)
				#)
			),

			
			### Sonde data tab:
			conditionalPanel(
			  condition="input.tabs==7",
			  helpText("This tool shows the conditions across sites in Utah Lake as measured by sensors deployed on sondes. 
			           Data were collected in 15-minute intervals and averaged into daily values."),
			  sliderInput(inputId="sonde_data_plot_months","Month range:",
			              min=4,max=11,
			              value=c(1, 12),sep="", step=1),
			  checkboxGroupInput("sonde_data_stations","Include:",
			                     choiceNames=c("Utah Lake North (4917365)", "Utah Lake Mid (4917390)", 
			                                   "Provo Bay (4917446)", "Utah Lake South (4917715)"),
			               choiceValues = c(4917365, 4917390, 4917446, 4917715), selected = c(4917365, 4917390, 4917446, 4917715)),
			  radioButtons("sonde_data_plot_type", "Plot type:", choiceNames=c("Scatterplot","Boxplot"), choiceValues=c(1,2),inline=T),
			  radioButtons("sonde_data_y_axis", "Y axis:", choiceNames=c("Linear scale","Log scale"), choiceValues=c(1,2),inline=T),
			  selectInput("sonde_choice_y","Parameter y:",choices=sonde_data_choices, selected="Temperature"),
			  downloadButton("downloadsondeData", "Download"),
			    
			),

			### Wind and turbidity data tab:
			conditionalPanel(
			  condition="input.tabs==8",
			  helpText("Sediments can be resuspended into the water column in response to waves and currents (shear stress),
			           resulting in high turbidity and reduced clarity.
			           This tool explores the theoretical and observational relationships between turbidity, wind speed, 
			           and shear stress due to wave activity."),
			  helpText("Theoretical tool: calculate wave shear experienced by sediments in response to changing wind and basin shape."),
			  sliderInput(inputId="shear_calculation_depth","Water depth (m):",
			              min = 1,max = 10, value = 3, sep="", step=0.5),
			  sliderInput(inputId="shear_calculation_wind","Wind speed (m/s):",
			              min = 0,max = 10, value = 2.5, sep="", step=0.5),
			  sliderInput(inputId="shear_calculation_fetch","Fetch (km):",
			              min = 5,max = 30, value = 24, sep="", step=1),
			  helpText("Observational tool: explore actual conditions in Utah Lake as measured by sensors on a data sonde. 
			           The dotted line on the wave shear axis represents critical shear, the shear value at which sediments are resuspended."),
			  checkboxGroupInput("wind_data_stations","Include:",
			                     choiceNames=c("Utah Lake North (4917365)", "Utah Lake Mid (4917390)", 
			                                   "Provo Bay (4917446)", "Utah Lake South (4917715)"),
			                     choiceValues = c(4917365, 4917390, 4917446, 4917715), selected = c(4917365, 4917390, 4917446, 4917715)),
			  selectInput("wind_choice_x","Parameter x:",choices=wind_data_choices1, selected="windspeed.m.s"),
			  selectInput("wind_choice_y","Parameter y:",choices=wind_data_choices2, selected="tau.wind"),
			),
			
			### Turbidity and macrophytes data tab:
			conditionalPanel(
			  condition="input.tabs==9",
			  helpText("Submerged aquatic vegetation (macrophytes) reduces the shear experienced by the sediments by reducing wave action.
			           This tool calculates the wave shear from observed dates based on the theoretical reduction provided by macrophytes."),
			  helpText("The vertical line represents the critical shear, the shear value at which sediments are resuspended.
			           Observations below critical shear will not experience sediment resuspension due to wave action."),
			  sliderInput(inputId="shear_reduction","% Reduction:",
			              min = 0, max = 80, value = 0, sep="", step=5),
			  radioButtons("macrophyte_stations","Include:",choiceNames=c("Utah Lake", "Provo Bay", "All"),choiceValues=c(1,2,3),selected=3),
			),
			
			## Light extinction data tab:
			conditionalPanel(
			  condition="input.tabs==10",
			  helpText("This tool illustrates the rapid attenuation of light with depth in Utah Lake. 
			           Light is measured as photosynthetically active radiation (PAR)."),
			  sliderInput(inputId="PAR_data_plot_months","Month range:",
			              min=min(PAR_data$Month,na.rm=T),max=max(PAR_data$Month,na.rm=T),
			              value=c(4, 9),sep="", step=1),
			  radioButtons("PAR_data_stations","Include:",choiceNames=c("Utah Lake", "Provo Bay", "All"),choiceValues=c(1,2,3),selected=3),
			  helpText("The degree of light attenuation can be measured by the light attenuation coefficient (k)
			           and Secchi depth. Both of these metrics are related to light absorbing and light scattering consituents in the water column."),
			  selectInput("clarity_choice_x","Parameter x:",choices=clarity_data_choices_x, selected="Turbidity"),
			  selectInput("clarity_choice_y","Parameter y:",choices=clarity_data_choices_y, selected="k"),
			  radioButtons(inputId = "light_fit",
			               label = "Line of best fit",
			               choiceNames=c("On","Off"),
			               choiceValues=c(1,0),selected=0,inline=TRUE),
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
				plotOutput(outputId = "phyto_output",width="1000px",height="800px")
			),
			conditionalPanel(
				condition="input.tabs==6 & input.phyto_plot_type==2",
				leafletOutput("phyto_map_output",width="800px",height="800px")
			), 
			conditionalPanel(
			  condition = "input.tabs == 7 & input.sonde_data_plot_type == 1",
			  plotOutput("sonde_data_scatterplot", width="800px", height="500px")
			),
			conditionalPanel(
			  condition = "input.tabs == 7 & input.sonde_data_plot_type == 2",
			  plotOutput("sonde_data_boxplot", width="800px", height="500px")
			),
			conditionalPanel(
			  condition = "input.tabs == 8",
			  br(),
			  br(),
			  withMathJax(uiOutput("theoretical_wave_shear")),
			  br(),
			  br(),
			  plotOutput("wind_data_plot", width="800px", height="500px")
			),
			conditionalPanel(
			  condition = "input.tabs == 9 & input.macrophyte_stations == 1",
			  br(),
			  br(),
			  textOutput("shear_exceeded_utah"),
			  br(),
			  br(),
			  plotOutput("macrophyte_data_plot_utah", width="800px", height="500px")
			), 
			conditionalPanel(
			  condition = "input.tabs == 9 & input.macrophyte_stations == 2",
			  br(),
			  br(),
			  textOutput("shear_exceeded_provo"),
			  br(),
			  br(),
			  plotOutput("macrophyte_data_plot_provo", width="800px", height="500px")
			), 
			conditionalPanel(
			  condition = "input.tabs == 9 & input.macrophyte_stations == 3",
			  br(),
			  br(),
			  textOutput("shear_exceeded"),
			  br(),
			  br(),
			  plotOutput("macrophyte_data_plot", width="800px", height="500px")
			),
			conditionalPanel(
			  condition = "input.tabs == 10 & input.PAR_data_stations == 1",
			  plotOutput("PAR_data_plot_utah", width="800px", height="600px"),
			  plotOutput("clarity_data_plot_utah",  width="400px", height="400px")

			),
			conditionalPanel(
			  condition = "input.tabs == 10 & input.PAR_data_stations == 2",
			  plotOutput("PAR_data_plot_provo", width="800px", height="600px"),
			  plotOutput("clarity_data_plot_provo",  width="400px", height="400px")

			),
			conditionalPanel(
			  condition = "input.tabs == 10 & input.PAR_data_stations == 3",
			  plotOutput("PAR_data_plot", width="800px", height="600px"),
			  plotOutput("clarity_data_plot",  width="400px", height="400px")

			)

		)
		
	)

)



###################################*
#### Server ####
###################################* xxx

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
		selectInput("chem_param1", "Parameter:", reactive_objects$param_choices, selected=input$chem_param1,selectize=T)
	})
    
	output$chem_param2 <- renderUI({
		selectInput("chem_param2", "Parameter:", reactive_objects$param_choices, selected=input$chem_param2,selectize=T)
	})
   
	observe({
		req(input$chem_param1)
		req(input$chem_param2)
		reactive_objects$chem_frac1_choices=unique(reactive_objects$wq_plot_data$Fraction[reactive_objects$wq_plot_data$Parameter==input$chem_param1])
		reactive_objects$chem_frac2_choices=unique(reactive_objects$wq_plot_data$Fraction[reactive_objects$wq_plot_data$Parameter==input$chem_param2])
	})

	output$chem_frac1 <- renderUI({
		selectInput("chem_frac1", "Fraction:", reactive_objects$chem_frac1_choices, selected=input$chem_frac1,selectize=T)
	})


	output$chem_frac2 <- renderUI({
		selectInput("chem_frac2", "Fraction:", reactive_objects$chem_frac2_choices, selected=input$chem_frac2,selectize=T)
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
		selectInput("reldepth1", "Sample depth:", reactive_objects$chem_reld1_choices, selected=input$reldepth1,selectize=T)
	})

	output$reldepth2 <- renderUI({
		selectInput("reldepth2", "Sample depth:", reactive_objects$chem_reld2_choices, selected=input$reldepth2,selectize=T)
	})


	output$map_param <- renderUI({
		selectInput("map_param", "Parameter:", reactive_objects$param_choices, selected=input$map_param,selectize=T)
	})

	observe({
		req(input$map_param)
		reactive_objects$map_frac_choices=unique(reactive_objects$wq_plot_data$Fraction[reactive_objects$wq_plot_data$Parameter==input$map_param])
	})

	output$map_frac <- renderUI({
		selectInput("map_frac", "Fraction:", reactive_objects$map_frac_choices, selected=input$map_frac,selectize=T)
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
				if(input$abd_bv==2){
					samples=data.frame(unique(phyto_plot_data[!is.na(phyto_plot_data$CellVolume_u3mL),c("Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude","Date","Year","Month")]))
				}
				else{
					samples=data.frame(unique(phyto_plot_data[!is.na(phyto_plot_data$CellperML),c("Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude","Date","Year","Month")]))				
				}
				
				
				#ID all divisions
				divisions=data.frame(unique(phyto_data[,c("Division")]))
				names(divisions)="Division"
				
				#ID all genera
				genera=data.frame(unique(phyto_data[,c("Genus")]))
				names(genera)="Genus"
				
				#Rename selected response var (abundance or biovolume)
				if(input$abd_bv==1 | input$phyto_samp_type=="HAB"){ #abd
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
					
					#Subset to selected division (if stack_divs==0 or input$phyto_plot_type==2)
					if(input$stack_divs==0 | input$phyto_plot_type==2){
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
			
			#Update input.adv_bv to abundance if sample type is HAB
			
			#Set response
			if((input$abd_bv==1 & input$abund_relabund=="Abundance") | (input$abd_bv==2 & input$bv_rbv=="Biovolume")){
				agg_phyto_plot_data$response=agg_phyto_plot_data$raw_response
			}else{
				agg_phyto_plot_data$response=agg_phyto_plot_data$rel_response
			}
			if(input$phyto_samp_type=="HAB"){
				if(input$abund_relabund=="Abundance"){
					agg_phyto_plot_data$response=agg_phyto_plot_data$raw_response
				}else{
					agg_phyto_plot_data$response=agg_phyto_plot_data$rel_response
				}
			
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
				if(input$stack_divs==1 & input$phyto_plot_type!=2){
						ycomp1="Division"
				}else{
					ycomp1=input$division
				}
			}
			
			if(input$abd_bv==1 | input$phyto_samp_type=="HAB"){
				if(input$abund_relabund=="Abundance"){
					ycomp2="abundance (cells/mL)"
				}else{
					ycomp2="relative abundance"
				}
			}else{
				if(input$bv_rbv=="Biovolume"){
					ycomp2="biovolume (u3/mL)"
				}else{
					ycomp2="relative biovolume"
				}
			}

			ylabel=paste(ycomp1,ycomp2)

			reactive_objects$phyto_ylab=ylabel

			#print(ylabel)
			#print(head(agg_phyto_plot_data))
			
	})
	

	output$map_reldepth <- renderUI({
		selectInput("map_reldepth", "Sample depth:", reactive_objects$map_reldepth_choices, selected=input$map_reldepth,selectize=T)
	})

	output$genus <- renderUI({
		selectInput("genus","Genus:",choices=unique(phyto_data$Genus)[order(unique(phyto_data$Genus))], selected=input$genus,selectize=T)
	})
	
	output$division <- renderUI({
		selectInput("division","Algal division:",choices=unique(phyto_data$Division)[order(unique(phyto_data$Division))], selected=input$division,selectize=T)
	})
	

	filtered_sonde_data <- reactive({
	  sonde_data %>%
	    filter(Month >= input$sonde_data_plot_months[1] & Month <= input$sonde_data_plot_months[2]) %>%
	    filter(SiteCode %in% input$sonde_data_stations)
	  
	})
	
	datasetsondeInput <- reactive({
	  sonde_data %>%
	    filter(Month >= input$sonde_data_plot_months[1] & Month <= input$sonde_data_plot_months[2]) %>%
	    filter(SiteCode %in% input$sonde_data_stations)	})
	
	filtered_wind_data <- reactive({
	  wind_data %>%
	    filter(SiteCode %in% input$wind_data_stations)
	})	
	
	theoretical_wave_shear <- reactive({
	    round(digits = 4, 
	          ((input$shear_calculation_wind^2/9.80665) * 
	           (0.283*tanh(0.53*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/4)) *
	            tanh(0.00565*(9.80665*input$shear_calculation_fetch*1000/(input$shear_calculation_wind^2))^(0.5)/
	            (tanh(0.53*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8)))))) * 1000 * 
	            ((0.00000105324176448 * (2*pi/((input$shear_calculation_wind/9.80665) * 
	            (7.54*tanh(0.833*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8)) *
	             tanh(0.0379*(9.80665*input$shear_calculation_fetch*1000/(input$shear_calculation_wind^2))^(0.5)/
	             (tanh(0.833*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8)))))))^3)^(0.5))/
	            (2*sinh(2*pi*input$shear_calculation_depth/(9.80665 * ((input$shear_calculation_wind/9.80665) * 
	             (7.54*tanh(0.833*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8)) *
	             tanh(0.0379*(9.80665*input$shear_calculation_fetch*1000/(input$shear_calculation_wind^2))^(0.5)/
	             (tanh(0.833*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8))))))^2 / (2*pi) * 
	             sqrt(tanh((4*pi^2*input$shear_calculation_depth/(((input$shear_calculation_wind/9.80665) * 
	             (7.54*tanh(0.833*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8)) *
	              tanh(0.0379*(9.80665*input$shear_calculation_fetch*1000/(input$shear_calculation_wind^2))^(0.5)/
	              (tanh(0.833*(9.80665*input$shear_calculation_depth/(input$shear_calculation_wind^2))^(3/8))))))^2*9.80665))))))))
	})
	
	adjusted_wind_data <- reactive({
	  wind_data %>%
	    mutate(tau.wind.calculated = tau.wind * (100-input$shear_reduction)/100)
	})
	
	samples_exceeding_critical_shear <- reactive({
	  adjusted_wind_data() %>%
	  drop_na(tau.wind.calculated) %>%
	  summarise(percent = length(tau.wind.calculated[tau.wind.calculated >0.16])/length(tau.wind.calculated)*100) %>%
	    round(2)
	})
	

	adjusted_wind_data_provo <- reactive({
	  wind_data %>%
	    filter(SiteCode == 4917446) %>%
	    mutate(tau.wind.calculated = tau.wind * (100-input$shear_reduction)/100)
	})
	
	samples_exceeding_critical_shear_provo <- reactive({
	  adjusted_wind_data_provo() %>%
	    drop_na(tau.wind.calculated) %>%
	    summarise(percent = length(tau.wind.calculated[tau.wind.calculated >0.16])/length(tau.wind.calculated)*100) %>%
	    round(2)
	})

	adjusted_wind_data_utah <- reactive({
	  wind_data %>%
	    filter(SiteCode != 4917446) %>%
	    mutate(tau.wind.calculated = tau.wind * (100-input$shear_reduction)/100)
	})
	
	samples_exceeding_critical_shear_utah <- reactive({
	  adjusted_wind_data_utah() %>%
	    drop_na(tau.wind.calculated) %>%
	    summarise(percent = length(tau.wind.calculated[tau.wind.calculated >0.16])/length(tau.wind.calculated)*100) %>%
	    round(2)
	})

	filtered_PAR_data <- reactive({
	  PAR_data %>%
	    filter(Month >= input$PAR_data_plot_months[1] & Month <= input$PAR_data_plot_months[2])
	})

	filtered_PAR_data_provo <- reactive({
	  PAR_data %>%
	    filter(Month >= input$PAR_data_plot_months[1] & Month <= input$PAR_data_plot_months[2]) %>%
	    filter(MonitoringLocationIdentifier %in% c(4917775, 4917452, 4917458, 4917450, 4917455,
	                                               4917460, 4917454, 4917470, 4917450, 4917470))
	})

	filtered_PAR_data_utah <- reactive({
	  PAR_data %>%
	    filter(Month >= input$PAR_data_plot_months[1] & Month <= input$PAR_data_plot_months[2]) %>%
	    filter(!(MonitoringLocationIdentifier %in% c(4917775, 4917452, 4917458, 4917450, 4917455,
	                                                 4917460, 4917454, 4917470, 4917450, 4917470)))
	})
	
	filtered_clarity_data <- reactive({
	  clarity_data %>%
	    filter(Month >= input$PAR_data_plot_months[1] & Month <= input$PAR_data_plot_months[2])
	})
	
	filtered_clarity_data_provo <- reactive({
	  clarity_data %>%
	    filter(Month >= input$PAR_data_plot_months[1] & Month <= input$PAR_data_plot_months[2] )%>%
	    filter(MonitoringLocationIdentifier %in% c(4917775, 4917452, 4917458, 4917450, 4917455,
	                                                 4917460, 4917454, 4917470, 4917450, 4917470))
	  
	})
	
	filtered_clarity_data_utah <- reactive({
	  clarity_data %>%
	    filter(Month >= input$PAR_data_plot_months[1] & Month <= input$PAR_data_plot_months[2]) %>%
	    filter(!(MonitoringLocationIdentifier %in% c(4917775, 4917452, 4917458, 4917450, 4917455,
	                                                 4917460, 4917454, 4917470, 4917450, 4917470)))
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
	
			#Plot type 2: boxplots
		if(input$chem_plot_type==2){
			par(mar=c(16,8,4.1,3))
			if(dim(plot_data)[1]>0){
				if(plot_log=="y" | plot_log=="xy"){
					boxplot(Result.Value~Monitoring.Location.ID,plot_data,cex.axis=2,cex.lab=2.5,ylab=ylabel,xlab=NA,main=title,cex.main=2,yaxt='n',las=2,log='y')
				}else{
					boxplot(Result.Value~Monitoring.Location.ID,plot_data,cex.axis=2,cex.lab=2.5,ylab=ylabel,xlab=NA,main=title,cex.main=2,yaxt='n',las=2)
				}
				axis(2,cex.axis=2) # xxx
			}
		}
			
			#Plot type 3: pairwise
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
	
	output$downloadchemData <- downloadHandler(
	  filename = function() {
	    paste(input$dataset, ".csv", sep = "")
	  },
	  content = function(file) {
	    write.csv(wq_data, file, row.names = FALSE)
	  }
	)
	
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
	
	#Genus/Division time series (single taxon) (consider adding log y capability - will need to transform response w/ +1 or +0.01 etc), need to work on stacked plot legend placement/colors
	output$phyto_output<-renderPlot({
		req(reactive_objects$agg_phyto_plot_data)
		agg_phyto_plot_data=reactive_objects$agg_phyto_plot_data
		ylabel=reactive_objects$phyto_ylab
		par(mfrow=c(2,1),mar=c(4.1,6.1,4.1,20.1))
		if(dim(reactive_objects$phyto_plot_data)[1]>0){
			if(input$phyto_plot_type==1){	
				if(input$genus_or_division==2 & input$stack_divs==1){ #stacked division plot
					xleg_yr=input$phyto_plot_years[2]+((input$phyto_plot_years[2]-input$phyto_plot_years[1])/(max(phyto_data$Year, na.rm=T)-min(phyto_data$Year, na.rm=T)))
					xleg_mon=input$phyto_plot_months[2]+((input$phyto_plot_months[2]-input$phyto_plot_months[1])/(15))
					pal_len=max(c(length(levels(agg_phyto_plot_data$group)),8))
					cols=brewer.pal(pal_len, name="Dark2")
					par(xpd=TRUE)
					suppressWarnings(
						lineplot.CI(Year, response, group, data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_years[1],input$phyto_plot_years[2]),cex=1.5,
							ylab=ylabel,xlab="Year",cex.lab=1.5,cex.axis=1.5,err.width=0.05,col=cols, xaxt='n',fixed=T, cex.leg=1.5, x.leg=xleg_yr)
					)
					axis(1,cex.axis=1.5,cex.lab=2)
					suppressWarnings(
						lineplot.CI(Month, response, group, data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_months[1],input$phyto_plot_months[2]),cex=1.5,
							ylab=ylabel,xlab="Month",cex.lab=1.5,cex.axis=1.5,err.width=0.05,col=cols, xaxt='n',fixed=T, cex.leg=1.5, x.leg=xleg_mon)
					)
					axis(1,cex.axis=1.5,cex.lab=2)
					par(xpd=FALSE)
				}else{ #All single plots
					suppressWarnings(
						lineplot.CI(Year, response, group, data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_years[1],input$phyto_plot_years[2]),cex=1.5,
							ylab=ylabel,xlab="Year",cex.lab=1.5,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
					)
					axis(1,cex.axis=1.5,cex.lab=2)
					suppressWarnings(
						lineplot.CI(Month, response, group,data=agg_phyto_plot_data,x.cont=TRUE,xlim=c(input$phyto_plot_months[1],input$phyto_plot_months[2]),cex=1.5,
							ylab=ylabel,xlab="Month",cex.lab=1.5,cex.axis=1.5,legend=F,err.width=0.05,pch=21,col="blue",lwd=2,xaxt='n')
					)
					axis(1,cex.axis=1.5,cex.lab=2)
				}
			}
			
		}else{
			frame()
			box()
			text(0.5,0.5,"No data for selected years & months.", cex=1.75)
			frame()
			box()
			text(0.5,0.5,"No data for selected years & months.", cex=1.75)
		}
	})
		
		
	output$phyto_map_output <- renderLeaflet({
		req(reactive_objects$agg_phyto_plot_data)
		req(reactive_objects$phyto_plot_data)
		agg_phyto_plot_data=reactive_objects$agg_phyto_plot_data
        		
		addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){ ###From: https://stackoverflow.com/questions/37446283/creating-legend-with-circles-leaflet-r
			colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
			labelAdditions <- paste0("<div class = 'legendCircle' style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
			return(addLegend(map, title = legend_title, colors = colorAdditions, labels = labelAdditions))
		}
        
		
		
		#Aggregate data by site
		if(dim(agg_phyto_plot_data)[1]>0){
				agg_phyto_plot_data_site=aggregate(response~Monitoring.Location.ID,agg_phyto_plot_data,FUN='mean')
			#Generate sites
			sites=unique(agg_phyto_plot_data[,c("Monitoring.Location.ID","Monitoring.Location.Latitude","Monitoring.Location.Longitude")])
			
			legend_title=reactive_objects$phyto_ylab
			
			names(agg_phyto_plot_data_site)[names(agg_phyto_plot_data_site)=="response"]="z_val"
			agg_phyto_plot_data_site=merge(agg_phyto_plot_data_site,sites)
			
			
			
			####Fixed z-scaling
			if(input$phyto_z_fixed_dynamic=="Fixed"){
				if(input$phyto_samp_type=="Total phytoplankton"){
					if((input$abd_bv==1 & input$abund_relabund=="Relative abundance") | (input$abd_bv==2 & input$bv_rbv=="Relative biovolume")){
						agg_phyto_plot_data_site$size=(agg_phyto_plot_data_site$z_val+0.1)*0.5
						legend_labs=c(0,0.25,0.5,0.75,1)
						legend_sizes=((legend_labs+0.1))*75
					}else{
						if(input$abd_bv==1){
							if(input$genus_or_division==1){
								temp_agg=aggregate(CellperML~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Genus, phyto_data, FUN='sum')
							}else{
								temp_agg=aggregate(CellperML~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Division, phyto_data, FUN='sum')
							}
							fixed_scale<-data.frame(phyto_data[,"CellperML"],rescale(log10(phyto_data[,"CellperML"]+1),c(0.1,0.5)))
						}else{
							if(input$genus_or_division==1){
								temp_agg=aggregate(CellVolume_u3mL~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Genus, phyto_data, FUN='sum')
							}else{
								temp_agg=aggregate(CellVolume_u3mL~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Division, phyto_data, FUN='sum')
							}
							fixed_scale<-data.frame(temp_agg[,"CellVolume_u3mL"],rescale(log10(temp_agg[,"CellVolume_u3mL"]+1),c(0.1,0.5)))
						}
						names(fixed_scale)=c("response","fixed_scale")
						fixed_scale_lm=lm(fixed_scale~log10(response+1),fixed_scale)
						agg_phyto_plot_data_site$response=agg_phyto_plot_data_site$z_val
						agg_phyto_plot_data_site$size=predict(fixed_scale_lm,newdata=agg_phyto_plot_data_site)
						legend_labs=c(min(fixed_scale$response, na.rm=T),quantile(fixed_scale$response,0.5, na.rm=T),quantile(fixed_scale$response,0.9, na.rm=T),quantile(fixed_scale$response,0.95, na.rm=T),quantile(fixed_scale$response,0.99, na.rm=T),quantile(fixed_scale$response,0.999, na.rm=T))
						legend_labs=signif(legend_labs,2)
						legend_sizes=c(min(fixed_scale$fixed_scale, na.rm=T),quantile(fixed_scale$fixed_scale,0.5, na.rm=T),quantile(fixed_scale$fixed_scale,0.9, na.rm=T),quantile(fixed_scale$fixed_scale,0.95, na.rm=T),quantile(fixed_scale$fixed_scale,0.99, na.rm=T),quantile(fixed_scale$fixed_scale,0.999, na.rm=T))*75
						legend_sizes=legend_sizes*2	
					}
				}
				if(input$phyto_samp_type=="HAB"){
					if(input$abund_relabund=="Relative abundance"){
						agg_phyto_plot_data_site$size=(agg_phyto_plot_data_site$z_val+0.1)*0.5
						legend_labs=c(0,0.25,0.5,0.75,1)
						legend_sizes=((legend_labs+0.1))*75
					}else{
						if(input$genus_or_division==1){
							temp_agg=aggregate(CellperML~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Genus, phyto_data, FUN='sum')
						}else{
							temp_agg=aggregate(CellperML~Monitoring.Location.ID+Monitoring.Location.Latitude+Monitoring.Location.Longitude+Date+Year+Month+Division, phyto_data, FUN='sum')
						}
						fixed_scale<-data.frame(phyto_data[,"CellperML"],rescale(log10(phyto_data[,"CellperML"]+1),c(0.1,0.5)))
					names(fixed_scale)=c("response","fixed_scale")
					fixed_scale_lm=lm(fixed_scale~log10(response+1),fixed_scale)
					agg_phyto_plot_data_site$response=agg_phyto_plot_data_site$z_val
					agg_phyto_plot_data_site$size=predict(fixed_scale_lm,newdata=agg_phyto_plot_data_site)
					legend_labs=c(min(fixed_scale$response, na.rm=T),quantile(fixed_scale$response,0.5, na.rm=T),quantile(fixed_scale$response,0.9, na.rm=T),quantile(fixed_scale$response,0.95, na.rm=T),quantile(fixed_scale$response,0.99, na.rm=T),quantile(fixed_scale$response,0.999, na.rm=T))
					legend_labs=signif(legend_labs,2)
					legend_sizes=c(min(fixed_scale$fixed_scale, na.rm=T),quantile(fixed_scale$fixed_scale,0.5, na.rm=T),quantile(fixed_scale$fixed_scale,0.9, na.rm=T),quantile(fixed_scale$fixed_scale,0.95, na.rm=T),quantile(fixed_scale$fixed_scale,0.99, na.rm=T),quantile(fixed_scale$fixed_scale,0.999, na.rm=T))*75
					legend_sizes=legend_sizes*2	
					}
						
				}
			agg_phyto_plot_data_site$radius=agg_phyto_plot_data_site$size*75
			
			}else{
				###Dynamic z-scaling
				agg_phyto_plot_data_site$size=rescale(agg_phyto_plot_data_site$z_val,c(0.1,0.5))
		
				agg_phyto_plot_data_site$radius=agg_phyto_plot_data_site$size*75
				#print(head(agg_phyto_plot_data_site))
		
				legend_labs=c(min(agg_phyto_plot_data_site$z_val),quantile(agg_phyto_plot_data_site$z_val,0.25),quantile(agg_phyto_plot_data_site$z_val,0.5),quantile(agg_phyto_plot_data_site$z_val,0.75),max(agg_phyto_plot_data_site$z_val))
				legend_labs=signif(legend_labs,2)
				legend_sizes=c(min(agg_phyto_plot_data_site$radius),quantile(agg_phyto_plot_data_site$radius,0.25),quantile(agg_phyto_plot_data_site$radius,0.5),quantile(agg_phyto_plot_data_site$radius,0.75),max(agg_phyto_plot_data_site$radius))
				legend_sizes=legend_sizes*2
				
			}
			
			legend=unique(data.frame(legend_sizes,legend_labs))
			phyto_points=st_as_sf(agg_phyto_plot_data_site, coords = c("Monitoring.Location.Longitude", "Monitoring.Location.Latitude"), crs = 4326, remove=FALSE) # crs 4326 is WGS84
		}
		
		if(dim(reactive_objects$phyto_plot_data)[1]>0){
			phyto_map=leaflet(phyto_points) %>%
				addTiles() %>%
				addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
				addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
				addPolygons(data=ul_poly,smoothFactor=2,fillOpacity = 0,weight=3,color="lightskyblue") %>%
				addFeatures(phyto_points,color="orange",radius=phyto_points$radius, stroke=F,fillOpacity=0.5) %>%
				addFeatures(phyto_points,color="orange",stroke=F,fillOpacity=0,
					popup = paste0(
						"MLID: ", phyto_points$Monitoring.Location.ID,
						"<br> Value: ", signif(phyto_points$z_val,2))
				) %>%			
	    
				addLegendCustom(colors = rep("orange", dim(legend)[1]), labels = legend$legend_labs, sizes = legend$legend_sizes) %>%
	    
				addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),
					options = layersControlOptions(collapsed = FALSE, autoZIndex=FALSE))
		}else{
			phyto_map=leaflet() %>%
				addTiles() %>%
				addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
				addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
				addPolygons(data=ul_poly,smoothFactor=2,fillOpacity = 0,weight=3,color="lightskyblue") %>%
				addLayersControl(
					position ="topleft",
					baseGroups = c("Topo","Satellite"),
					options = layersControlOptions(collapsed = FALSE, autoZIndex=FALSE))
		
		}	

	})

	
	#Tab 7: Sonde plot outputs

	output$sonde_data_scatterplot <- renderPlot({
	    ggplot(data = filtered_sonde_data(), 
	           aes_string(x = "date", y = input$sonde_choice_y, color = "as.factor(SiteCode)")) +
	      geom_point(size = 2, alpha = 0.8) +
	      scale_color_viridis_d(option = "magma", end = 0.8, begin = 0.2) +
	      scale_x_date(date_breaks = "2 months", date_labels = "%b-%y") +
	      theme_classic(base_size = 20) +
	      theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
	      labs(x = "", color = "Station", 
	           y = paste("\n", names(sonde_data_choices[sonde_data_choices == input$sonde_choice_y]))) + 
	      if(input$sonde_data_y_axis == 2) {scale_y_log10()} else 
	        if(input$sonde_data_y_axis == 1) {scale_y_continuous()} 
	      
	})
	
	output$sonde_data_boxplot <- renderPlot({
	  ggplot(data = filtered_sonde_data(), 
	         aes_string(x = "as.factor(Month)", y = input$sonde_choice_y, fill = "as.factor(SiteCode)")) +
	      geom_boxplot(alpha = 0.8) +
	      scale_fill_viridis_d(option = "magma", end = 0.8, begin = 0.2) +
	      theme_classic(base_size = 20) +
	      theme(legend.position = "top") +
	      labs(x = "Month", fill = "Station", 
	           y = paste("\n", names(sonde_data_choices[sonde_data_choices == input$sonde_choice_y])))+ 
	    if(input$sonde_data_y_axis == 2) {scale_y_log10()} else 
	      if(input$sonde_data_y_axis == 1) {scale_y_continuous()} 
	  
	})
	
	output$downloadsondeData <- downloadHandler(
	  filename = function() {
	    paste(input$dataset, ".csv", sep = "")
	  },
	  content = function(file) {
	    write.csv(datasetsondeInput(), file, row.names = FALSE)
	  }
	)
	
	# Tab 8: Wind plot outputs
	
	output$theoretical_wave_shear <- renderUI({
	  withMathJax(paste0("$$\\text{Theoretical wave shear (N/m }^2 ) =", theoretical_wave_shear(),"$$"))
	 })
	
	
	output$wind_data_plot <- renderPlot({
	  ggplot(data = filtered_wind_data(), 
	         aes_string(x = input$wind_choice_x, y = input$wind_choice_y,  color = "as.factor(SiteCode)")) +
	    geom_point(alpha = 0.8, size = 2) + 
	    scale_color_viridis_d(option = "magma", end = 0.8, begin = 0.2) +
	    labs(color = "Station", 
	         x = paste("\n", names(wind_data_choices1[wind_data_choices1 == input$wind_choice_x])),  
	         y = paste("\n", names(wind_data_choices2[wind_data_choices2 == input$wind_choice_y]))) +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "top") +
	    if(input$wind_choice_x == "tau.wind") {geom_vline(xintercept = 0.16, lty = 2, size = 1)} else
	    if(input$wind_choice_y == "tau.wind") {geom_hline(yintercept = 0.16, lty = 2, size = 1)}
	    
	})
	
#	Tab 9: Macrophyte plot outputs
	output$shear_exceeded <- renderText({
	  paste(samples_exceeding_critical_shear(), "% of samples exceed critical shear")
	})
	
	output$macrophyte_data_plot <- renderPlot({
	  ggplot(data = adjusted_wind_data(), 
	         aes_string(x = "tau.wind.calculated")) +
	    geom_density(fill = "#fc9d6fff", alpha = 0.8) +
	    geom_vline(xintercept = 0.16, lty = 2, size = 1) +
	    labs(x = expression("Wave shear (N/m"^2*")")) +
	  	theme_classic(base_size = 20) +
	    scale_x_continuous(expand = c(0, 0)) + 
	    scale_y_continuous(expand = c(0, 0))
	})
	
	output$shear_exceeded_provo <- renderText({
	  paste(samples_exceeding_critical_shear_provo(), "% of samples exceed critical shear")
	})
	
	output$macrophyte_data_plot_provo <- renderPlot({
	  ggplot(data = adjusted_wind_data_provo(), 
	         aes_string(x = "tau.wind.calculated")) +
	    geom_density(fill = "#fc9d6fff", alpha = 0.8) +
	    geom_vline(xintercept = 0.16, lty = 2, size = 1) +
	    theme_classic(base_size = 20) +
	    scale_x_continuous(expand = c(0, 0)) + 
	    scale_y_continuous(expand = c(0, 0))
	})
	
	output$shear_exceeded_utah <- renderText({
	  paste(samples_exceeding_critical_shear_utah(), "% of samples exceed critical shear")
	})
	
	output$macrophyte_data_plot_utah <- renderPlot({
	  ggplot(data = adjusted_wind_data_utah(), 
	         aes_string(x = "tau.wind.calculated")) +
	    geom_density(fill = "#fc9d6fff", alpha = 0.8) +
	    geom_vline(xintercept = 0.16, lty = 2, size = 1) +
	    theme_classic(base_size = 20) +
	    scale_x_continuous(expand = c(0, 0)) + 
	    scale_y_continuous(expand = c(0, 0))
	})
	
	
	# Tab 10: Light and Clarity
	output$PAR_data_plot <- renderPlot({
	  ggplot(filtered_PAR_data(), aes_string(x = "ResultMeasureValue", y = "SampleDepthValue", color = "Month")) +
	    geom_point(size = 3) +
	    geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, aes(group = Month)) +
	    scale_y_reverse() +
	    facet_wrap(vars(MonitoringLocationIdentifier), scales = "free_y") +
	    scale_color_viridis_c(option = "magma", end = 0.8, direction = -1) +
	    labs(x = expression(PAR ~ (mu*mol ~ m^{-2} ~ s^{-1})), y = "Depth (m)") +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "top")
	})
	
	output$PAR_data_plot_provo <- renderPlot({
	  ggplot(filtered_PAR_data_provo(), aes_string(x = "ResultMeasureValue", y = "SampleDepthValue", color = "Month")) +
	    geom_point(size = 3) +
	    geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, aes(group = Month)) +
	    scale_y_reverse() +
	    facet_wrap(vars(MonitoringLocationIdentifier), scales = "free_y") +
	    scale_color_viridis_c(option = "magma", end = 0.8, direction = -1) +
	    labs(x = expression(PAR ~ (mu*mol ~ m^{-2} ~ s^{-1})), y = "Depth (m)") +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "top")
	})
	
	output$PAR_data_plot_utah <- renderPlot({
	  ggplot(filtered_PAR_data_utah(), aes_string(x = "ResultMeasureValue", y = "SampleDepthValue", color = "Month")) +
	    geom_point(size = 3) +
	    geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, aes(group = Month)) +
	    scale_y_reverse() +
	    facet_wrap(vars(MonitoringLocationIdentifier), scales = "free_y") +
	    scale_color_viridis_c(option = "magma", end = 0.8, direction = -1) +
	    labs(x = expression(PAR ~ (mu*mol ~ m^{-2} ~ s^{-1})), y = "Depth (m)") +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "top")
	})
	  
	output$clarity_data_plot <- renderPlot({
	  ggplot(data = filtered_clarity_data(),
	         aes_string(x = input$clarity_choice_x, y = input$clarity_choice_y, color = "Month")) +
	    geom_point(size = 3) +
	    scale_color_viridis_c(option = "magma", end = 0.8, direction = -1) +
	    labs(x = paste("\n", names(clarity_data_choices_x[clarity_data_choices_x == input$clarity_choice_x])), 
	         y = paste("\n", names(clarity_data_choices_y[clarity_data_choices_y == input$clarity_choice_y]))) +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "none") +
	    if(input$clarity_choice_y == "k" & input$light_fit == 1) 
	    {geom_smooth(method = lm, se = FALSE, color = "black")} else
	      if(input$clarity_choice_y == "secchidepth.m" & input$light_fit == 1) 
	      {geom_smooth(method = lm, se = FALSE, color = "black", formula = y ~ log(x))}
	})
	
	clarity_data_choices_x
	
	output$clarity_data_plot_provo <- renderPlot({
	  ggplot(data = filtered_clarity_data_provo(),
	         aes_string(x = input$clarity_choice_x, y = input$clarity_choice_y, color = "Month")) +
	    geom_point(size = 3) +
	    scale_color_viridis_c(option = "magma", end = 0.8, direction = -1) +
	    labs(x = paste("\n", names(clarity_data_choices_x[clarity_data_choices_x == input$clarity_choice_x])), 
	         y = paste("\n", names(clarity_data_choices_y[clarity_data_choices_y == input$clarity_choice_y]))) +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "none") +
	    if(input$clarity_choice_y == "k" & input$light_fit == 1) 
	    {geom_smooth(method = lm, se = FALSE, color = "black")} else
	      if(input$clarity_choice_y == "secchidepth.m" & input$light_fit == 1) 
	      {geom_smooth(method = lm, se = FALSE, color = "black", formula = y ~ log(x))}
	})
	
	output$clarity_data_plot_utah <- renderPlot({
	  ggplot(data = filtered_clarity_data_utah(),
	         aes_string(x = input$clarity_choice_x, y = input$clarity_choice_y, color = "Month")) +
	    geom_point(size = 3) +
	    scale_color_viridis_c(option = "magma", end = 0.8, direction = -1) +
	    labs(x = paste("\n", names(clarity_data_choices_x[clarity_data_choices_x == input$clarity_choice_x])), 
	         y = paste("\n", names(clarity_data_choices_y[clarity_data_choices_y == input$clarity_choice_y]))) +
	    theme_classic(base_size = 20) +
	    theme(legend.position = "none") +
	    if(input$clarity_choice_y == "k" & input$light_fit == 1) 
	    {geom_smooth(method = lm, se = FALSE, color = "black")} else
	      if(input$clarity_choice_y == "secchidepth.m" & input$light_fit == 1) 
	      {geom_smooth(method = lm, se = FALSE, color = "black", formula = y ~ log(x))}
	})

		
}

# Run app
shinyApp(ui = ui, server = server)
