

###################################*
#### Utah Lake Data Explorer - Data Processing ####
###################################*


###################################*
#### Contact Info ####
###################################*

# Jake Vander Lann (jvander@utah.gov)
# Mark Fernandez (mark.fernandez@tetratech.com)




###################################.
#### Citations ####
###################################.

# R Citation
citation()
# R Core Team. 2019. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

citation('leaflet')
# Cheng, J., B. Karambelkar and Y. Xie. 2018. leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package version 2.0.2. https://CRAN.R-project.org/package=leaflet






###################################*
#### Load Packages ####
###################################*

# Load packages
# library(openxlsx)
# library(visdat)
library(data.table)
library(leaflet)
library(ggmap)
library(sf)
# detach(package:aaa,unload=T)


# Clean workspace
rm(list=ls()); gc()

# NA strings to search for
na.strings=c(NA,'NA','',' ','<Null>','None')
# NOTE: "translate_params.csv" requires "N/A" values for the Shiny app, so do not include "N/A" above.

# Set Elevation below Compromise
compElev=4489.045

# Source custom functions
source('../functions/calcTSI.R')







###################################*
#### **** Load Data **** ####
###################################*


###################################*
#### Stations ####
###################################*

# Load data
sites=fread('DataInput/Stations.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(sites) # 49 xx

# Convert to character
sites[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]

# Add prefix to SiteID
sites[,MonitoringLocationIdentifier:=Monitoring.Location.ID] # Reset
sites[MonitoringLocationIdentifier!='7458',MonitoringLocationIdentifier:=paste0('UTAHDWQ_WQX-',MonitoringLocationIdentifier)]
sites[MonitoringLocationIdentifier=='7458',MonitoringLocationIdentifier:=paste0('NALMS-',MonitoringLocationIdentifier)]

# Sort
setorder(sites,Monitoring.Location.ID)





###################################*
#### Load Basemap ####
###################################*

# Load the basemap image
# load('Basemap.RData')

# Set API key - Once per R session
# has_google_key() # TRUE

# Download map
# zoom=10L
# tmp=c(-111.83,40.20) # Center of Utah Lake
# basemap=list()
# basemap$terrain=get_map(location=tmp,zoom=zoom,maptype='terrain')
# basemap$hybrid=get_map(location=tmp,zoom=zoom,maptype='hybrid')
# basemap$satellite=get_map(location=tmp,zoom=zoom,maptype='satellite')

# Save map
# save(basemap,file='Basemap.RData')






###################################*
#### Map - All Sites ####
###################################*

# leaflet Map
leaflet(sites) %>%
	addCircles(lat=~Monitoring.Location.Latitude,lng=~Monitoring.Location.Longitude,radius=500,stroke=T,weight=1,opacity=1,color=1,fillColor='dodgerblue',fillOpacity=1) %>%
	# addMarkers(lat=~Monitoring.Location.Latitude,lng=~Monitoring.Location.Longitude) %>%
	addProviderTiles('Esri.WorldImagery') %>% # Esri.WorldImagery Esri.WorldTopoMap Esri.NatGeoWorldMap OpenStreetMap
	setView(lng=-111.83,lat=40.20,zoom=11) %>%
	fitBounds(lng1=-112.02,lng2=-111.62,lat1=40.00,lat2=40.40)

# ggmap Map
png(filename='Map_Sites_ggmap.png',width=4.0,height=5.2,units='in',res=300,type='cairo',pointsize=9)
ggmap(basemap$terrain)+
	geom_point(data=sites,mapping=aes(x=Monitoring.Location.Longitude,y=Monitoring.Location.Latitude,fill=as.factor(Provo)),size=3,shape=24)+
	coord_map('mercator',xlim=c(-112.02,-111.62),ylim=c(40.00,40.40))+
	labs(x='Longitude',y='Latitude',title='Monitoring Sites')+
	theme_classic()+
	theme(legend.position='bottom',text=element_text(size=9))
dev.off()





###################################*
#### GIS Layers ####
###################################*

# Load polygon
ul_poly=st_read('../polyrast','UtahLake_poly_wgs84')
ul_poly=st_zm(ul_poly) # Removing 'z' coordinates for leaflet





###################################*
#### Elevation-Storage LUT ####
###################################*

# Dataset currently not used in Shiny app. Keep for reference.

# Load data
storageElev=fread('DataInput/elevation_storage.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(storageElev) # 194 xx

# Rename fields
tmp=c('Depth_ft','Storage_acreFt')
cbind(names(storageElev),tmp)
names(storageElev)=tmp

# Add Elevation
storageElev[,Elevation_ft:=Depth_ft+compElev]

# Sort
setorder(storageElev,Elevation_ft)






###################################*
#### Lake Elevation - Monthly ####
###################################*

# Create Month LUT
monthLUT=data.table(Month=1:12,MonthName=month.abb,MonthLab=toupper(month.abb))

# Load data
lake_elev_data=fread('DataInput/UL_Elevation_Avg_Monthly.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(lake_elev_data) # 1044 xx

# Add Month
lake_elev_data=merge(lake_elev_data,monthLUT[,.(MonthLab,Month)],by='MonthLab',all.x=T) # Left outer join

# Sort
setorder(lake_elev_data,Year,Month)





###################################*
#### Phytoplankton ####
###################################*

# Load data
phyto_data=fread('DataInput/phytoplankton.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(phyto_data) # 9127 xx

# Filter fields
tmp=c('Monitoring.Location.ID','SiteName','Monitoring.Location.Latitude','Monitoring.Location.Longitude','Date','SampleType','Division','Genus','Taxon','CellperML','CellVolume_u3mL')
phyto_data=phyto_data[,..tmp]

# Convert to double (don't use integer)
phyto_data[,CellVolume_u3mL:=as.double(CellVolume_u3mL)]

# Temporarily set Date='2008' to '1/1/2008'
phyto_data[Date=='2008',Date:='1/1/2008']

# Convert to Date
phyto_data[,Date:=as.Date(Date,format='%m/%d/%Y')]

# Add Year and Month
phyto_data[,Year:=year(Date)]
phyto_data[,Month:=month(Date)]

# Remove Date="2008-01-01"
phyto_data[Date==as.Date('2008-01-01'),Date:=NA]

# Check SampleType
phyto_data[,.N,by=SampleType]
#               SampleType    N
# 1: Total Plankton Sample 2188
# 2:   Total Phytoplankton 6410
# 3:                   HAB  529

# Set SampleType values to either "HAB" or "Total phytoplankton"
phyto_data[SampleType!='HAB',SampleType:='Total phytoplankton']

# Check Division
phyto_data[,.N,by=Division]
#           Division    N
# 1:      Cyanophyta 2127
# 2:       Dinophyta  227
# 3:    Euglenophyta  304
# 4: Bacillariophyta 1834
# 5:     Chlorophyta 4391
# 6:           Other    7
# 7:     Cryptophyta  152
# 8:     Chrysophyta   79
# 9:            <NA>    6

# Set Division=NA to Other
phyto_data[is.na(Division),Division:='Other']

# Check Genus
phyto_data[,.N,by=Genus]

# Set Genus=NA to Other
phyto_data[is.na(Genus),Genus:='Other']

# Drop fields (for join)
phyto_data[,Monitoring.Location.Latitude:=NULL]
phyto_data[,Monitoring.Location.Longitude:=NULL]

# Join Sites
intersect(names(phyto_data),names(sites))
phyto_data=merge(phyto_data,sites,by='Monitoring.Location.ID',all.x=T) # Left outer join
phyto_data[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]
dim(phyto_data) # 9127 xx

# Sort
setorder(phyto_data,Monitoring.Location.ID,Date,Division,Genus)





###################################*
#### Parameter LUT ####
###################################*

# Notes:
# ===========================================.
# All Fecal Coliform set to Surface
# All Settleable solids set to Total
# All Turbidity (TURB) set to Fraction=N/A

# NOTE: "translate_params.csv" requires "N/A" values for the Shiny app, so do not include "N/A" in na.strings
# Check for "N/A"
if(any(grepl('N/A',na.strings))) print('STOP - na.string cannot include "N/A"')

# Load data
translate_params=fread('DataInput/translate_params.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(translate_params) # 503 xx

# Sort
setorder(translate_params,Parameter,Fraction,Depth)





###################################*
#### NLA Data ####
###################################*

# Load data
nla_data=fread('DataInput/NLA2012_TrophicData.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(nla_data) # 1230 xx

# Convert to character
nla_data[,UID:=as.character(UID)]

# Calculate TSI
tmp=calcTSI(setDF(nla_data),chl='Chlorophyll_a_ugL',TP='Total_phosphorus_mgL',SD='Secchi_m')
dim(tmp) # 1230 xx

# Join TSI
nla_data=cbind(nla_data,tmp)
setDT(nla_data)

# Sort
setorder(nla_data,UID)





###################################*
#### **** WQP Data **** ####
###################################*

###################################*
#### Process Data ####
###################################*

# Provisional data rejected
# Non-detects at ½ sample detection limit
# All chla included (corrected, uncorrected, and unspecified all translated to Chlorophyll a)
# Samples of chemical parameters missing fraction or depth not included.
# ChlA and TP TSI analyses conducted only on total/surface samples.
# Fraction == Acid Soluble = Total
# All sample depths for Secchi set to N/A
# All pH fractions set to N/A
# All total dissolved solids fractions marked dissolved
# TFS, TSS, and TVS fractions all marked total
# Turbidity fractions all marked N/A
# Settleable solids fraction marked total.
# All fecal coliform sample depths assumed surface.

# Load wq_data pre-processed data
load('WQP/ul_data.RData')
dim(wq_data) # 49422 xx

# Rename fields (to match Shiny code)
setnames(wq_data,'ActivityStartDate','Date')
setnames(wq_data,'ResultMeasure.MeasureUnitCode','Result.Unit')
setnames(wq_data,'ResultMeasureValue','Result.Value')

# Check ResultStatusIdentifier
wq_data[,.N,by=ResultStatusIdentifier]
#    ResultStatusIdentifier     N
# 1:                  Final 15836
# 2:               Accepted 33497
# 3:            Provisional    89

# Remove Provisional data
wq_data=wq_data[ResultStatusIdentifier!='Provisional']
dim(wq_data) # 49333 xx

# Check RelativeDepth
wq_data[,.N,by=RelativeDepth]
#    RelativeDepth     N
# 1:          <NA> 34870
# 2:       Surface 13543
# 3:        Bottom   920

# Check for SampleDepthValue when RelativeDepth=NA
wq_data[is.na(SampleDepthValue) & is.na(RelativeDepth),.N]
# 4834 - Potential observations to include

# Check Depth Units
tmp=wq_data[!is.na(SampleDepthValue),.N,by=SampleDepthUnit]
tmp
#    SampleDepthUnit     N
# 1:               m 30036

# Standardize Depth units - Manually adjust code below as needed
if(nrow(tmp)>1) cat('STOP - Multiple depth units. Manually adjust code below and fix.')
# wq_data[SampleDepthUnit=='ft',':='(SampleDepthValue=SampleDepthValue/3.28084,SampleDepthUnit='m')]

# Set SampleDepthValue <=1 m to Surface; >1 m to Bottom
# REVISIT. CONFIRM! Previous version just deleted all RelativeDepth=NA. Confirm UT wants to do this, and that 1m is an appropriate threshold.
wq_data[is.na(RelativeDepth) & SampleDepthValue<=1,RelativeDepth:='Surface']
wq_data[is.na(RelativeDepth) & SampleDepthValue>1,RelativeDepth:='Bottom']

# Check RelativeDepth
wq_data[,.N,by=RelativeDepth]
#    RelativeDepth     N
# 1:          <NA>  4834
# 2:       Surface 32196
# 3:        Bottom 12303





#### Join translate_params
# Edit translate_params field names to match WQP data
tmp=c('CharacteristicName','ResultSampleFractionText','RelativeDepth','Parameter','Fraction','Depth')
cbind(names(translate_params),tmp)
names(translate_params)=tmp

# Join translate_params to data
intersect(names(wq_data),names(translate_params))
wq_data=merge(wq_data,translate_params,by=c('CharacteristicName','ResultSampleFractionText','RelativeDepth'),all.x=T) # Left outer join
dim(wq_data) # 49333 xx

# Drop unused fields
wq_data[,CharacteristicName:=NULL]
wq_data[,ResultSampleFractionText:=NULL]
wq_data[,RelativeDepth:=NULL]

# Remove Fraction=NA
sum(is.na(wq_data$Fraction)) # 4100
wq_data=wq_data[!is.na(Fraction),]
dim(wq_data) # 45233 xx

# Remove Depth=NA
sum(is.na(wq_data$Depth)) # 4571
wq_data=wq_data[!is.na(Depth),]
dim(wq_data) # 40662 xx




#### Check for multiple units
tmp=wq_data[,1,by=.(Parameter,Result.Unit)]
tmp=tmp[,.N,by=Parameter]
tmp=tmp[N>1]
setorder(tmp,Parameter)
tmp # Parameters with N>1 have multiple units
#                   Parameter N
# 1:                   Barium 2
# 2:            Chlorophyll a 2
# 3: Depth, Secchi disk depth 2
# 4:                     Iron 2
# 5:                 Salinity 2
# 6:           Total Coliform 2
if(nrow(tmp)>0) cat('STOP - Multiple units are present. Manually adjust code below to standardize units.')

# Standardize units - Manually update code as needed
# REVISIT. Confirm. Alternative is to remove data with non-majority units
wq_data[Parameter=='Barium',.N,by=Result.Unit]
wq_data[Parameter=='Barium'  & Result.Unit=='mg/l',':='(Result.Unit='ug/l',Result.Value=Result.Value*1000)]
wq_data[Parameter=='Chlorophyll a',.N,by=Result.Unit]
wq_data[Parameter=='Chlorophyll a' & Result.Unit=='mg',':='(Result.Unit='ug/l',Result.Value=NA)]
wq_data[Parameter=='Depth, Secchi disk depth',.N,by=Result.Unit]
wq_data[Parameter=='Depth, Secchi disk depth' & Result.Unit=='cm',':='(Result.Unit='m',Result.Value=Result.Value/100)]
wq_data[Parameter=='Iron',.N,by=Result.Unit]
wq_data[Parameter=='Iron' & Result.Unit=='mg/l',':='(Result.Unit='ug/l',Result.Value=Result.Value*1000)]
wq_data[Parameter=='Salinity',.N,by=Result.Unit]
wq_data[Parameter=='Salinity' & Result.Unit=='ppth',Result.Unit:='ppt']
wq_data[Parameter=='Total Coliform',.N,by=Result.Unit]
wq_data[Parameter=='Total Coliform' & Result.Unit=='#/100ml',Result.Unit:='MPN/100ml']

# Check for multiple units
tmp=wq_data[,1,by=.(Parameter,Result.Unit)]
tmp=tmp[,.N,by=Parameter]
tmp=tmp[N>1]
setorder(tmp,Parameter)
tmp # Should be empty
if(nrow(tmp)>0) cat('STOP - Multiple units are present. Manually adjust code below to standardize units.')






###################################*
#### Impute NDs ####
###################################*

# Check NDs
wq_data[,.N,by=ResultDetectionConditionText]
#          ResultDetectionConditionText     N
# 1:                       Not Detected  4581
# 2:                               <NA> 35835
# 3: Present Above Quantification Limit   243
# 4: Present Below Quantification Limit     3

# Check that all NDs have blank values
wq_data[ResultDetectionConditionText=='Not Detected',.N,by=is.na(Result.Value)]
#    is.na    N
# 1:  TRUE 4581

# Check that MDL units and value units match
tmp=wq_data[ResultDetectionConditionText=='Not Detected',.(Result.Unit,MethodDetectionLevelUnit),by=Parameter]
tmp=unique(tmp)
tmp # Some MDL units = NA
tmp=tmp[Result.Unit!=MethodDetectionLevelUnit,]
tmp # No missmatched units
if(nrow(tmp)>0) cat('STOP - Some value and MDL units do not match. Manually adjust code below to standardize MDL units.')

# Standardize MDL units to match value units - Manually update code as needed
# wq_data[Parameter=='aaa',.N,by=.(Result.Unit,MethodDetectionLevelUnit)]
# wq_data[Parameter=='aaa'  & MethodDetectionLevelUnit=='mg/l',':='(MethodDetectionLevelUnit='ug/l',MethodDetectionLevel=MethodDetectionLevel*1000)]

# Impute NDs to 1/2 MDL
wq_data[is.na(Result.Value) & ResultDetectionConditionText=='Not Detected',Result.Value:=MethodDetectionLevel/2]

# Remove value=NA
sum(is.na(wq_data$Result.Value)) # 1675
wq_data=wq_data[!is.na(Result.Value),]
dim(wq_data) # 38987 xx






###################################*
#### Negative/Zero Values ####
###################################*

# Check for negative values
tmp=wq_data[Result.Value<0,.(Fraction,Parameter,Result.Value)]
tmp=unique(tmp)
tmp # Temperature data - OK
if(nrow(tmp)>0) cat('STOP - Negative values are present. Check if values are appropriate. Then manually adjust code below.')

# Set inappropriate negative values to NA - Manually update code as needed
# wq_data[Result.Value<0 & Fraction=='aaa' & Parameter=='aaa',Result.Value:=NA]

# Check for zero values
tmp=wq_data[Result.Value==0,min(Result.Value),by=.(Fraction,Parameter,Result.Unit)]
tmp
#    Fraction                          Parameter Result.Unit V1
# 1:    Total                     Carbon dioxide        mg/l  0
# 2:    Total                          Carbonate        mg/l  0
# 3:    Total                              Depth           m  0
# 4:      N/A           Depth, Secchi disk depth           m  0
# 5:      N/A              Dissolved oxygen (DO)        mg/l  0
# 6:      N/A        Dissolved oxygen saturation           %  0
# 7:    Total                     Fecal Coliform     #/100ml  0
# 8:    Total Fecal Streptococcus Group Bacteria     #/100ml  0
# 9:    Total                          Hydroxide        mg/l  0
# 10:    Total                           Salinity         ppt  0
# 11:    Total                      Sum of anions        mg/l  0
# 12:    Total                     Sum of cations        mg/l  0
# 13:    Total                     Total Coliform   MPN/100ml  0
# 14:    Total              Total volatile solids        mg/l  0
if(nrow(tmp)>0) cat('STOP - Zero values are present. Check if values are appropriate. Then manually adjust code below.')

# Check minimum nonzero values
tmp1=wq_data[Result.Value>0,min(Result.Value,na.rm=T),by=.(Fraction,Parameter,Result.Unit)]
tmp1

# Set inappropriate zero values to NA or epsilon - Manually update code as needed
# REVISIT. "Epsilon" can be set to 1) minimum nonzero values, 2) half of minimum nonzero values, 3) NA, or 4) something else.
# wq_data[Result.Value==0 & Fraction=='aaa' & Parameter=='bbb',
# 				Result.Value:=tmp1[Fraction='aaa' & Parameter=='bbb',V1]]

# Remove value=NA
sum(is.na(wq_data$Result.Value)) # 0
wq_data=wq_data[!is.na(Result.Value),]
dim(wq_data) # 38987 xx






###################################*
#### Create Profile Dataset ####
###################################*

# Extract profile data
profile_data=copy(wq_data[!is.na(DataLoggerLine),])
dim(profile_data) # 15032 xx

# Join Sites
intersect(names(profile_data),names(sites))
profile_data=merge(profile_data,sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join
profile_data[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]
dim(profile_data) # 15032 xx

# Subset WQP data
wq_data=copy(wq_data[is.na(DataLoggerLine),])
dim(wq_data) # 23955 xx





###################################*
#### N:P Ratios ####
###################################*

# Extract TN and TP
np_flat=copy(wq_data[Parameter %in% c('Nitrogen','Phosphate-phosphorus'),])
dim(np_flat) # 2219 xx

# Pivot & Aggregate to account for any duplicates
np_mat=dcast(np_flat,MonitoringLocationIdentifier+Date+Fraction+Depth~Parameter,value.var='Result.Value',fun.aggregate=mean,fill=NA)
dim(np_mat) # 1585 xx

# Calculate N:P ratios
np_mat[,NP_mass:=Nitrogen/`Phosphate-phosphorus`]
np_mat[,NP_mol:=NP_mass*(30.974/14.007)]

# Drop fields
np_mat[,Nitrogen:=NULL]
np_mat[,`Phosphate-phosphorus`:=NULL]

# Unpivot
np_flat=melt(np_mat,measure.vars=c('NP_mass','NP_mol'),variable.factor=F,na.rm=T,variable.name='Parameter',value.name='Result.Value')
dim(np_flat) # 1220 xx

# Rename values
np_flat[Parameter=='NP_mass',Parameter:='N:P ratio (mass)']
np_flat[Parameter=='NP_mol',Parameter:='N:P ratio (molar)']

# Append N:P ratios back to wq_data
wq_data=rbindlist(list(wq_data,np_flat),use.names=T,fill=T)
dim(wq_data) # 25175 xx






###################################*
#### Create Trophic Dataset ####
###################################*

# Subset Trophic Dataset
trophic_data=copy(wq_data)
trophic_data=trophic_data[Fraction=='Total' & Depth=='Surface' & Parameter %in% c('Chlorophyll a','Phosphate-phosphorus'),]
dim(trophic_data) # 1807 xx

# Subset Secchi data (Depth & Fraction = N/A)
tmp=copy(wq_data[Parameter=='Depth, Secchi disk depth',])
dim(tmp) # 682 xx

# Append Secchi
trophic_data=rbindlist(list(trophic_data,tmp),use.names=T,fill=T)
dim(trophic_data) # 2489 xx

# Check for values=0 (affects TSI log calculations)
trophic_data[Result.Value==0,.(Fraction,Parameter,Result.Value)]
#    Fraction                Parameter Result.Value
# 1:      N/A Depth, Secchi disk depth                  0

# Check minimum observed values
tmp=trophic_data[Result.Value>0,min(Result.Value,na.rm=T),by=.(Parameter,Result.Unit)]
tmp
#                   Parameter Result.Unit    V1
# 1:            Chlorophyll a        ug/l 0.200
# 2:     Phosphate-phosphorus        mg/l 0.008
# 3: Depth, Secchi disk depth           m 0.025

# Set zero values to NA or epsilon
# REVISIT. "Epsilon" can be set to 1) minimum nonzero values, 2) half of minimum nonzero values, 3) NA, or 4) something else. Currently set to minimum nonzero value
trophic_data[Result.Value==0 & Parameter=='Chlorophyll a',
						 Result.Value:=tmp[Parameter=='Chlorophyll a',V1]]
trophic_data[Result.Value==0 & Parameter=='Phosphate-phosphorus ',
						 Result.Value:=tmp[Parameter=='Phosphate-phosphorus ',V1]]
trophic_data[Result.Value==0 & Parameter=='Depth, Secchi disk depth',
						 Result.Value:=tmp[Parameter=='Depth, Secchi disk depth',V1]]

# Pivot, Long to Wide
trophic_data=dcast(trophic_data,MonitoringLocationIdentifier+Date~Parameter,value.var='Result.Value',fun.aggregate=mean,fill=NA)
dim(trophic_data) # 756 xx

# Calculate TSI
tmp=calcTSI(setDF(trophic_data),chl='Chlorophyll a',TP='Phosphate-phosphorus',SD='Depth, Secchi disk depth')
dim(tmp) # 756 xx

# Join TSI
trophic_data=cbind(trophic_data,tmp)
setDT(trophic_data)

# Add Year and Month
trophic_data[,Year:=year(Date)]
trophic_data[,Month:=month(Date)]

# Join Sites
intersect(names(trophic_data),names(sites))
trophic_data=merge(trophic_data,sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join
trophic_data[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]
dim(trophic_data) # 756 xx

# Sort
setorder(trophic_data,Monitoring.Location.ID,Date)





###################################*
#### Aggregate WQP Data ####
###################################*

# Pivot & Aggregate
wq_data=dcast(wq_data,MonitoringLocationIdentifier+Date+Depth+Fraction+Result.Unit~Parameter,value.var='Result.Value',fun.aggregate=mean,fill=NA)
dim(wq_data) # 8779 xx

# Unpivot
wq_data=melt(wq_data,id.vars=c('MonitoringLocationIdentifier','Date','Depth','Fraction','Result.Unit'),variable.factor=F,na.rm=T,variable.name='Parameter',value.name='Result.Value')
dim(wq_data) # 23900 xx

# Add Year and Month
wq_data[,Year:=year(Date)]
wq_data[,Month:=month(Date)]

# Join Sites
intersect(names(wq_data),names(sites))
wq_data=merge(wq_data,sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join
wq_data[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]
dim(wq_data) # 23900 xx

# Sort
setorder(wq_data,Monitoring.Location.ID,Date,Depth,Fraction,Parameter)




###################################*
#### **** Save Processed Datasets **** ####
###################################*

# Export to RData (Archived)
save(ul_poly,file=paste0('ArchivedRDataFiles/ul_poly_',Sys.Date(),'.RData'))
save(lake_elev_data,file=paste0('ArchivedRDataFiles/lake_elev_data_',Sys.Date(),'.RData'))
save(nla_data,file=paste0('ArchivedRDataFiles/nla_data_',Sys.Date(),'.RData'))
save(phyto_data,file=paste0('ArchivedRDataFiles/phyto_data_',Sys.Date(),'.RData'))
save(trophic_data,file=paste0('ArchivedRDataFiles/trophic_data_',Sys.Date(),'.RData'))
save(wq_data,file=paste0('ArchivedRDataFiles/wq_data_',Sys.Date(),'.RData'))

# Export to RData
save(ul_poly,file='../Shiny/RDataFiles/ul_poly.RData')
save(lake_elev_data,file='../Shiny/RDataFiles/lake_elev_data.RData')
save(nla_data,file='../Shiny/RDataFiles/nla_data.RData')
save(phyto_data,file='../Shiny/RDataFiles/phyto_data.RData')
save(trophic_data,file='../Shiny/RDataFiles/trophic_data.RData')
save(wq_data,file='../Shiny/RDataFiles/wq_data.RData')

# Export to csv (Archived)
fwrite(lake_elev_data,paste0('ProcessedCSVs/ArchivedCSVs/lake_elev_data_',Sys.Date(),'.csv'))
fwrite(nla_data,paste0('ProcessedCSVs/ArchivedCSVs/nla_data_',Sys.Date(),'.csv'))
fwrite(phyto_data,paste0('ProcessedCSVs/ArchivedCSVs/phyto_data_',Sys.Date(),'.csv'))
fwrite(trophic_data,paste0('ProcessedCSVs/ArchivedCSVs/trophic_data_',Sys.Date(),'.csv'))
fwrite(wq_data,paste0('ProcessedCSVs/ArchivedCSVs/wq_data_',Sys.Date(),'.csv'))

# Export to csv
fwrite(lake_elev_data,'ProcessedCSVs/lake_elev_data.csv')
fwrite(nla_data,'ProcessedCSVs/nla_data.csv')
fwrite(phyto_data,'ProcessedCSVs/phyto_data.csv')
fwrite(trophic_data,'ProcessedCSVs/trophic_data.csv')
fwrite(wq_data,'ProcessedCSVs/wq_data.csv')
