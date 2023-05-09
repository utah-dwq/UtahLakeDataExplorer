

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

# Load libraries
options(digits=7L)
par(mar=c(4.5,4.5,1,1),bty='l')
# library(openxlsx)
library(data.table)
library(leaflet)
library(ggplot2)
library(sf)
library(leaflet)
library(mapview)
# detach(package:aaa,unload=T)


# Clean workspace
rm(list=ls()); gc()

# Global settings
na.strings=c(NA,'NA','',' ','<Null>','None')
# NOTE: "translate_params.csv" requires "N/A" values for the Shiny app, so do NOT include "N/A" above.
compElev=4489.045 # Set Elevation below Compromise
ftToMeters=3.28084



# Source custom functions
source('../Shiny/functions/calcTSI.R')







###################################*
#### **** Load Data **** ####
###################################*


###################################*
#### Stations ####
###################################*

# This dataset is static. It only needs to be updated if a new monitoring station is added to WQP, etc.

# Load data
sites=fread('Data/Stations.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(sites) # 48 xx

# Convert to character
sites[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]

# Add prefix to SiteID
sites[,MonitoringLocationIdentifier:=Monitoring.Location.ID]
sites[MonitoringLocationIdentifier!='7458',MonitoringLocationIdentifier:=paste0('UTAHDWQ_WQX-',MonitoringLocationIdentifier)]
sites[MonitoringLocationIdentifier=='7458',MonitoringLocationIdentifier:=paste0('NALMS-',MonitoringLocationIdentifier)]

# Sort
setorder(sites,Monitoring.Location.ID)




###################################*
#### Map ####
###################################*

# leaflet Map
map=leaflet(sites) %>%
	addCircles(lat=~Monitoring.Location.Latitude,lng=~Monitoring.Location.Longitude,radius=500,stroke=T,weight=1,opacity=1,color=1,fillColor='dodgerblue',fillOpacity=1) %>%
	# addMarkers(lat=~Monitoring.Location.Latitude,lng=~Monitoring.Location.Longitude) %>%
	addProviderTiles('Esri.WorldImagery') %>% # Esri.WorldImagery Esri.WorldTopoMap Esri.NatGeoWorldMap OpenStreetMap
	setView(lng=-111.83,lat=40.20,zoom=11) %>%
	fitBounds(lng1=-112.02,lng2=-111.62,lat1=40.00,lat2=40.40)

# Export to png
mapshot(map,file='Map_Sites.png')





###################################*
#### GIS Layers ####
###################################*

# This dataset is static.

# Load polygon
ul_poly=st_read('../Shiny/polyrast','UtahLake_poly_wgs84')
ul_poly=st_zm(ul_poly) # Removing 'z' coordinates for leaflet





###################################*
#### Elevation-Storage LUT ####
###################################*

# This dataset currently not used in Shiny app. Keep for reference.

# Load data
storageElev=fread('Data/elevation_storage.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
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

# Source file is "ULDB_Lake_Elevation_All.xlsx", "UL Elevation_Avg Monthly" tab.
# This dataset is dynamic.
# Most recent value is Dec 2020

# Create Month LUT
monthLUT=data.table(Month=1:12,MonthName=month.abb,MonthLab=toupper(month.abb))

# Load data
lake_elev_data=fread('Data/UL_Elevation_Avg_Monthly.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(lake_elev_data) # 1092 4

# Add Month
intersect(names(lake_elev_data),names(monthLUT)) # MonthLab
lake_elev_data=merge(lake_elev_data,monthLUT[,.(MonthLab,Month)],by='MonthLab',all.x=T) # Left outer join
rm(monthLUT)

# Sort
setorder(lake_elev_data,Year,Month)





###################################*
#### Phytoplankton ####
###################################*

# Source file is "UtahLakePhytoplankton2002 to 12-18-2018.xlsx", "Clean" tab.
# This dataset is dynamic.
# Most recent value is 11/14/2018.

# Load data.
phyto_data=fread('Data/phytoplankton.csv',sep=',',header=T,skip=0,check.names=T,na.strings=c(na.strings,'N/A')) # Do NOT permanently add "N/A" to na.strings object.
dim(phyto_data) # 11494 xx

# Remove Date=NA
sum(is.na(phyto_data$Date)) # 3
phyto_data=phyto_data[!is.na(Date),]
dim(phyto_data) # 11491 xx

# Remove CellperML=NA
sum(is.na(phyto_data$CellperML)) # 2
phyto_data=phyto_data[!is.na(CellperML),]
dim(phyto_data) # 11489 xx

# Remove CellVolume.µ³.mL.=NA - Not Done, as there is still CellperML data

# Remove MLID=NA. REVISIT. Not done yet. Confirm
# sum(is.na(phyto_data$MLID)) # 20
# phyto_data=phyto_data[!is.na(MLID),]
# dim(phyto_data) # aaa xx

# Remove MLID=DWQ. REVISIT. Not done yet. Confirm
# phyto_data[MLID=='DWQ',.N] # 71
# phyto_data=phyto_data[MLID!='DWQ',]
# dim(phyto_data) # aaa xx


# Add missing fields
phyto_data[,Genus:=NA_character_]

# Filter fields
# tmp=c('Monitoring.Location.ID','SiteName','Monitoring.Location.Latitude','Monitoring.Location.Longitude','Date','SampleType','Division','Genus','Taxon','CellperML','CellVolume_u3mL') # OLD
tmp=c('MLID','SiteName','Date','SampleType','AlgalDivision','Genus','Taxon','CellperML','CellVolume.µ³.mL.') 
phyto_data=phyto_data[,..tmp]

# Rename fields
tmp=c('Monitoring.Location.ID','SiteName','Date','SampleType','Division','Genus','Taxon','CellperML','CellVolume_u3mL')
cbind(names(phyto_data),tmp)
names(phyto_data)=tmp

# Add Genus values, by grabbing the first word in the "Taxon" field
phyto_data[,Genus:=Taxon] # Copy
phyto_data[,Genus:=gsub(' ',' ',Genus)] # Replace weird space with a normal space
phyto_data[,Genus:=sub(' .*','',Genus)] # Grab the first word only
phyto_data[,.N,by=Genus]

# Temporarily set Date='2008' to '1/1/2008'
#phyto_data[Date=='2008',Date:='1/1/2008']

# Convert to Date
phyto_data[,Date:=as.Date(Date,format='%m/%d/%Y',tz='UTC')]

# Add Year and Month
phyto_data[,Year:=year(Date)]
phyto_data[,Month:=month(Date)]

# Set Date="1905-06-30" to NA
phyto_data[Date==as.Date('1905-06-30'),Date:=NA]
phyto_data[Year=='1905',Year:=NA]

# Convert to numeric (don't use integer)
head(sort(unique(phyto_data$CellperML)),50)
tail(sort(unique(phyto_data$CellperML)),50)
phyto_data[,CellperML:=gsub(',','',CellperML,fixed=T)]
phyto_data[,CellperML:=as.numeric(CellperML)]

# Convert to numeric (don't use integer)
head(sort(unique(phyto_data$CellVolume_u3mL)),50)
tail(sort(unique(phyto_data$CellVolume_u3mL)),50)
phyto_data[,CellVolume_u3mL:=gsub(',','',CellVolume_u3mL,fixed=T)]
phyto_data[,CellVolume_u3mL:=as.numeric(CellVolume_u3mL)]

# Check SampleType
phyto_data[,.N,by=SampleType]
#                   SampleType    N
# 1:       Total Phytoplankton 7156
# 2:                      <NA>   15
# 3:     Total Plankton Sample 2221
# 4:                       HAB  471
# 5:            Total Plankton   15
# 6:             Phytoplankton 1470
# 7: Phytoplankton/Zooplankton  113
# 8:                  PHYTOHAB   28

# Set SampleType values to either "HAB" or "Total phytoplankton"
phyto_data[SampleType %in% c('HAB','PHYTOHAB'),SampleType:='HAB']
phyto_data[!SampleType %in% 'HAB',SampleType:='Total phytoplankton']
phyto_data[,.N,by=SampleType]
#             SampleType     N
# 1: Total phytoplankton 10990
# 2:                 HAB   499

# Check Division
phyto_data[,.N,by=Division]
#             Division    N
# 1:       Chlorophyta 5029
# 2:   Bacillariophyta 1644
# 3:       Cryptophyta  302
# 4:        Cyanophyta 2272
# 5:      Euglenophyta  320
# 6:         Dinophyta  242
# 7:       Chrysophyta   40
# 8:              <NA> 1633
# 9: Unspecified Algae    5
# 10:            Diatom    2

# Set Division=NA to Other
phyto_data[is.na(Division),Division:='Other']

# single incidence of Division = "c" should be Cyanophyta (Aphanizomenon)
phyto_data <- phyto_data[!(phyto_data$Division == "c")]

# Check Genus
phyto_data[,.N,by=Genus]

# Set Genus=NA to Other
phyto_data[is.na(Genus),Genus:='Other']

# Join Sites
intersect(names(phyto_data),names(sites)) # Monitoring.Location.ID
phyto_data=merge(phyto_data,sites,by='Monitoring.Location.ID',all.x=T) # Left outer join
dim(phyto_data) # 11489 xx

# Sort
setorder(phyto_data,Monitoring.Location.ID,Date,Division,Genus)





###################################*
#### NLA 2012 Data ####
###################################*

# Source file is "NLA2012_TrophicData.csv".
# NLA 2007 data was not incorporated. NLA 2017 data is not yet available.
# This dataset is static, until the next NLA in 2017.
# Most recent value is 2012.

# Load data
nla_data=fread('Data/NLA2012_TrophicData.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
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
#### Parameter LUT ####
###################################*

# This file standardizes parameter name, fraction, and depth fields.
# This file is static, though it could require updating in the future.

# Notes:
# ======.
# All Fecal Coliform set to Surface
# All Settleable solids set to Total
# All Turbidity (TURB) set to Fraction=N/A

# NOTE: "translate_params.csv" requires "N/A" values for the Shiny app, so do not include "N/A" in na.strings
# Check for "N/A"
if(any(grepl('N/A',na.strings))) print("STOP - na.string cannot include 'N/A'")

# Load data
translate_params=fread('Data/translate_params.csv',sep=',',header=T,skip=0,check.names=T,na.strings=na.strings)
dim(translate_params) # 503 xx

# Sort
setorder(translate_params,Parameter,Fraction,Depth)




###################################*
#### **** WQP Data **** ####
###################################*

###################################*
#### Process Data ####
###################################*

### Notes:
# ========.
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


# WQP data were pulled and pre-processed in "/R_WQP/WQP_DataPull.r"
# Load pre-processed data
wq_data <- read.csv('WQP/ul_data_wqp_processed_2021-10-21.csv')
# load('WQP/ul_data.RData')
dim(wq_data) # 100080 43

# Rename fields to match Shiny code
setnames(wq_data,'ActivityStartDate','Date')
setnames(wq_data,'ResultMeasure.MeasureUnitCode','Result.Unit')
setnames(wq_data,'ResultMeasureValue','Result.Value')

# Check ResultStatusIdentifier
setDT(wq_data)[,.N,keyby=ResultStatusIdentifier]
#    ResultStatusIdentifier     N
# 1:                  Final 15686
# 2:               Accepted 84304
# 3:            Provisional   90

# Remove Provisional data
wq_data=wq_data[!ResultStatusIdentifier %in% c('Provisional')]
dim(wq_data) # 99990 43

# Check RelativeDepth
wq_data[,.N,by=RelativeDepth]
#    RelativeDepth     N
# 1:          <NA> 80454
# 2:       Surface 18579
# 3:        Bottom   957

# Check for SampleDepthValue when RelativeDepth=NA
wq_data[is.na(SampleDepthValue) & is.na(RelativeDepth),.N] # 4784

# Check Depth Units
tmp=wq_data[!is.na(SampleDepthValue),.N,by=SampleDepthUnit]
tmp
#    SampleDepthUnit     N
# 1:               m 75670

# Standardize Depth units - Manually adjust code below as needed
if(nrow(tmp)>1) cat('STOP - Multiple depth units. Manually adjust code below and fix.')
wq_data[SampleDepthUnit=='ft',':='(SampleDepthValue=SampleDepthValue/ftToMeters,SampleDepthUnit='m')]

# Set SampleDepthValue <=1 m to Surface; >1 m to Bottom
wq_data[is.na(RelativeDepth) & SampleDepthValue<=1,RelativeDepth:='Surface']
wq_data[is.na(RelativeDepth) & SampleDepthValue>1,RelativeDepth:='Bottom']

# Check RelativeDepth
wq_data[,.N,by=RelativeDepth]
#    RelativeDepth     N
# 1:          <NA>  4784
# 2:       Surface 51761
# 3:        Bottom 43445





#### Join translate_params
# Rename fields
setnames(wq_data,'aaa','CharacteristicName')
setnames(wq_data,'aaa','ResultSampleFractionText')
setnames(wq_data,'aaa','RelativeDepth')

# Join translate_params to data xxx
intersect(names(wq_data),names(translate_params)) # RelativeDepth" "CharacteristicName" "ResultSampleFractionText"
wq_data=merge(wq_data,translate_params,by=c('CharacteristicName','ResultSampleFractionText','RelativeDepth'),all.x=T) # Left outer join

# Drop unused fields
wq_data[,CharacteristicName:=NULL]
wq_data[,ResultSampleFractionText:=NULL]
wq_data[,RelativeDepth:=NULL]

# Remove Fraction=NA
sum(is.na(wq_data$Fraction)) # 13357
wq_data=wq_data[!is.na(Fraction),]
dim(wq_data) # 86633 43

# Remove Depth=NA
sum(is.na(wq_data$Depth)) # 4497
wq_data=wq_data[!is.na(Depth),]
dim(wq_data) # 82136 43




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
wq_data[Parameter=='Barium',.N,by=Result.Unit]
wq_data[Parameter=='Barium' & Result.Unit=='mg/l',':='(Result.Unit='ug/l',Result.Value=Result.Value*1000)]
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
#### NDs ####
###################################*

# Check NDs
wq_data[,.N,by=ResultDetectionConditionText]
#          ResultDetectionConditionText     N
# 1:                       Not Detected  7153
# 2:                               <NA> 74599
# 3: Present Above Quantification Limit   379
# 4: Present Below Quantification Limit     5

# Check that all NDs have blank values
wq_data[ResultDetectionConditionText=='Not Detected',.N,by=is.na(Result.Value)]
#    is.na    N
# 1:  TRUE 7153

# Check that MDL units and value units match
tmp=wq_data[ResultDetectionConditionText=='Not Detected',.(Result.Unit,MethodDetectionLevelUnit),by=Parameter]
tmp=unique(tmp)
tmp # Some MDL units = NA
tmp=tmp[Result.Unit!=MethodDetectionLevelUnit,]
tmp # Should be empty
if(nrow(tmp)>0) cat('STOP - Some value and MDL units do not match. Manually adjust code below to standardize MDL units.')

# Standardize MDL units to match value units - Manually update code as needed
# wq_data[Parameter=='aaa',.N,by=.(Result.Unit,MethodDetectionLevelUnit)]
# wq_data[Parameter=='aaa'  & MethodDetectionLevelUnit=='mg/l',':='(MethodDetectionLevelUnit='ug/l',MethodDetectionLevel=MethodDetectionLevel*1000)]

# Impute NDs to 1/2 MDL
wq_data[ResultDetectionConditionText=='Not Detected' & is.na(Result.Value),Result.Value:=MethodDetectionLevel/2]

# Remove value=NA
sum(is.na(wq_data$Result.Value)) # 1813
wq_data=wq_data[!is.na(Result.Value),]
dim(wq_data) # 80323 43






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

# Set inappropriate zero values to "minimum/2" - Manually update code as needed
# wq_data[Result.Value==0 & Fraction=='aaa' & Parameter=='bbb',
# 				Result.Value:=tmp1[Fraction='aaa' & Parameter=='bbb',V1/2]]

# Remove value=NA
sum(is.na(wq_data$Result.Value)) # 0
wq_data=wq_data[!is.na(Result.Value),]
dim(wq_data) # 80323 43





###################################*
#### Create Profile Dataset ####
###################################*

# Not Used. Archived for future potential use. Requires "DataLoggerLine" field from WQP data pull (not present).

# # Extract profile data
# profile_data=copy(wq_data[!is.na(DataLoggerLine),])
# dim(profile_data) # 15032 xx
# 
# # Join Sites
# intersect(names(profile_data),names(sites)) # 
# profile_data=merge(profile_data,sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join
# profile_data[,Monitoring.Location.ID:=as.character(Monitoring.Location.ID)]
# dim(profile_data) # 15032 xx
# 
# # Subset WQP data
# wq_data=copy(wq_data[is.na(DataLoggerLine),])
# dim(wq_data) # 23955 xx





###################################*
#### N:P Ratios ####
###################################*

# Extract TN and TP
np_flat=copy(wq_data[Parameter %in% c('Nitrogen','Phosphate-phosphorus'),])
dim(np_flat) # 3854 43

# Pivot & Aggregate to account for any duplicates
np_mat=dcast(np_flat,MonitoringLocationIdentifier+Date+Fraction+Depth~Parameter,value.var='Result.Value',fun.aggregate=mean,fill=NA)
dim(np_mat) # 2383 6

# Calculate N:P ratios
np_mat[,NP_mass:=Nitrogen/`Phosphate-phosphorus`]
np_mat[,NP_mol:=NP_mass*(30.974/14.007)]

# Drop fields
np_mat[,Nitrogen:=NULL]
np_mat[,`Phosphate-phosphorus`:=NULL]

# Unpivot
np_flat=melt(np_mat,measure.vars=c('NP_mass','NP_mol'),variable.factor=F,na.rm=T,variable.name='Parameter',value.name='Result.Value')
dim(np_flat) # 2816 6

# Rename values
np_flat[Parameter=='NP_mass',Parameter:='N:P ratio (mass)']
np_flat[Parameter=='NP_mol',Parameter:='N:P ratio (molar)']

# Append N:P ratios back to wq_data
wq_data=rbindlist(list(wq_data,np_flat),use.names=T,fill=T)
rm(np_flat,np_mat)
dim(wq_data) # 83139 xx






###################################*
#### Create Trophic Dataset ####
###################################*

# Subset Trophic Dataset
trophic_data=copy(wq_data)
trophic_data=trophic_data[Fraction=='Total' & Depth=='Surface' & Parameter %in% c('Chlorophyll a','Phosphate-phosphorus'),]
dim(trophic_data) # 2169 43

# Filter for Secchi data (Depth & Fraction already equal = N/A)
tmp=copy(wq_data[Parameter=='Depth, Secchi disk depth',])
dim(tmp) # 2953 43

# Append Secchi to Chla and TP data
trophic_data=rbindlist(list(trophic_data,tmp),use.names=T,fill=T)
dim(trophic_data) # 5122 43

# Check for values=0 (affects TSI log calculations)
trophic_data[Result.Value==0,sort(unique(Parameter))] # Depth, Secchi disk depth

# Check minimum observed values
tmp=trophic_data[Result.Value>0,min(Result.Value,na.rm=T),by=.(Parameter,Result.Unit)]
tmp
#                   Parameter Result.Unit    V1
# 1:            Chlorophyll a        ug/l 0.200
# 2:     Phosphate-phosphorus        mg/l 0.008
# 3: Depth, Secchi disk depth           m 0.025

# Set zero values to "minimum/2"
trophic_data[Result.Value==0 & Parameter=='Chlorophyll a',
						 Result.Value:=tmp[Parameter=='Chlorophyll a',V1/2]]
trophic_data[Result.Value==0 & Parameter=='Phosphate-phosphorus ',
						 Result.Value:=tmp[Parameter=='Phosphate-phosphorus ',V1/2]]
trophic_data[Result.Value==0 & Parameter=='Depth, Secchi disk depth',
						 Result.Value:=tmp[Parameter=='Depth, Secchi disk depth',V1/2]]

# Pivot
trophic_data=dcast(trophic_data,MonitoringLocationIdentifier+Date~Parameter,value.var='Result.Value',fun.aggregate=mean,fill=NA)
dim(trophic_data) # 970 5

# Calculate TSI
tmp=calcTSI(setDF(trophic_data),chl='Chlorophyll a',TP='Phosphate-phosphorus',SD='Depth, Secchi disk depth')
dim(tmp) # 970 3

# Join TSI
trophic_data=cbind(trophic_data,tmp)
setDT(trophic_data)

# Add Year and Month
trophic_data[,Year:=year(Date)]
trophic_data[,Month:=month(Date)]

# Join Sites
intersect(names(trophic_data),names(sites)) # MonitoringLocationIdentifier
trophic_data=merge(trophic_data,sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join

# Sort
setorder(trophic_data,Monitoring.Location.ID,Date)





###################################*
#### Aggregate WQP Data ####
###################################*

# Aggregation not run. For potential future use.

# Current size
# dim(wq_data) # aaa xx
# 
# # Pivot & Aggregate
# wq_data=dcast(wq_data,MonitoringLocationIdentifier+Date+Depth+Fraction+Result.Unit~Parameter,value.var='Result.Value',fun.aggregate=mean,fill=NA)
# dim(wq_data) # aaa xx
# 
# # Unpivot
# wq_data=melt(wq_data,id.vars=c('MonitoringLocationIdentifier','Date','Depth','Fraction','Result.Unit'),variable.factor=F,na.rm=T,variable.name='Parameter',value.name='Result.Value')
# dim(wq_data) # aaa xx

# Add Year and Month
wq_data[,Year:=year(Date)]
wq_data[,Month:=month(Date)]

# Join Sites
intersect(names(wq_data),names(sites)) # MonitoringLocationIdentifier
wq_data=merge(wq_data,sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join
dim(wq_data) # 83139 50

# Sort
setorder(wq_data,Monitoring.Location.ID,Date,Depth,Fraction,Parameter)


###################################*
#### **** Sonde Data **** ####
###################################*

sonde_data_mean <- read.csv('./Data/BuoyData_Wide_Mean.csv')
sonde_data_max <- read.csv('./Data/BuoyData_Wide_Max.csv')
sonde_data_min <- read.csv('./Data/BuoyData_Wide_Min.csv')
sonde_data_mean$date <- as.Date(sonde_data_mean$date)
sonde_data_max$date <- as.Date(sonde_data_max$date)
sonde_data_min$date <- as.Date(sonde_data_min$date)

###################################*
#### **** Save Processed Datasets **** ####
###################################*

# Export to RData. This OVERWRITES data in the Shiny folder!!!
save(ul_poly,file='../Shiny/RDataFiles/ul_poly.RData')
save(lake_elev_data,file='../Shiny/RDataFiles/lake_elev_data.RData')
save(nla_data,file='../Shiny/RDataFiles/nla_data.RData')
save(phyto_data,file='../Shiny/RDataFiles/phyto_data.RData')
save(trophic_data,file='../Shiny/RDataFiles/trophic_data.RData')
save(wq_data,file='../Shiny/RDataFiles/wq_data.RData')
save(sonde_data_mean,file='../Shiny/RDataFiles/sonde_data_mean.RData')
save(sonde_data_max,file='../Shiny/RDataFiles/sonde_data_max.RData')
save(sonde_data_min,file='../Shiny/RDataFiles/sonde_data_min.RData')
































