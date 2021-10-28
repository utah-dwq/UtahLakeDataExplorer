

###################################*
#### Utah Lake - Automated Data Query & Processing ####
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
# R Core Team. 2019. R: A language and environment for statistical computing. R Foundation for Statistical Computing,Vienna,Austria. URL https://www.R-project.org/.

citation('wqTools')
# Jake Vander Laan, Utah Division of Water Quality, jvander@utah.gov, Elise Hinman, Utah Division of Water Quality and ehinman@utah.gov. 2019. wqTools: A Collection of R Tools for Utah Division of Water Quality. R package version 0.0.0.9000.




###################################*
#### Load Packages ####
###################################*

# Load libraries
options(digits=7L)
par(mar=c(4.5,4.5,1,1),bty='l')
# devtools::install_github('utah-dwq/wqTools')
library(data.table)
library(wqTools)
library(leaflet)
library(mapview)
# detach(package:aaa,unload=T)


# Clean workspace
rm(list=ls()); gc()

# Global settings
na.strings=c(NA,'NA','N/A','#N/A','na','-88','',' ','None','none','<Null>')
seed=27709L





###################################*
#### Overview ####
###################################*

# Utah Lake Automated data query & processing.
# This script is designed to automate the data query process for Utah Lake data and generate
# a data output that is readily interface-able with the WASP WRDB.
# 
# There are two dataframe outputs which are written to csv in working directory:  
# 1. Raw,as queried data from EPA WQP (https://www.waterqualitydata.us/portal/)
# 2. Processed data for WRDB interface 
# 
# The target output for the WRDB interface product is a dataframe with one unique result value for each site,date,parameter,and depth level,with a single uniform time stamp and required metadata. To generate this product,this script:  
# 1. Queries data for Utah Lake ul_sites from EPA WQP.
# 2. Removes lab based results for field parameters (pH,temperature,DO,specific conductance)  
# 3. Harmonizes multiple depth attribute columns (relative depth,profile result depth,& grab sample activity depth) to a single attribute.
# 4. Harmonizes multiple time stamps available for a site & date to a single (earliest) time stamp to apply to all records collected at that site & date.
# 5. Extracts a single unique result value for each site,date,parameter,and depth level.



###################################*
#### WQP Data Structure ####
###################################*

# WQP data types include:
# =======================.
#        Result - aaa REVISIT. UPDATE
# Narrow Result - aaa
#         Sites - Site metatdata
#      Activity - aaa
#  detquantlim - Quantification Limit





###################################*
#### **** Download Raw Data **** ####
###################################*

# WQP data are queried as four separate components: 1) site metadata,2) result values (narrow result),3) activity metadata (activity),and 4) detection/quantitation limits. 
# Site metadata,activity metadata,and detection/quantitation limits are all merged to narrow result to generate a single dataset. Multiple det/quant limits are often available for a single water quality result. These are cast to a wide format before merging to narrow result to maintain all available det/quant limits.


# Utah Lake site list
auid=c('UT-L-16020201-004_02','UT-L-16020201-004_01')

# Read Utah Lake site locations
# Note: Connection to WQP occasionally times out. Function tries connection up to 10 times.
ul_sites=readWQP(type='sites',auid=auid,siteType=c('Lake, Reservoir, Impoundment'),sampleMedia='Water')
setDT(ul_sites)
dim(ul_sites) # 68 xx

# Make syntactically correct field names - Not needed
# names(ul_sites)=make.names(names(ul_sites),unique=T)

# Drop fields that are contained in ul_nr
ul_sites[,OrganizationIdentifier:=NULL]
ul_sites[,OrganizationFormalName:=NULL]
ul_sites[,ProviderName:=NULL]

# Generate site map
map=buildMap(sites=ul_sites,plot_polys=F)
map=setView(map,lng=median(ul_sites$LongitudeMeasure),lat=median(ul_sites$LatitudeMeasure),zoom=10)

# Export to png
mapshot(map,file='Map_Sites.png')

# Download WQP components
ul_nr=readWQP(type='narrowresult',auid=auid,siteType=c('Lake, Reservoir, Impoundment'),sampleMedia='Water',print=F)
ul_act=readWQP(type='activity',auid=auid,sampleMedia='Water')
ul_dql=readWQP(type='detquantlim',auid=auid,sampleMedia='Water')

# Check that all 4 WQP components were downloaded
if(!all(exists('ul_sites'),exists('ul_nr'),exists('ul_act'),exists('ul_dql'))) print('STOP - WQP component(s) did not download.')

# Convert to data.tables
setDT(ul_nr)
setDT(ul_act)
setDT(ul_dql)

# Dims
dim(ul_nr)  # 67690 xx
dim(ul_act) # 37000 xx
dim(ul_dql) # 5955 xx

# Make syntactically correct field names - Not needed
# names(ul_nr)=make.names(names(ul_nr),unique=T)
# names(ul_act)=make.names(names(ul_act),unique=T)
# names(ul_dql)=make.names(names(ul_dql),unique=T)

#### Process ul_dql
# Pivot ul_dql on "detection limit type"
dql_vals=dcast(ul_dql,ResultIdentifier~DetectionQuantitationLimitTypeName,fun.aggregate=NULL,value.var='DetectionQuantitationLimitMeasure.MeasureValue')
dim(dql_vals) # 21101 xx

# Pivot ul_dql on "Units"
dql_units=dcast(ul_dql,ResultIdentifier~DetectionQuantitationLimitTypeName,fun.aggregate=NULL,value.var='DetectionQuantitationLimitMeasure.MeasureUnitCode')
dim(dql_units) # 21101 xx

# Make syntactically correct field names (don't use make.names())
names(dql_vals)=gsub(' ','', names(dql_vals))
names(dql_units)=gsub(' ','', names(dql_units))

# Edit dql_units names
tmp=paste0(names(dql_units),'Unit')
tmp[1]='ResultIdentifier'
cbind(names(dql_units),tmp)
names(dql_units)=tmp

# Join detection type and units
intersect(names(dql_vals),names(dql_units)) # ResultIdentifier
ul_dql_wide=merge(dql_vals,dql_units,by='ResultIdentifier',all=F) # Inner join
dim(ul_dql_wide) # 21101 xx


#### Process ul_nr
# Drop fields from ul_nr that are in ul_act
ul_nr[,ActivityStartDate:=NULL]
ul_nr[,ActivityStartTime.Time:=NULL]
ul_nr[,ActivityStartTime.TimeZoneCode:=NULL]

# Drop fields from ul_act that are in ul_nr
ul_nr[,OrganizationIdentifier:=NULL]
ul_nr[,OrganizationFormalName:=NULL]
ul_nr[,MonitoringLocationIdentifier:=NULL]
ul_nr[,ProviderName:=NULL]



#### Create wq_data
# Join activity metadata data to narrowresult
intersect(names(ul_nr),names(ul_act)) # ActivityIdentifier
wq_data=merge(ul_nr,ul_act,by='ActivityIdentifier',all.x=T) # Left outer join
dim(wq_data) # 67690 xx

# Convert to Date
wq_data[,ActivityStartDate:=as.Date(ActivityStartDate,format='%Y-%m-%d')]
head(wq_data$ActivityStartDate)

# Join site data to narrowresult
intersect(names(wq_data),names(ul_sites)) # MonitoringLocationIdentifier
wq_data=merge(wq_data,ul_sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join

# Join detection/quantification limit data to narrowresult
intersect(names(wq_data),names(ul_dql_wide)) # ResultIdentifier
wq_data=merge(wq_data,ul_dql_wide,by='ResultIdentifier',all.x=T) # Left outer join




###################################*
#### **** Process Raw Data **** ####
###################################*


###################################*
#### Filter Data ####
###################################*

# Remove QA/QC field replicate site
wq_data=wq_data[MonitoringLocationIdentifier!='UTAHDWQ_WQX-4917320',]
dim(wq_data) # 63395 xx

# Check field parameters for activity types
field_params=c('pH','Specific conductance','Dissolved oxygen (DO)','Dissolved oxygen saturation','Temperature,water')
wq_data[CharacteristicName %in% field_params,.N,by=ActivityTypeCode]
#                      ActivityTypeCode     N
# 1:                     Sample-Routine   412
# 2:                      Field Msr/Obs  3496
# 3: Field Msr/Obs-Portable Data Logger 12688
# 4: Sample-Integrated Vertical Profile   164

# Remove lab data for field parameters
wq_data=wq_data[!(CharacteristicName %in% field_params &
										ActivityTypeCode %in% c('Sample-Routine','Sample-Integrated Vertical Profile','Quality Control Sample-Field Replicate')),]
dim(wq_data) # 62689 xx
# Note: "Sample-Routine" is removed because it is usually associated with laboratory analysis of grab samples. Field data are coded under "Field Msr/Obs" or "Field Msr/Obs-Portable Data Logger". So, this step is to exclude possible lab measurements of field parameters like temperature, pH, etc.

# Check field parameters for activity types
wq_data[CharacteristicName %in% field_params,.N,by=ActivityTypeCode]
#                      ActivityTypeCode     N
# 1:                      Field Msr/Obs  4027
# 2: Field Msr/Obs-Portable Data Logger 16960





###################################*
#### Sample Depth ####
###################################*

# Merge ActivityDepth and ResultDepth values to a new column
wq_data[,SampleDepthValue:=ActivityDepthHeightMeasure.MeasureValue]
wq_data[is.na(SampleDepthValue),SampleDepthValue:=ResultDepthHeightMeasure.MeasureValue]

# Merge ActivityDepth and ResultDepth units to a new column
wq_data[,SampleDepthUnit:=ActivityDepthHeightMeasure.MeasureUnitCode]
wq_data[is.na(SampleDepthUnit),SampleDepthUnit:=ResultDepthHeightMeasure.MeasureUnitCode]

# *NOTE:*  
# Many records missing both depth fields have a RelativeDepth filled in which could potentially be used to fill in values.
# Others are for CharacteristicName == 'Depth, data-logger (ported)' and can be filled with the result value and unit.
wq_data[,.N,by=.(is.na(SampleDepthValue) & is.na(ActivityRelativeDepthName))]
#    is.na     N
# 1:  TRUE  5432
# 2: FALSE 57257

wq_data[,.N,by=.(is.na(SampleDepthValue) & CharacteristicName=='Depth, data-logger (ported)')]
#    is.na     N
# 1: FALSE 62109
# 2:  TRUE   580

# Fill sample depths and units for CharacteristicName 'Depth, data-logger (ported)'
wq_data[is.na(SampleDepthValue) & CharacteristicName == 'Depth, data-logger (ported)',
				SampleDepthValue:=ResultMeasureValue]
wq_data[is.na(SampleDepthUnit) & CharacteristicName == 'Depth, data-logger (ported)',
				SampleDepthUnit:=ResultMeasure.MeasureUnitCode]

# Check SampleDepthValue
wq_data[,.N,by=.(is.na(SampleDepthValue) & CharacteristicName=='Depth, data-logger (ported)')]
#    is.na     N
# 1: FALSE 62689

# Setting SampleDepthValue to NA for records w/ ActivityRelativeDepthName=='Bottom' & ActivityDepthHeight==0
wq_data[ActivityRelativeDepthName=='Bottom' & ActivityDepthHeightMeasure.MeasureValue==0,SampleDepthValue:=NA]

# Generating new RelativeDepth column for cases where both ActivityDepth and ResultDepth are NULL
wq_data[,RelativeDepth:=NA_character_]
wq_data[is.na(SampleDepthValue) & !is.na(ActivityRelativeDepthName),RelativeDepth:=ActivityRelativeDepthName]
wq_data[,.N,by=RelativeDepth]
#    RelativeDepth     N
# 1:          <NA> 44720
# 2:       Surface 17012
# 3:        Bottom   957



###################################*
#### Add DateTime ####
###################################*

# Extract times by site & date
site_date_starttime=unique(wq_data[,.(MonitoringLocationIdentifier,ActivityStartDate,ActivityStartTime.Time,ActivityStartTime.TimeZoneCode)])
dim(site_date_starttime) # 2630 xx

# Remove Time=NA
site_date_starttime=site_date_starttime[!is.na(ActivityStartTime.Time),]
dim(site_date_starttime) # 2405 xx

# Check timeszones
site_date_starttime[,.N,by=ActivityStartTime.TimeZoneCode]
#    ActivityStartTime.TimeZoneCode    N
# 1:                            MDT   17
# 2:                            MST 2386
# 3:                            EST    2
### Accepting all and set tz to 'America/Denver'.

# Add datetime
site_date_starttime[,datetime:=as.POSIXct(paste(ActivityStartDate,ActivityStartTime.Time),format='%Y-%m-%d %H:%M:%S',tz='America/Denver')]

# Filter for minimum time value, by Site-Date
min(site_date_starttime$datetime) # 1978-08-31 09:30:00 MDT
site_date_starttime=site_date_starttime[,.(datetime=min(datetime)),by=.(MonitoringLocationIdentifier,ActivityStartDate)]
dim(site_date_starttime) # 1259 xx

# Add hour and minute
site_date_starttime[,hour:=hour(datetime)]
site_date_starttime[,minute:=minute(datetime)]

# Join unified time stamps back to data, by Site-Date
intersect(names(wq_data),names(site_date_starttime)) # MonitoringLocationIdentifier ActivityStartDate
wq_data=merge(wq_data,site_date_starttime,by=c('MonitoringLocationIdentifier','ActivityStartDate'),all.x=T) # Left outer join
dim(wq_data) # 62689





###################################*
#### Account for Multiple Results ####
###################################*

# Subset to relevant columns xxx
tmp=c(
	'OrganizationIdentifier','OrganizationFormalName','MonitoringLocationIdentifier','MonitoringLocationName','MonitoringLocationTypeName','LatitudeMeasure','LongitudeMeasure','HorizontalCoordinateReferenceSystemDatumName','ActivityStartDate',
	# These are the columns that have been added through the code above  
	'SampleDepthUnit','SampleDepthValue','RelativeDepth','datetime','minute','hour',
	'CharacteristicName','ResultSampleFractionText','ResultMeasureValue','ResultMeasure.MeasureUnitCode', 
	'MethodSpecificationName','MeasureQualifierCode','ResultDetectionConditionText', 
	'ResultStatusIdentifier','ResultValueTypeName','ActivityMediaName', 
	'EstimatedQuantitationLimit','LowerQuantitationLimit','LowerReportingLimit','MethodDetectionLevel', 
	'UpperQuantitationLimit','EstimatedQuantitationLimitUnit', 
	'LowerQuantitationLimitUnit','LowerReportingLimitUnit','MethodDetectionLevelUnit','UpperQuantitationLimitUnit'  
)
wq_data=wq_data[,..tmp]

# Filter for unique records
wq_data=unique(wq_data)
dim(wq_data) # 58369 xx

# Check for multiple result values by site, date, depth, parameter, & fraction
tmp=wq_data[,.(MonitoringLocationIdentifier,datetime,SampleDepthValue,RelativeDepth,CharacteristicName,ResultSampleFractionText)]
nrow(wq_data) - nrow(unique(tmp)) # 3564 multiple results

# *NOTE:*  
# Some records have one or more unique result value. These are flagged in processed data w/ result_count column.

# Copy dataset
tmp=c('MonitoringLocationIdentifier','datetime','SampleDepthValue','RelativeDepth','CharacteristicName','ResultSampleFractionText')
result_count=copy(wq_data[,..tmp])
dim(result_count) # 58369 xx

# Add duplicate count, per combination of all fields
result_count=result_count[,.(result_count=.N),by=tmp]
dim(result_count) # 54805 xx

# Join result_count (use all fields)
wq_data=merge(wq_data,result_count,by=tmp,all.x=T) # Left outer join
wq_data[,.N,by=result_count]
#    result_count     N
# 1:            1 52981
# 2:            2  1348
# 3:            3  2190
# 4:            4  1188
# 5:            5   520
# 6:            6    96
# 7:           13    13
# 8:           12    12
# 9:           21    21

# Sort
setorder(wq_data,MonitoringLocationIdentifier,datetime,SampleDepthValue,RelativeDepth,CharacteristicName,ResultSampleFractionText,ResultMeasureValue)

# Save data. This will OVERWRITE existing data.
save(wq_data,file='ul_data.RData')


































