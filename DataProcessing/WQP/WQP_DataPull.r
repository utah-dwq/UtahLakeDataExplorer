

###################################*
#### Utah Lake - Automated Data Query & Processing ####
###################################*


###################################*
#### Contact Info ####
###################################*

# Jake Vander Lann (jvander@utah.gov)




###################################.
#### Citations ####
###################################.

# R Citation
citation()
# R Core Team. 2019. R: A language and environment for statistical computing. R Foundation for Statistical Computing,Vienna,Austria. URL https://www.R-project.org/.




###################################*
#### Load Packages ####
###################################*

# Load packages
# devtools::install_github('utah-dwq/wqTools')
library(wqTools)
library(data.table)
# detach(package:aaa,unload=T)
options(digits=5)


# Clean workspace
rm(list=ls()); gc()





###################################*
#### Overview ####
###################################*

# Utah Lake Automated data query & processing
# This script is designed to automate the data query process for Utah Lake data and generate
# a data output that is readily interface-able with the WASP WRDB.
# 
# There are two dataframe outputs which are written to csv in working directory:  
# 1. Raw,as queried data from EPA WQP (https://www.waterqualitydata.us/portal/)
# 2. Processed data for WRDB interface 
# 
# The target output for the WRDB interface product is a dataframe with one unique result value for each site,date,parameter,and depth level,with a single uniform time stamp and required metadata. To generate this product,this script:  
# 1. Queries data for Utah Lake sites from EPA WQP.
# 2. Removes lab based results for field parameters (pH,temperature,DO,specific conductance)  
# 3. Harmonizes multiple depth attribute columns (relative depth,profile result depth,& grab sample activity depth) to a single attribute.
# 4. Harmonizes multiple time stamps available for a site & date to a single (earliest) time stamp to apply to all records collected at that site & date.
# 5. Extracts a single unique result value for each site,date,parameter,and depth level.




###################################*
#### **** Download Raw Data **** ####
###################################*

# WQP data are queried as four separate components: 1) site metadata,2) result values (narrow result),3) activity metadata (activity),and 4) detection/quantitation limits. 
# Site metadata,activity metadata,and detection/quantitation limits are all merged to narrow result to generate a single dataset. Multiple det/quant limits are often available for a single water quality result. These are cast to a wide format before merging to narrow result to maintain all available det/quant limits.


# Utah Lake site list
auid_list=c('UT-L-16020201-004_02','UT-L-16020201-004_01')

# Read Utah Lake site locations  & generate site map
# Note: Connection to WQP occasionally times out. Function tries connection up to 10 times.
ul_sites=wqTools::readWQP(type='sites',auid=auid_list,siteType=c('Lake, Reservoir, Impoundment'),sampleMedia='Water')
setDT(ul_sites)

# Make syntactically correct field names
names(ul_sites)=gsub(' ','', names(ul_sites))

# Drop fields that are contained in ul_nr
ul_sites[,OrganizationIdentifier:=NULL]
ul_sites[,OrganizationFormalName:=NULL]
ul_sites[,ProviderName:=NULL]

# Generate site map
map=wqTools::buildMap(sites=ul_sites,plot_polys=F)
map=leaflet::setView(map,lng=median(ul_sites$LongitudeMeasure),lat=median(ul_sites$LatitudeMeasure),zoom=10)
map

# Download WQP components
ul_nr=wqTools::readWQP(type='narrowresult',auid=auid_list,siteType=c('Lake, Reservoir, Impoundment'),sampleMedia='Water',print=F)
ul_act=wqTools::readWQP(type='activity',auid=auid_list,sampleMedia='Water')
ul_dql=wqTools::readWQP(type='detquantlim',auid=auid_list,sampleMedia='Water')

# Check that all 4 WQP components were downloaded
if(!all(exists('ul_sites'),exists('ul_nr'),exists('ul_act'),exists('ul_dql'))) print('STOP - WQP component(s) did not download.')

# Convert to data.tables
setDT(ul_nr)
setDT(ul_act)
setDT(ul_dql)

# Make syntactically correct field names
names(ul_nr)=gsub(' ','', names(ul_nr))
names(ul_act)=gsub(' ','', names(ul_act))
names(ul_dql)=gsub(' ','', names(ul_dql))

# Pivot ul_dql on "detection limit type"
dql_vals=dcast(ul_dql,ResultIdentifier~DetectionQuantitationLimitTypeName,value.var='DetectionQuantitationLimitMeasure.MeasureValue')

# Pivot ul_dql on "Units"
dql_units=dcast(ul_dql,ResultIdentifier~DetectionQuantitationLimitTypeName,value.var='DetectionQuantitationLimitMeasure.MeasureUnitCode')

# Make syntactically correct field names
names(dql_vals)=gsub(' ','', names(dql_vals))
names(dql_units)=gsub(' ','', names(dql_units))

# Edit dql_units names
tmp=paste0(names(dql_units),'Unit')
tmp[1]='ResultIdentifier'
names(dql_units)=tmp

# Join detection type and units
intersect(names(dql_vals),names(dql_units))
ul_dql_wide=merge(dql_vals,dql_units,by='ResultIdentifier',all=F) # Inner join (Left/Right Outer join also works)

# Drop fields from ul_nr that are in ul_act
ul_nr[,ActivityStartDate:=NULL]
ul_nr[,ActivityStartTime.Time:=NULL]
ul_nr[,ActivityStartTime.TimeZoneCode:=NULL]

# Drop fields from ul_act that are in ul_nr
ul_nr[,OrganizationIdentifier:=NULL]
ul_nr[,OrganizationFormalName:=NULL]
ul_nr[,MonitoringLocationIdentifier:=NULL]
ul_nr[,ProviderName:=NULL]

# Join activity metadata data to narrowresult
intersect(names(ul_nr),names(ul_act))
wq_data=merge(ul_nr,ul_act,by='ActivityIdentifier',all.x=T) # Left outer join
dim(wq_data) # 54862 xx

# Convert to Date
wq_data[,ActivityStartDate:=as.Date(ActivityStartDate,format='%Y-%m-%d')]

# Join site data to narrowresult
intersect(names(wq_data),names(ul_sites))
wq_data=merge(wq_data,ul_sites,by='MonitoringLocationIdentifier',all.x=T) # Left outer join

# Join detection/quantification limit data to narrowresult
intersect(names(wq_data),names(ul_dql_wide))
wq_data=merge(wq_data,ul_dql_wide,by='ResultIdentifier',all.x=T) # Left outer join

# Write raw, as queried data from EPA WQP
fwrite(wq_data,paste0('ArchivedRawDatasets/ul_data_wqp_raw_',Sys.Date(),'.csv'))
dim(wq_data) # 54862 xx







###################################*
#### **** Process Raw Data **** ####
###################################*


###################################*
#### Filter Data ####
###################################*

# Remove QA/QC field replicate site
wq_data=wq_data[MonitoringLocationIdentifier!='UTAHDWQ_WQX-4917320',]
dim(wq_data) # 51405 xx

# Check field parameters for activity types
field_params=c('pH','Specific conductance','Dissolved oxygen (DO)','Dissolved oxygen saturation','Temperature,water')
wq_data[CharacteristicName %in% field_params,.N,by=ActivityTypeCode]
#                      ActivityTypeCode     N
# 1:                     Sample-Routine   412
# 2:                      Field Msr/Obs  3496
# 3: Field Msr/Obs-Portable Data Logger 12688
# 4: Sample-Integrated Vertical Profile   164
# knitr::kable(table(wq_data[CharacteristicName %in% field_params,'ActivityTypeCode']))

# Remove lab data for field parameters
# REVISIT. Why are "Sample-Routine" obs removed?
wq_data=wq_data[!(CharacteristicName %in% field_params &
										ActivityTypeCode %in% c('Sample-Routine','Sample-Integrated Vertical Profile','Quality Control Sample-Field Replicate')),]
dim(wq_data) # 50829 xx

# (double) Check field parameters for activity types
wq_data[CharacteristicName %in% field_params,.N,by=ActivityTypeCode]
#                      ActivityTypeCode     N
# 1:                      Field Msr/Obs  3496
# 2: Field Msr/Obs-Portable Data Logger 12688
# knitr::kable(table(wq_data[CharacteristicName %in% field_params,'ActivityTypeCode']))





###################################*
#### Sample Depth ####
###################################*

# Merge ActivityDepth and ResultDepth values to a new column
wq_data[,SampleDepthValue:=ActivityDepthHeightMeasure.MeasureValue] # Reset
wq_data[is.na(SampleDepthValue),SampleDepthValue:=ResultDepthHeightMeasure.MeasureValue]

# Merge ActivityDepth and ResultDepth units to a new column
wq_data[,SampleDepthUnit:=ActivityDepthHeightMeasure.MeasureUnitCode] # Reset
wq_data[is.na(SampleDepthUnit),SampleDepthUnit:=ResultDepthHeightMeasure.MeasureUnitCode]

# *NOTE:*  
# Many records missing both depth fields have a RelativeDepth filled in which could potentially be used to fill in values.
# Others are for CharacteristicName == 'Depth, data-logger (ported)' and can be filled with the result value and unit.
wq_data[,.N,by=.(is.na(SampleDepthValue) & is.na(ActivityRelativeDepthName))]
#    is.na     N
# 1:  TRUE  5432
# 2: FALSE 45397
# knitr::kable(table(is.na(wq_data$SampleDepthValue) & is.na(wq_data$ActivityRelativeDepthName)))
wq_data[,.N,by=.(is.na(SampleDepthValue) & CharacteristicName=='Depth, data-logger (ported)')]
#    is.na     N
# 1: FALSE 50249
# 2:  TRUE   580
# knitr::kable(table(is.na(wq_data$SampleDepthValue) & wq_data$CharacteristicName=='Depth, data-logger (ported)'))

# Fill sample depths and units for CharacteristicName 'Depth, data-logger (ported)'
wq_data[is.na(SampleDepthValue) & CharacteristicName == 'Depth, data-logger (ported)',
				SampleDepthValue:=ResultMeasureValue]
wq_data[is.na(SampleDepthUnit) & CharacteristicName == 'Depth, data-logger (ported)',
				SampleDepthUnit:=ResultMeasure.MeasureUnitCode]

# Check SampleDepthValue
wq_data[,.N,by=.(is.na(SampleDepthValue) & CharacteristicName=='Depth, data-logger (ported)')]
#    is.na     N
# 1: FALSE 50829
# knitr::kable(table(is.na(wq_data$SampleDepthValue) & wq_data$CharacteristicName=='Depth, data-logger (ported)'))

# Setting SampleDepthValue to NA for records w/ ActivityRelativeDepthName=='Bottom' & ActivityDepthHeight==0
wq_data[ActivityRelativeDepthName=='Bottom' & ActivityDepthHeightMeasure.MeasureValue==0,SampleDepthValue:=NA]

# Generating new RelativeDepth column for cases where both ActivityDepth and ResultDepth are NULL
wq_data[,RelativeDepth:=NA_character_] # Reset
wq_data[is.na(SampleDepthValue) & !is.na(ActivityRelativeDepthName),RelativeDepth:=ActivityRelativeDepthName]






###################################*
#### Add DateTime ####
###################################*

# Extract times by site & date
site_date_starttime=unique(wq_data[,.(MonitoringLocationIdentifier,ActivityStartDate,ActivityStartTime.Time,ActivityStartTime.TimeZoneCode)])
dim(site_date_starttime) # 2215 xx

# Remove Time=NA
site_date_starttime=site_date_starttime[!is.na(ActivityStartTime.Time),]
dim(site_date_starttime) # 1990 xx

# Check timeszones
site_date_starttime[,.N,by=ActivityStartTime.TimeZoneCode]
#    ActivityStartTime.TimeZoneCode    N
# 1:                            MDT   17
# 2:                            MST 1971
# 3:                            EST    2
# knitr::kable(table(site_date_starttime$ActivityStartTime.TimeZoneCode))

# *Note - a few samples marked as MDT time zone. Default seems to be MST. Accepting all times and setting tz to 'America/Denver'.*

# Add datetime
site_date_starttime[,datetime:=as.POSIXct(paste(ActivityStartDate,ActivityStartTime.Time),format='%Y-%m-%d %H:%M:%S',tz='America/Denver')]

# Filter for minimum time value
site_date_starttime=site_date_starttime[,.(datetime=min(datetime)),by=.(MonitoringLocationIdentifier,ActivityStartDate)]
dim(site_date_starttime) # 1111 xx

# Add hour and minute
site_date_starttime[,hour:=hour(datetime)]
site_date_starttime[,minute:=minute(datetime)]

# Join unified time stamps back to data (by site & date)
intersect(names(wq_data),names(site_date_starttime))
wq_data=merge(wq_data,site_date_starttime,by=c('MonitoringLocationIdentifier','ActivityStartDate'),all.x=T) # Left outer join
dim(wq_data) # 50829 xx





###################################*
#### Account for Multiple Results ####
###################################*

# Subset to relevant columns
cols_keep=c(
	'OrganizationIdentifier','OrganizationFormalName','MonitoringLocationIdentifier','MonitoringLocationName','MonitoringLocationTypeName','LatitudeMeasure','LongitudeMeasure','HorizontalCoordinateReferenceSystemDatumName','ActivityStartDate','DataLoggerLine',
	# These are the columns that have been added through the code above  
	'SampleDepthUnit','SampleDepthValue','RelativeDepth','datetime','minute','hour',
	'CharacteristicName','ResultSampleFractionText','ResultMeasureValue','ResultMeasure.MeasureUnitCode', 
	'MethodSpecificationName','MeasureQualifierCode','ResultDetectionConditionText', 
	'ResultStatusIdentifier','ResultValueTypeName','ActivityMediaName', 
	'EstimatedQuantitationLimit','LowerQuantitationLimit','LowerReportingLimit','MethodDetectionLevel', 
	'UpperQuantitationLimit','EstimatedQuantitationLimitUnit', 
	'LowerQuantitationLimitUnit','LowerReportingLimitUnit','MethodDetectionLevelUnit','UpperQuantitationLimitUnit'  
)
wq_data=wq_data[,..cols_keep]

# Extracting unique result value for each site, date, depth, parameter, & fraction
wq_data=unique(wq_data)
dim(wq_data) # 49418 xx

# Check for multiple result values by site, date, depth, parameter, & fraction
tmp=wq_data[,.(MonitoringLocationIdentifier,datetime,SampleDepthValue,RelativeDepth,CharacteristicName,ResultSampleFractionText)]
nrow(wq_data) - nrow(unique(tmp)) # 5569 - multiple results

# *NOTE:*  
# Some records have one or more unique result value. These are flagged in processed data w/ result_count column.

# Copy dataset and relevant fields
tmp=c('MonitoringLocationIdentifier','datetime','SampleDepthValue','RelativeDepth','CharacteristicName','ResultSampleFractionText')
result_count=copy(wq_data[,..tmp])
dim(result_count) # 49418 xx

# Count multiple results
result_count=result_count[,.(result_count=.N),by=tmp]
dim(result_count) # 43849 xx

# Join result_count (use all fields)
wq_data=merge(wq_data,result_count,by=tmp,all.x=T) # Left outer join
dim(wq_data) # 49422 xx
wq_data[,.N,by=result_count]
#    result_count     N
# 1:            1 40123
# 2:            2  5176
# 3:            3  1902
# 4:            4  1384
# 5:            5   695
# 6:            6    96
# 7:           13    13
# 8:           12    12
# 9:           21    21

# Sort
setorder(wq_data,MonitoringLocationIdentifier,datetime,SampleDepthValue,RelativeDepth,CharacteristicName,ResultSampleFractionText,ResultMeasureValue)

# Note: Files below are names "ul_data" to avoid confusion with the final processed "wq_data.RData" file.
# Export to csv and RData
fwrite(wq_data,paste0('ArchivedProcessedDatasets/ul_data_wqp_processed_',Sys.Date(),'.csv'))
save(wq_data,file=paste0('ArchivedProcessedDatasets/ul_data_',Sys.Date(),'.RData'))

# OVERWRITE existing "wq_data" object in parent directory.
# THIS WILL overwrite the .RData file that is the input to the Shiny app.
save(wq_data,file='ul_data.RData')
