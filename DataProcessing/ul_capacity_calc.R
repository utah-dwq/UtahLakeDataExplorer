# Read Utah Lake stage data and convert to surface area and volume estimates

library(dplyr)
library(data.table)

# Source and select capacity curves (UDWR or LIDAR based cap curves available):
setwd("C:\\Users\\jvander\\Documents\\R\\ul_capacity")
source("ul_cap_curves.R") #either setwd() or update path to capacity curves
cap=cap_lidar #or use cap_udwr

# Read data
## In browser, go to https://waterrights.utah.gov/cgi-bin/dvrtview.exe?STATION_ID=9019&RECORD_YEAR=2022&Modinfo=Daily_Comma
browseURL("https://waterrights.utah.gov/cgi-bin/dvrtview.exe?STATION_ID=9019&RECORD_YEAR=2022&Modinfo=Daily_Comma")
## Open comma delimited daily values at "Daily comma delimited data for the station is located here."
## Paste csv url below: 

url="https://waterrights.utah.gov/tmpdata/46451904.txt"
result=read.csv(file=url)

## Convert gage heights to elevation, rename/reformat columns as needed
result=result %>% dplyr::rename(gage_height=Daily.gage.height.in.FEET) %>% 
	dplyr::mutate(
		date=lubridate::make_date(year, month, day), #date format
		elevation_ft=4489.045+gage_height, #gage height to elevation (add compromise elevation to height)
		elevation_m=elevation_ft*0.3048
	)
	
## Elevation to SA & volume
result=data.table::data.table(result)
data.table::setkey(result, elevation_m) # Note, you could do this join to any elevation vector
cap=data.table::data.table(cap)
data.table::setkey(cap, stage_m) # Note, you could do this join to any elevation vector
result=data.frame(cap[result, roll="nearest"]) %>% # Join lake elevation data to capacity curves w/ "nearest" option from data.table
	dplyr::rename(elevation_m=stage_m) %>% 
	dplyr::select(date, elevation_ft, elevation_m, year, month, day, surface_area_m2, volume_m3)

## Results/plots
head(result)
plot(volume_m3~elevation_m, result, xlab="Lake elevation (m)", ylab="Lake volume (m3)")
plot(volume_m3~date, result, xlab="", ylab="Lake volume (m3)")

## Export data file
ul_elev_sa_vol=result
save(ul_elev_sa_vol, cap_lidar, file="C:\\Users\\jvander\\Documents\\R\\UtahLakeDataExplorer\\Shiny\\RDataFiles\\ul_elev_sa_vol.Rdata")




