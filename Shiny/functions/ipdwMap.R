
#ipdwMap function
ipdwMap=function(mapdata,maptitle,costraster,mask_poly,uniform_scale=FALSE){
		
	if(dim(mapdata)[1]>0){
		withProgress(message="Initializing...",{
			aggmean=aggregate(Result.Value~Monitoring.Location.Latitude+Monitoring.Location.Longitude,data=mapdata,FUN='mean')
			aggcount=aggregate(Result.Value~Monitoring.Location.Latitude+Monitoring.Location.Longitude,data=mapdata,FUN='length')
			names(aggcount)[names(aggcount)=="Result.Value"]="n_count"
			
			dsp <- SpatialPoints(aggmean[,2:1], proj4string=CRS("+proj=longlat +datum=WGS84"))
			dsp <- SpatialPointsDataFrame(dsp, aggmean)
			dsp=merge(dsp,aggcount)
			
			projection(costraster)=projection(dsp)
			
			setProgress(message="Interpolating...",0.3)
			dsp.ipdw=ipdw(dsp,costraster,paramlist="Result.Value",range=10)
			
			setProgress(message="Plotting...",0.6)

			maskr=rasterize(mask_poly,dsp.ipdw)
			dsp.ipdw.mask=mask(x=dsp.ipdw,maskr)
			
			setProgress(message="Plotting...",0.9)
			
			projection(mask_poly)=projection(dsp)
			plot(mask_poly,col=NA,border=NA,main=maptitle,cex.main=1.75)
			
			setProgress(1)
			
			#if(uniform_scale==TRUE){
			#	breaks=unique(signif(seq(min(data[,parameter],na.rm=TRUE),max(data[,parameter],na.rm=TRUE),length.out=9),2))
			#	mappalette=brewer.pal(n = length(breaks), name = "OrRd")
			#	plot(dsp.ipdw.mask,axis.args=c(cex.axis=1.5),col=mappalette,add=T,breaks=breaks)
			#	#plot(costraster)
			#}else{
				mappalette=brewer.pal(n = 9, name = "OrRd")
				plot(dsp.ipdw.mask,axis.args=c(cex.axis=1.5),col=mappalette,add=T,legend=FALSE)
				plot(dsp.ipdw.mask,legend.only=TRUE,col=mappalette,smallplot=c(0.85,0.9, 0.2,0.8),axis.args=list(cex.axis=1.5))

			#}
			text(dsp,dsp$n_count,cex=1.75,halo=TRUE,hw=0.2)
			axis(1,cex.axis=1.5)
			axis(2,cex.axis=1.5)

		})
	}else{
		frame()
		text(0.5,0.5,"No data for selected parameters.",cex=2.25)
	}
}
#ipdwMap(gsl_data,"Total_nitrogen_mgL",c("Surface","Deep"),date_min="2001-08-01",date_max="2018-04-01")


