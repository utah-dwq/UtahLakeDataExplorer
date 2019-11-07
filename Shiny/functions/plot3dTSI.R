#3D TSI plot function


plot3dTSI=function(data){
data=na.omit(data)
ChlA_Secchi=data$TSIchl-data$TSIsd
ChlA_TP=data$TSIchl-data$TSItp

	plot(NA,NA,xaxt='n',yaxt='n',ylim=c(-100,100),xlim=c(-100,100),ylab="",xlab="",bty="n",main="",cex.axis=2,cex.lab=1.5)
	axis(1,at=c(-50,50),pos=0)
	axis(1,at=seq(-75,75,25),pos=0,labels=F)
	axis(2,at=c(-50,50),pos=0,las=1)
	axis(2,at=seq(-75,75,25),pos=0,las=1,labels=F)
	axis(2,at=seq(-75,75,25),pos=0,las=1,labels=F)
	segments(-55,-55,55,55,lty=2,lwd=2,col="dimgrey")	
	arrows(40,-40,52,-52,lwd=2,col="dimgrey",length=0.125)
	arrows(-40,40,-52,52,lwd=2,col="dimgrey",length=0.125)
	rect(-100,-100,100,100)
	
	points(ChlA_Secchi,ChlA_TP,pch=21,col="black",bg="orange",cex=3)
	#points(mean(ChlA_Secchi),mean(ChlA_TP),pch="+",col="darkgreen",cex=5)
	
	par(xpd=NA)
	text(x=-75,y=110,"Small particulates")
	arrows(-15,110,-45,110,lwd=2,col="dimgrey",length=0.125)
	text(x=75,y=110,"Large particulates")
	arrows(15,110,45,110,lwd=2,col="dimgrey",length=0.125)
	text(x=-50,y=-110,"TSI ChlA < TSI Secchi")
	text(y=-125,x=0,"TSI ChlA - TSI Secchi", cex=1.5)
	text(x=-125,y=0,"TSI ChlA - TSI TP", cex=1.5,srt=90)
	text(x=50,y=-110,"TSI ChlA > TSI Secchi")
	text(x=-110,y=-50,"TSI ChlA < TSI TP",srt=90)
	text(x=-110,y=50,"TSI ChlA > TSI TP",srt=90)
	text(x=50,y=-90,"Zooplankton grazing")
	text(x=-50,y=90,"Dissolved color/clay particles")
	text(x=57,y=-57,"TSI Secchi < TSI TP",srt=45)
	text(x=-57,y=57,"TSI Secchi > TSI TP",srt=45)
	text(x=-95,y=-65,"P surplus",srt=90)
	text(x=-95,y=65,"P limitation",srt=90)
	text(x=40,y=40,adj=c(0,-0.5),"TSI Secchi = TSI TP",srt=45)
	arrows(-95,-15,-95,-45,lwd=2,col="dimgrey",length=0.125)
	arrows(-95,15,-95,45,lwd=2,col="dimgrey",length=0.125)

	
	
}



