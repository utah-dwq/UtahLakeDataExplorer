###Calculate TSI function

calcTSI=function(x,in_format="matrix",chl="chla",TP="TP",SD="SD",type_column=NA,val_column=NA){
#default input units: chla (ug/L), TP (mg/L), SD (m)
	if(in_format=="matrix"){
		TSIchl=9.81*log(x[,chl])+30.6
		TSItp=14.2*log(x[,TP]*1000)+4.15
		TSIsd=60-14.41*log(x[,SD])
		TSI=cbind(TSIchl,TSItp,TSIsd)
	}
	return(TSI)
}

