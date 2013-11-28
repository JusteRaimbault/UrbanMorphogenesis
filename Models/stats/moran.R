#curves for moran index study (which size of the grid ?)

library("ggplot2")


alldata <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/project/Results/Moran/moran.csv",sep=";")

#hand set vars
nrepets=20
serie_length = 27
col_moran = 5

drawCurvesMoran<-function(indexes){
	#0:floor(length(alldata[,1])/(nrepets*serie_length))
	moran=c();se=c();groups=c();gridsize=c();
	for(i in indexes - 1){
		for(j in 0:(serie_length-1)){
			d = alldata[seq((i* nrepets * serie_length) + 1,((i+1)*nrepets*serie_length), serie_length)+j,col_moran]
			moran = append(moran,mean(d));se=append(se,sd(d));groups=append(groups,i)
			gridsize=append(gridsize,j)
		}	
	}
	dat = data.frame(moran,se,groups,gridsize)

	limits <- aes(ymax = moran + se, ymin= moran - se)
	p <- ggplot(dat, aes(colour=groups, y= moran, x= gridsize))
	p + geom_line(aes(group=groups)) + geom_errorbar(limits, width=0.2)
}

drawCurvesMoran(0:floor(length(alldata[,1])/(nrepets*serie_length)))

drawCurvesMoran(c(1,3,5))



