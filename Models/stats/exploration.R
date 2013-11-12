

robustnessPositions <- function(){
	dat <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/project/Results/Robustness/400repets.csv",sep=";")
	summary(dat)
	par(mfcol=c(2,2),bg="lightyellow")
	hist(dat$eval.density,breaks=100,xlab="Density",main="")
    hist(dat$spatial.autocorrelation.index,breaks=100,xlab="Moran",main="")
	hist(dat$eval.speed,breaks=100,xlab="Speed",main="")
	hist(dat$eval.activities,breaks=100,xlab="Accessibility",main="")

}





