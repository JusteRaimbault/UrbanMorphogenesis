
#library(scatterplot3d)
#lattice is better
library(lattice)


robustnessPositions <- function(){
	dat <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/project/Results/Robustness/400repets.csv",sep=";")
	summary(dat)
	par(mfcol=c(2,2),bg="lightyellow")
	hist(dat$eval.density,breaks=100,xlab="Density",main="")
    hist(dat$spatial.autocorrelation.index,breaks=100,xlab="Moran",main="")
	hist(dat$eval.speed,breaks=100,xlab="Speed",main="")
	hist(dat$eval.activities,breaks=100,xlab="Accessibility",main="")

}

robustnessPositions()


grid <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/project/Results/GridExploration/grid.csv",sep=";")

plot3d <- function(reporterName,xParamName,yParamName, otherParams,otherParamsValues,theta,phi,title){
	
	x <- sort(unique(grid[[xParamName]]))
	y <- sort(unique(grid[[yParamName]]))
	z = matrix(nrow=length(x),ncol=length(y))
	xcors = matrix(nrow=length(x),ncol=length(y))
	ycors = matrix(nrow=length(x),ncol=length(y))
	for(i in 1:length(x)){
		for(j in 1:length(y)){
			z[i,j] = getReporterValue(c(xParamName,yParamName, otherParams),c(x[i],y[j], otherParamsValues),reporterName,grid)
			xcors[i,j]=x[i];ycors[i,j]=y[j]
		}
	}

	#wireframe(z~xcors*ycors,data=data.frame(z,xcors,ycors),aspect = c(1,1),col="blue",shade=TRUE,light.source = c(10,0,10))
	
	#wireframe(x = z,row.values = x, column.values = y ,angle=50,scales = list(arrows = FALSE,distance=c(2,2,2)),screen = list(z = 30, x = -60),drape = TRUE,xlab=xParamName,ylab=yParamName,zlab=reporterName)
	
	#persp function is more ergonomic
	persp(x=x,y=y,z=z,r=10,theta=theta,phi=phi,col="lightblue",xlab=xParamName,ylab=yParamName,zlab=reporterName,shade = 0.75, ticktype = "detailed",cex.lab=0.8,cex.axis=0.6,main=title)
	
}

#let plot different reporters 
par(mfcol=c(2,2))
plot3d("eval.density","distance.to.activities.coefficient","distance.to.roads.coefficient",c("distance.to.center.coefficient","density.coefficient"),c(0.4,0.4),45,25,"Density")
plot3d("spatial.autocorrelation.index","distance.to.center.coefficient","density.coefficient",c("distance.to.activities.coefficient","distance.to.roads.coefficient"),c(0.4,0.4),140,25,"Moran Index")
plot3d("eval.speed","distance.to.activities.coefficient","density.coefficient",c("distance.to.center.coefficient","distance.to.roads.coefficient"),c(0.4,0.4),145,25,"Speed in Network")
plot3d("eval.activities","distance.to.activities.coefficient","density.coefficient",c("distance.to.center.coefficient","distance.to.roads.coefficient"),c(0.4,0.4),45,35,"Accessibility")




#get corrsponding values after sorting
#quite dirty but should be quick, data are not so big
#don't forget to take the mean on all realisations !
getReporterValue <- function(paramNames,paramValues,repName,dat){
	cum = 0 ; reals = 0
	for(i in 1:length(dat[[paramNames[1]]])){
		j = 1
		eq = TRUE
		for(param in paramNames){
			eq = eq&&(dat[[param]][i]==paramValues[j])
			j = j + 1
		}
		if(eq){cum=cum+dat[[repName]][i];reals=reals+1}
	}
	if(reals==0){return(0)}
	else{return(cum/reals)}
}

#test --> OK !
getReporterValue(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),c(0,0,0,0.2),"eval.density",grid)



#plot morphological classification of differences between cont and seq updates

update <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/project/Results/UpdateType/100houses.csv",sep=";")


plotMorphDiff <- function(paramNames){
	x = c();y=c();d=c();paramValues = c();nextPar=c();
	for(param in paramNames){
		paramValues=append(paramValues,update[[param]][1])
		nextPar = append(nextPar,update[[param]][1])
	}
	cumd=0;cumm=0;cumg=0;reals=0;
	for(i in 1:length(update[["moran.diff"]])){
		j = 1
		eq = TRUE
		for(param in paramNames){
			eq = eq&&(update[[param]][i]==paramValues[j])
			nextPar[j]=update[[param]][i]
			j = j + 1
		}
		#no pb to take the mean since same values
		if(eq){cumd=cumd+as.numeric(update[["eval.density.diff"]][i]);cumm=cumm+as.numeric(update[["moran.diff"]][i]);cumg=cumg+as.numeric(update[["global.density.diff"]][i]);reals=reals+1}
		#never 0 repets: at least 1!
		else{x=append(x,cumd/reals);y=append(y,cumm/reals);
			d=append(d,cumg/reals);
			cumd=as.numeric(update[["eval.density.diff"]][i]);
			cumm=as.numeric(update[["moran.diff"]][i]);
			cumg=as.numeric(update[["global.density.diff"]][i]);reals=1}
		paramValues=nextPar
	}
	
	#kmeans clustering for easy computation of local densities
	#km <- kmeans(data.frame(x,y),100)
	#density is cprresponding cluster size
	#dens <- km$size[km$cluster]
	#DOES NOT REALLY WORK :: cluster are quite random when number of clusters becomes big
	#need to compute local densities by hand : let do it brutally in O(n^2)
	
	#we weight by the significance of the point == global size of the difference !
	#really important to do that !
	dens <- densities(x,y) * d
	dens <- floor(99 * dens / max(dens)) + 1
	
	#compute corresponding color - ok since dens are integers
	col <- colorRampPalette(c("black", "red"))(100)[dens]
	
	plot(x,y,
	pch=19,
	cex=0.5,
	col=col,
	xlab="Density",ylab="Moran",main="Morphologic imprint of differences")
	return(data.frame(x,y))
}

testkmeans <-plotMorphDiff(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"))




#function to compute local densities in graph
#works with real points : all differents !
densities <- function(x,y){
	res = c()
	for(i in 1:length(x)){
	   tot = 0 ;
	   for (j in 1:length(x)){
	   	  if(j != i&&sqrt((x[i]-x[j])^2+(y[i]-y[j])^2)<0.03){tot = tot + 1 }
	   }
	   res = append(res,tot)
	}
	return(floor(res))
}






####
## Now plot imprint of each type of update
####

updatesCoords <- function(paramNames,updateValue,outParam){
	x = c();y=c();p=c();paramValues = c();nextPar=c();
	for(param in paramNames){
		paramValues=append(paramValues,update[[param]][1])
		nextPar = append(nextPar,update[[param]][1])
	}
	cumd=0;cumm=0;cump=0;reals=0;
	for(i in 1:length(update[["moran.diff"]])){
		j = 1
		eq = TRUE
		if(update[["built.cells.per.tick"]][i]==updateValue){
	      totalWeight=0
		  for(param in paramNames){
			eq = eq&&(update[[param]][i]==paramValues[j])
			totalWeight = totalWeight + paramValues[j]
			nextPar[j]=update[[param]][i]
			j = j + 1
		  }
		  #no pb to take the mean since same values
		  if(eq){cumd=cumd+as.numeric(update[["eval.density"]][i]);
		  	cumm=cumm+as.numeric(update[["spatial.autocorrelation.index"]][i]);
		  	cump=cump+(as.numeric(update[[outParam]][i])/totalWeight);
		  	reals=reals+1}
		  #never 0 repets: at least 1!
		  else{x=append(x,cumd/reals);y=append(y,cumm/reals);p=append(p,cump/reals);
			cumd=as.numeric(update[["eval.density"]][i]);
			cumm=as.numeric(update[["spatial.autocorrelation.index"]][i]);
			cump=as.numeric(update[[outParam]][i])/totalWeight;
			reals=1}
		  paramValues=nextPar
		}
	}
	
	#dens <- densities(x,y)
	#dens <- floor(99 * dens / max(dens)) + 1
	
	#compute corresponding color - ok since dens are integers
	#col <- colorRampPalette(c("black", "red",space = "Lab"))(100)[dens]
	return(data.frame(x,y,p ^ 1.5))
}


cont <-updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"density.coefficient")
seq <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),20,"density.coefficient")

plot(cont$x,cont$y,col="green",
xlab="Density",ylab="Moran",main="Morphologic imprints",
cex=0.5,pch=1
)
points(seq$x,seq$y,col="blue",
cex=0.2,pch=19
)


#interesting to look at: how distance to centre influence Morphology
# //compared to distance to activities :: argument for polycentrism ??
#beware : color =  relative weight !
#let plot 4 possible params !
par(mfcol=c(2,2))
d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"density.coefficient")
plot(d$x,d$y,
#col=colorRampPalette(c("yellow","black"))(100)[floor(d$p * 100)],
cex=0.3,pch=19,
xlab="Density",ylab="Moran",main="Morphological classification"
)
d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"distance.to.activities.coefficient")
plot(d$x,d$y,col=colorRampPalette(c("yellow","black"))(100)[floor(d$p * 100)],
cex=0.3,pch=19,
xlab="Density",ylab="Moran",main="Accessibility influence"
)
d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"distance.to.center.coefficient")
plot(d$x,d$y,col=colorRampPalette(c("yellow","black"))(100)[floor(d$p * 100)],
cex=0.3,pch=19,
xlab="Density",ylab="Moran",main="Centers influence"
)
d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"distance.to.roads.coefficient")
plot(d$x,d$y,col=colorRampPalette(c("yellow","black"))(100)[floor(d$p * 100)],
cex=0.3,pch=19,
xlab="Density",ylab="Moran",main="Roads influence"
)



