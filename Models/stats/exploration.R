
#library(scatterplot3d)
#lattice is better
library(lattice)



#stats on outputs

dat <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/UrbanMorphogenesis/Results/Robustness/500manyparamsGood.csv",sep=";")

#dat$eval.activities <- dat$eval.activities/max(unlist(dat$eval.activities))
mor <- dat$spatial.autocorrelation.index


robustnessPositions <- function(){
	
	
	#summary(dat)
	par(mfcol=c(2,2),bg="lightyellow")
	hist(dat$eval.density,breaks=100,xlab="Density",main="")
    hist(dat$spatial.autocorrelation.index,breaks=100,xlab="Moran",main="")
	hist(dat$eval.speed,breaks=100,xlab="Speed",main="")
	hist(dat$eval.activities,breaks=100,xlab="Accessibility",main="")

}

robustnessPositions()

histFit <-function(rows,col,xlim,ylim,prop,xlab,colors){
	#rows in 0:(floor(length(dat[,1])/n_repets)-1)
  n_repets = 500
	
  #plot.new()
  
  #par(mfcol=c(1,1))
  
  k=1
  
	for(i in rows){
    d = dat[((i * n_repets) + 1):((i+1)*n_repets),col]
    d = removeOutliers(d,prop)
    #c = paste("light",colors[k])
    #if(c=="light red"){c="red3"}
		h = hist(d,plot=TRUE,breaks=50,
             xlim=xlim,ylim=ylim,
             add=(i!=rows[1]),col=colors[k],border=colors[k],
             xlab=xlab,main="")
		K = max(h$counts)
		curve(K*exp(- ((x - mean(d))^2)/(2*sd(d)^2)),add=TRUE,col=paste("darkred"),n=300,lwd=2)
		#plot(h$mids,h$counts)
    k=k+1
	}
}

robManyConfs <- function(cols){ 
  par(mfcol=c(2,2),bg="white")
  histFit(cols,5,c(0,1),c(0,50),1.5,"Density",c("red3","green","yellow"))
  histFit(cols,6,c(-0.05,1),c(0,30),0.5,"Moran",c("red3","green","yellow")) 
  histFit(cols,7,c(1,3),c(0,45),1.5,"Speed",c("red3","green","yellow")) 
  histFit(cols,8,c(0,1),c(0,30),1.5,"Accessibility",c("red3","green","yellow"))
  
}

robManyConfs(c(7,3,1))

removeOutliers<-function(x,prop){
  if(prop>0){
    x=x[which(abs(x-mean(x))<sd(x)*prop)]
  #res = x;num=floor(length(x)*prop)
  #for(i in 1:num){
  #  p1 = chisq.out.test(res,opposite=FALSE)$p.value
  #  p2 = chisq.out.test(res,opposite=TRUE)$p.value
  #  ma = max(res);mi=min(res);me=mean(res)
  #  if(abs(ma-me)>abs(mi-me)){if(p1<p2){res=res[!which(res==ma)]}else{res=res[!which(res==mi)]}}
  #  else{if(p1<p2){res=res[!which(res==mi)]}else{res=res[!which(res==ma)]}}
  #}
  }
  return(x)
}




#Grid

grid <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/UrbanMorphogenesis/Results/GridExploration/gridGood.csv",sep=";")

plot3d <- function(reporterName,xParamName,yParamName, otherParams,otherParamsValues,theta,phi,title,xlab,ylab){
	
	x <- sort(unique(grid[[xParamName]]))
	y <- sort(unique(grid[[yParamName]]))
	z = matrix(nrow=length(x),ncol=length(y))
	xcors = matrix(nrow=length(x),ncol=length(y))
	ycors = matrix(nrow=length(x),ncol=length(y))
	for(i in 1:length(x)){
		for(j in 1:length(y)){
      #if(i!=1&&j!=1){
			z[i,j] = getReporterValue(c(xParamName,yParamName, otherParams),c(x[i],y[j], otherParamsValues),reporterName,grid)
			xcors[i,j]=x[i];ycors[i,j]=y[j]
      #}
		}
	}
  
	#wireframe(z~xcors*ycors,data=data.frame(z,xcors,ycors),aspect = c(1,1),col="blue",shade=TRUE,light.source = c(10,0,10))
	
	#wireframe(x = z,row.values = x, column.values = y ,angle=50,scales = list(arrows = FALSE,distance=c(2,2,2)),screen = list(z = 30, x = -60),drape = TRUE,xlab=xParamName,ylab=yParamName,zlab=reporterName)
	
	#persp function is more ergonomic
	persp(x=x,y=y,z=z,r=10,theta=theta,phi=phi,col="lightblue",xlab=xlab,ylab=ylab,zlab="",shade = 0.75, ticktype = "detailed",cex.lab=0.8,cex.axis=0.6,main=title)
	
}

#let plot different reporters 
par(mfcol=c(2,2))
par(mar=c(1,2,1,2))
plot3d("eval.density","distance.to.activities.coefficient","distance.to.roads.coefficient",c("distance.to.center.coefficient","density.coefficient"),c(0,0),225,25,"Density",xlab="alpha_4",ylab="alpha_2")
plot3d("spatial.autocorrelation.index","distance.to.center.coefficient","density.coefficient",c("distance.to.activities.coefficient","distance.to.roads.coefficient"),c(0,0),50,25,"Moran Index",xlab="alpha_3",ylab="alpha_1")
plot3d("eval.speed","distance.to.activities.coefficient","density.coefficient",c("distance.to.center.coefficient","distance.to.roads.coefficient"),c(0,0),320,35,"Speed",xlab="alpha_4",ylab="alpha_1")
plot3d("eval.activities","distance.to.activities.coefficient","density.coefficient",c("distance.to.center.coefficient","distance.to.roads.coefficient"),c(0,0),45,35,"Accessibility",xlab="alpha_4",ylab="alpha_1")




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

update <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/UrbanMorphogenesis/Results/UpdateType/100houses.csv",sep=";")


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
	col <- colorRampPalette(c("darkgrey", "red"))(100)[dens]
	
  par(mfcol=c(1,1))
  par(mar=c(5,5,5,5))
  
	plot(x[order(col)],y[order(col)],
	pch=19,
	cex=0.5,
	col=sort(col),
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
	return(data.frame(x,y,p))
}

par(mfcol=c(1,1))
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
p=d$p[order(d$p,decreasing=TRUE)];x=d$x[order(d$p,decreasing=TRUE)];y=d$y[order(d$p,decreasing=TRUE)]
plot(x,y,
col=colorRampPalette(c("yellow","black"))(100)[floor(p * 100)],
#cex=1+(p),
pch=3,lwd=2,
xlab="Density",ylab="Moran",main="Density influence"
)

d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"distance.to.activities.coefficient")
p=d$p[order(d$p,decreasing=TRUE)];x=d$x[order(d$p,decreasing=TRUE)];y=d$y[order(d$p,decreasing=TRUE)]
plot(x,y,
     col=colorRampPalette(c("yellow","black"))(100)[floor(p * 100)],
     #cex=1+(p),
     pch=3,lwd=2,
xlab="Density",ylab="Moran",main="Accessibility influence"
)
d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"distance.to.center.coefficient")
p=d$p[order(d$p,decreasing=TRUE)];x=d$x[order(d$p,decreasing=TRUE)];y=d$y[order(d$p,decreasing=TRUE)]
plot(x,y,
     col=colorRampPalette(c("yellow","black"))(100)[floor(p * 100)],
     #cex=1+(p),
     pch=3,lwd=2,
xlab="Density",ylab="Moran",main="Centers influence"
)
d <- updatesCoords(c("distance.to.activities.coefficient","density.coefficient","distance.to.center.coefficient","distance.to.roads.coefficient"),1,"distance.to.roads.coefficient")
p=d$p[order(d$p,decreasing=TRUE)];x=d$x[order(d$p,decreasing=TRUE)];y=d$y[order(d$p,decreasing=TRUE)]
plot(x,y,
     col=colorRampPalette(c("yellow","black"))(100)[floor(p * 100)],
     #cex=1+(p),
     pch=3,lwd=2,
xlab="Density",ylab="Moran",main="Roads influence"
)



#plot for application
app <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/project/Results/Application/atlantiscorrect.csv",sep=";")

#plot Pareto graph
plot(app[["eval.economic"]],app[["eval.activities"]]
,xlab="Economic performance",ylab="Accessibility",main="Pareto plot of configurations"
)

getIndById<-function(id){
	res = 0
	for(i in 1:length(app[["id"]])){
		if(app[["id"]][i]==id) {res = i}
	}
	return(res)
}

getIndByVal<-function(ec,ac){
	res = c()
	for(i in 1:length(app[["id"]])){
		if(app[["eval.economic"]][i]<ec&&app[["eval.activities"]][i]<ac) {res = append(res,app[["id"]][i])}
	}
	return(res)
}


#get index of real sit
r = getIndById(121222112)
#corresponding values
app[["eval.economic"]][r];app[["eval.activities"]][r];

#get conf of good sits
getIndByVal(0.065,0.66)
#->result 122211112, 221111121
getIndByVal(0.06,0.725)
#122111212




