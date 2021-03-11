library(ggplot2)

#plot of economic time series for convergence assessment

eco <- read.csv("/Users/Juste/Documents/Cours/ComplexSystemsMadeSimple/UrbanMorphogenesis/Results/Economic/200repets10eco.csv",sep=";")

#data are in column

# we set num_repets and index wanted

nrepets = 200
nrepetseco = 10
maxtime = 250
beginindex_rent = 4
beginindex_segreg = 5
step_index = 3

plotEco <- function(config_numbers){
	rent=c();rentse=c();
	segreg=c();segregse=c();
	groups=c();time=c();
	for(i in config_numbers - 1){
		for(j in 0:(maxtime-1)){
      drent=c()
      for(rep in seq(from=(i* nrepets)+1,by=nrepetseco,length.out=nrepetseco)){
        drent = append(drent,mean(unlist(eco[seq(from=(i* nrepets )+(rep * nrepetseco) + 1,by=1,length.out=nrepetseco),beginindex_rent + (step_index * j)])))
      }
			rent = append(rent,mean(drent));
			rentse=append(rentse,sd(drent));
			
			
      dsegreg=c()
      for(rep in seq(from=(i* nrepets)+1,by=nrepetseco,length.out=nrepetseco)){
        dsegreg = append(dsegreg,mean(unlist(eco[seq(from=(i* nrepets )+(rep * nrepetseco) + 1,by=1,length.out=nrepetseco),beginindex_segreg + (step_index * j)])))
      }    
      segreg = append(segreg,mean(dsegreg));
			segregse=append(segregse,sd(dsegreg));		
			
			groups=append(groups,i)
			time=append(time,j)
		}	
	}
	dat = data.frame(rent,rentse,segreg,segregse,groups,time)

	limitsrent <- aes(ymax = rent + rentse, ymin= rent - rentse)
	prent <- ggplot(dat, aes(colour=groups, y= rent, x= time))
	prent + geom_line(aes(group=groups)) + geom_errorbar(limitsrent, width=0.2)
	
	#limitssegreg <- aes(ymax = segreg + segregse, ymin= segreg - segregse)
	#psegreg <- ggplot(dat, aes(colour=groups, y= segreg, x= time))
	#psegreg + geom_line(aes(group=groups)) + geom_errorbar(limitssegreg, width=0.2)
	
}

removeOutliers <- function(x,proportionMax){
  res = x
  for(i in 1:floor(length(res)*proportionMax/2)){
    test <- grubbs.test(res,type=11)
    if(test$p.value > 0.1){
      res = res[which(res!=min(res)&&res!=max(res))]
    }
  }
}



