#script to run regressions using jorge's maxent analysis and abundances

library(quantreg)
library(SDMTools)

# read in abundance data
fish.abundance = read.csv("c:/userdata/SDM/jorge/DataMares_2007_2010_Abundance_250m2.csv",
	header=TRUE)

# get a list of fishnames
fish.abundance.names = colnames(fish.abundance[,6:70])

# get a list of sites
site.gps = fish.abundance[,c("Longitude", "Latitude", "Site")]

for (fish in fish.abundance.names) {

	# get the fish env suitability from maxent output
	# get the .asc file for the fish
	ascfile = read.asc(file=paste("z:/jorge/", fish, "/", fish, ".asc", sep=""))
	# extract the value at each site
	es = extract.data(cbind(site.gps$Longitude,site.gps$Latitude), ascfile)

	# get the abundance for that fish
	f.abundance = fish.abundance[colnames(fish.abundance)==fish][,1]

	# create a data.frame of es and abundance
	df.es.f.abundance = as.data.frame(cbind(es, f.abundance))

	# limit abundance estimates to those that fell within the species 
	#	potential distributions (VDW et al 2009, pg 284)
	# retrieve threshold value from maxent results
	me.results = read.csv(paste("z:/jorge/", fish, "/output_maxent/maxentResults.csv", sep=""))
	threshold = me.results$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold
	# VDW.threshold = me.results$Balance.training.omission..predicted.area.and.threshold.value.cumulative.threshold
	# exclude sites with es below threshold from analysis
	tr.df.es.f.abundance = df.es.f.abundance[df.es.f.abundance$es > threshold,]
		
	# divide the abundance by the maximum abundance value recorded for the 
	#	species to rescale all abundance from 0 to 1
	tr.df.es.f.abundance$prop.mean.abundance = tr.df.es.f.abundance$f.abundance/max(tr.df.es.f.abundance$f.abundance)

	# fit OLS
	ols.model = lm(tr.df.es.f.abundance$prop.mean.abundance ~ tr.df.es.f.abundance$es)
	plot(tr.df.es.f.abundance$es, tr.df.es.f.abundance$prop.mean.abundance, 
		main=fish, xlab="Predicted Environmental Suitability", 
		ylab="Proportionate Abundance", xlim=c(0,1))
	abline(ols.model, lty=3)
#	text(0.2, max(tr.df.es.f.abundance$prop.mean.abundance), 
#		paste("R2: ", round(summary(ols.model)$r.squared,digits=4)))

	# fit linear quantreg
#	taus = c(0.5, 0.75, 0.9, 0.95, 0.975, 0.99) # some of these are wonky
	taus = c(0.9, 0.975)
	qr.model = rq(tr.df.es.f.abundance$prop.mean.abundance ~ tr.df.es.f.abundance$es, 
		tau=taus)
	for(i in 1:length(taus)){
		abline(rq(tr.df.es.f.abundance$prop.mean.abundance ~ tr.df.es.f.abundance$es,
		tau=taus[i]), col=1+i, lty=i)
	}

	# fit nonlinear quantreg
	nl.qr.model = nlrq(prop.mean.abundance ~ (1-exp(-b*es)), data=tr.df.es.f.abundance,
		start=list(b=0.2), tau=0.975)
	lines(sort(tr.df.es.f.abundance$es), sort(predict(nl.qr.model, 
		newdata=list(x=tr.df.es.f.abundance$es))), col=4)

	# calculate goodness-of-fit measure (Koenker and Machado 1999)
	# TBD

	# calculate a pseudo R2 measure (Nagelkerke 1991)
	# TBD
}