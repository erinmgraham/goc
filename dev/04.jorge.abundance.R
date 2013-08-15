# Date Created: 20 June 2013
# Last Modified By: EGraham 14.08.13

# this script performs linear and quantile regressions for jorge's abundance
#	data and maxent's environmental suitability
# analysis described in VanDerWal et al 2009

# source("/home/jc140298/jorge_setup/jorge_abundance.R")

# read in the arguments listed at the command line
args=(commandArgs(TRUE))  
for(i in 1:length(args)) { 
	eval(parse(text=args[[i]])) 
}

library(quantreg)
library(SDMTools)

# read in abundance data
fish.abundance = read.csv("/home/jc140298/jorge_setup/DataMares_2007_2010_Abundance_250m2.csv",
	header=TRUE)

# get a list of fishnames
#fish.abundance.names = colnames(fish.abundance[,6:70])

# get a list of sites
site.gps = fish.abundance[,c("Longitude", "Latitude", "Site")]

# create a list of taus (quantiles)
my.taus = c(seq(0.5, 0.95, 0.05), 0.975, 0.99)

# create a list of model names
my.names = c("ols", as.character(my.taus), "nlrq")

# create matrices to hold the slopes and pvalues
slopes = rep(NA, length(my.taus)+2)
pvalues = rep(NA, length(my.taus)+2)

# create a matrix to hold the pseudo R2's (Nagelkerke 1991)
pseudoR2s = rep(NA, length(my.taus)+2)

# function to calculate pseudo R2
# (1 - exp(-2/N *(m.ll - n.ll)))/(1 - exp(2/N * n.ll))
calculatePseudoR2 = function(model, null.model, n) {

	pR2 = (1 - exp(-2/n *(logLik(model)[1] - logLik(null.model)[1])))/
		(1 - exp(2/n * logLik(null.model)[1]))

	return (pR2)
}

# create a matrix to hold the Goodness of Fits (Koenker and Machado 1999)
qr.gof = rep(NA, length(my.taus))

	# get the fish env suitability from maxent output
	# get the .asc file for the fish
	ascfile = read.asc(file=paste("/home/jc140298/jorge/", fish, "/", fish, ".asc", sep=""))
	# extract the value at each site
	es = extract.data(cbind(site.gps$Longitude,site.gps$Latitude), ascfile)

	# get the abundance for that fish
	f.abundance = fish.abundance[colnames(fish.abundance)==fish][,1]

	# create a data.frame of es and abundance
	df.es.f.abundance = as.data.frame(cbind(es, f.abundance))

	# limit abundance estimates to those that fell within the species 
	#	potential distributions (VDW et al 2009, pg 284)
	# retrieve threshold value from maxent results
	me.results = read.csv(paste("/home/jc140298/jorge/", fish, "/output_maxent/maxentResults.csv", sep=""))
	threshold = me.results$Equate.entropy.of.thresholded.and.original.distributions.logistic.threshold
	# VDW.threshold = me.results$Balance.training.omission..predicted.area.and.threshold.value.cumulative.threshold
	# exclude sites with es below threshold from analysis
	tr.df.es.f.abundance = df.es.f.abundance[df.es.f.abundance$es > threshold,]
		
	# divide the abundance by the maximum abundance value recorded for the 
	#	species to rescale all abundance from 0 to 1
	tr.df.es.f.abundance$prop.mean.abundance = tr.df.es.f.abundance$f.abundance/max(tr.df.es.f.abundance$f.abundance)

	# fit OLS
	ols.model = lm(tr.df.es.f.abundance$prop.mean.abundance ~ tr.df.es.f.abundance$es)

	# add the OLS slope to matrix
	slopes[1] = summary(ols.model)$coefficients[2,1]
	pvalues[1] = summary(ols.model)$coefficients[2,4]

	# fit linear quantreg to taus
	qr.model = list(); qr.null.model = list()
	for(i in 1:length(my.taus)){
		qr.model[[i]] = rq(tr.df.es.f.abundance$prop.mean.abundance ~ tr.df.es.f.abundance$es, 
			tau=my.taus[i])

		qr.null.model[[i]] = rq(tr.df.es.f.abundance$prop.mean.abundance ~ 1, 
			tau=my.taus[i])

		# save the LQ slopes
		slopes[1+i] = coef(summary(qr.model[[i]], se="boot"))[2,1]
		pvalues[1+i] = coef(summary(qr.model[[i]], se="boot"))[2,4]
	}

	# fit nonlinear quantreg and save bs, pvals
	nl.qr.model = nlrq(prop.mean.abundance ~ (1-exp(-b*es)), data=tr.df.es.f.abundance,
		start=list(b=0.2), tau=0.975)
	slopes[length(my.names)] = coef(summary(nl.qr.model))[1,1]
	pvalues[length(my.names)] = coef(summary(nl.qr.model))[1,4]

	# calculate a pseudo-R2 measure (Nagelkerke 1991)
	# for ols
	ols.null.model = lm(tr.df.es.f.abundance$prop.mean.abundance~1)
	pseudoR2s[1] = calculatePseudoR2(ols.model, ols.null.model, length(tr.df.es.f.abundance$es))

	# for linear quantreg
	for(i in 1:length(my.taus)){
		pseudoR2s[1+i] = calculatePseudoR2(qr.model[[i]], qr.null.model[[i]], 
			length(tr.df.es.f.abundance$es))
	}

	# for nonlinear quantreg
	pseudoR2s[length(my.names)] = calculatePseudoR2(nl.qr.model, ols.null.model, length(tr.df.es.f.abundance$es))

	# calculate local measure of goodness-of-fit (Koenker and Machado 1999)
	# R1 <- 1 - f1$rho/f0$rho 
	# for linear quantreg

	for(i in 1:length(my.taus)){
		qr.gof[i] = 1 - qr.model[[i]]$rho/qr.null.model[[i]]$rho 
	}

	# write results to file
	# kludge to make qr.fog the same length
	qr.gof.tosave = c(NA, qr.gof, NA)
	outputs = cbind(slopes, pvalues, pseudoR2s, qr.gof.tosave)
	write.csv(outputs, file=paste(fish, "_abundance.csv", sep=""), row.names = my.names)
	
	# save plot
	# create .pdf to save figure
	pdf(file=paste("/home/jc140298/jorge_abundance/", fish, "_abundance.pdf", sep=""))
	plot(tr.df.es.f.abundance$es, tr.df.es.f.abundance$prop.mean.abundance, 
		main=fish, xlab="Predicted Environmental Suitability", ylab="Proportionate Abundance",
		xlim=c(0,1))
	abline(ols.model, lty=1)
	text(0.2, max(tr.df.es.f.abundance$prop.mean.abundance), 
		paste("OLS R2: ", round(summary(ols.model)$r.squared,digits=4)))

	for(i in 1:length(my.taus)){
		abline(rq(tr.df.es.f.abundance$prop.mean.abundance ~ tr.df.es.f.abundance$es,
		tau=my.taus[i]), col=i+1, lty=i)
	}
	newx=seq(0,1,by=0.01)
	lines(newx, predict(nl.qr.model, newdata=list(es=newx)), col=4)
	}
	
	# close the .pdf 
	dev.off()
		
}