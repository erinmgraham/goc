
# Date Created: 7 June 2013
# Last Modified By: EGraham 15.08.13

# this script helps prepare Jorge's data for maxent analysis, specifically:
# create csv's for each species with just lon, lat from original data file
# create background SWD csv using maxent tool 
# collate output from 04.abundance scripts

###############
#
# create csv's for each species with just lon, lat
#
###############

# read in original data file
fish.all = read.csv("z:/jorge/fish_all.csv", stringsAsFactors=FALSE)

# get a list of species names
fish.names = unique(fish.all$SNAME)

# for each species, create a folder with occurrence data
for (fish in fish.names) {

	# get that species' records	
	sp.records = fish.all[fish.all$SNAME == fish,]

	# extract occurrence data (just lon and lat)
	fish.occur = sp.records[,c("Longitude", "Latitude")]

	# create new data file
	# first remove space from fish name
	fish.nospace = sub(" ", "_", fish)
	# create directory
	dir.create(path=paste("z:/jorge/", fish.nospace, sep=""), recursive=TRUE)
	# write file 
	write.csv(fish.occur, file=paste("z:/jorge/", fish.nospace, "/occur.csv", sep=""), row.names=FALSE)
}

###############
#
# create background SWD csv using maxent tool 
# 	java -cp maxent.jar density.tools.RandomSample num grid1 grid2
#	
#	EMG: num=10000+num.occurrence.records=10000+6029=16029									
#		
###############

# create system command text
bkgd.str = paste("java -cp z:/maxent.jar density.tools.RandomSample 16029 z:/jorge/ascii/*.asc") 
# make the system call and store the output
bkgd = system(bkgd.str, intern=TRUE)
# write the file
write(bkgd, file="z:/jorge/bkgd.csv")
# EMG: Note lon lat is x y and species is background instead of bkgd



###############
#
# collate output from 04.abundance scripts
#
#	Create a Slopes vs Regression Type (Fig 3a JVW et al 2009)
#	Create a Goodness of fit vs Regression Type Fig 3b JVW et al 2009)
#	Create multiple pdfs of model fit figures
#
###############


library(png)

## Needed for tryCatch'ing:
err.null <- function (e) return(NULL)

# create a list of taus (quantiles) for row names
my.taus = c(seq(0.5, 0.95, 0.05), 0.975, 0.99)

# create a list of model names for row names
my.names = c("ols", as.character(my.taus), "nlrq")

# read in abundance data
#fish.abundance = read.csv("/home/jc140298/jorge_setup/DataMares_2007_2010_Abundance_250m2.csv",
fish.abundance = read.csv("z:/jorge_setup/DataMares_2007_2010_Abundance_250m2.csv",
	header=TRUE)

# get a list of fishnames
fish.abundance.names = colnames(fish.abundance[,6:70])

# create matrices for each column of outputs
#	slopes, pvalues, pseudoR2s, qr.gof
slopes = matrix(data=NA, nrow=14, ncol=length(fish.abundance.names))
colnames(slopes)=fish.abundance.names
pvalues = matrix(data=NA, nrow=14, ncol=length(fish.abundance.names))
colnames(pvalues)=fish.abundance.names
pseudoR2s = matrix(data=NA, nrow=14, ncol=length(fish.abundance.names))
colnames(pseudoR2s)=fish.abundance.names
qr.gof = matrix(data=NA, nrow=14, ncol=length(fish.abundance.names))
colnames(qr.gof)=fish.abundance.names

# for each species, collate the slopes and goodnesses of fit
for (fish in fish.abundance.names) {

	f = which(fish.abundance.names == fish)
#	outputs = tryCatch(read.csv(file=paste("/home/jc140298/jorge_abundance/", fish, "_abundance.csv", sep=""), 
	outputs = tryCatch(read.csv(file=paste("z:/jorge_abundance/", fish, "_abundance.csv", sep=""), 
		row.names=c(1)), error = err.null)
	if (!is.null(outputs)) {
		slopes[,f] = outputs$slopes
		pvalues[,f] = outputs$pvalues
		pseudoR2s[,f] = outputs$pseudoR2s
		qr.gof[,f] = outputs$qr.gof.tosave
	} else {
		slopes[,f] = rep(NA, 14)
		pvalues[,f] = rep(NA, 14)
		pseudoR2s[,f] = rep(NA, 14)
		qr.gof[,f] = rep(NA, 14)
	}
}


# determine which slopes coefficients that were significantly different from 
#	zero and calculate mean and se of these coefficients
slope.means = rep(NA, 14)
slope.ses = rep(NA, 14)
for (r in 1:nrow(slopes)) {

	sig.slopes = slopes[r, which(pvalues[r,] < 0.05,)]
	slope.means[r] = mean(sig.slopes)
	slope.ses[r] = sd(sig.slopes)/length(sig.slopes)
}

# plot mean +- se on single figure (Fig 3a VDW et al 2009)
plot(1:13, slope.means[1:13], xlab="Regression Type", ylab="Slope", 
	type="b", xaxt="n")
axis(side=1, at=1:13, labels=my.names[1:13], cex.axis=0.75)
arrows(1:13, slope.means[1:13]+slope.ses[1:13], 1:13, 
	slope.means[1:13]-slope.ses[1:13], code=3, length=0.05, angle=90)

# calculate means and se of goodness of fits
ps.means = rep(NA, 14)
ps.ses = rep(NA, 14)
gof.means = rep(NA, 14)
gof.ses = rep(NA, 14)
for (r in 1:nrow(pseudoR2s)) {

	ps.means[r] = mean(as.numeric((pseudoR2s[r,pseudoR2s[r,]>0])), na.rm=TRUE)
	ps.ses[r] = sd(as.numeric((pseudoR2s[r,pseudoR2s[r,]>0])), na.rm=TRUE)/length(pseudoR2s[r,pseudoR2s[r,]>0])
	gof.means[r] = mean(as.numeric((qr.gof[r,qr.gof[r,]>0])), na.rm=TRUE)
	gof.ses[r] = sd(as.numeric((qr.gof[r,qr.gof[r,]>0])), na.rm=TRUE)/length(qr.gof[r,qr.gof[r,]>0])
}

# plot gofs (Fig 3b VDW et al 2009)
# create new plot window
windows() 
# plot pseudo regressions
plot(1:14, ps.means, xlab="Regression Type", ylab="Goodness of fit", 
	type="b", xaxt="n", pch=19, lty=2)
# custom axis labels
axis(side=1, at=1:14, labels=my.names, cex.axis=0.75)
# add se
arrows(1:14, ps.means+ps.ses, 1:14, ps.means-ps.ses, code=3, length=0.05, 
	angle=90)
# do the same for the goodness of fit
lines(1:14, gof.means, type="b")
arrows(1:14, gof.means+gof.ses, 1:14, gof.means-gof.ses, code=3, length=0.05, 
	angle=90)

legend(1, max(ps.means, na.rm=TRUE), legend=c("pseudoR2", "gof"), 
	pch=c(19,1), lty=c(2,1), bty="n")


# collate species pdfs into smaller number of files (3x3)
for (fish in fish.abundance.names) {

	f = which(fish.abundance.names == fish)

	# get the fish's figure
#	outpng = tryCatch(readPNG(paste("/home/jc140298/jorge_abundance/", fish, "_abundance.png", sep="")), 
	outpng = tryCatch(readPNG(paste("z:/jorge_abundance/", fish, "_abundance.png", sep="")), 
		error = err.null)

	# create a new plot window depending on which species number
	if(f %in% seq(1,65,9)) {
		windows()
		par(mfrow=c(3,3), mar=c(0,0,0,0))
	}
		# if the fish plot is there, add it to the new window
		if (!is.null(outpng)) {
			plot(c(0,1), c(0,1), main="", type = "n", xlab = "", ylab = "", 
				axes=FALSE)
			rasterImage(outpng, 0, 0, 1, 1)
		}

		# if the plot window is full, save as pdf
		if (f %in% seq(9,65,9)) {

			# save the figure as a .pdf
			dev.copy(pdf, paste("z:/jorge_abundance/", f, ".pdf", sep=""))
			dev.off()
		}
}
