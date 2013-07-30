#script to produce output from jorge's maxent analysis

# read in the arguments listed at the command line
args=(commandArgs(TRUE))  
for(i in 1:length(args)) { 
	eval(parse(text=args[[i]])) 
}
# expecting wd, fish.nospace

library(png)
library(SDMTools)

# read in fish occurrence data
fish.all = read.csv("/home/jc140298/jorge_setup/fish_all.csv", stringsAsFactors=FALSE)

# function to return top 5 environmental variables
returnOrderedVariables = function(sp.name) {

	# get the maxent results
	me.results = read.csv(file=paste("/home/jc140298/jorge/", sp.name, "/output_maxent/maxentResults.csv", sep=""))
	
	var.names = sapply(strsplit(as.character(colnames(sort(me.results[,8:34], decreasing=TRUE))), "\\."), "[[" ,1)
	var.percContrib = as.numeric(sort(me.results[,8:34], decreasing=TRUE))

	return(list(var.names, var.percContrib))
}

# set the colours for the figure
cols = colorRampPalette(c("violet", "blue", "green", "yellow", "orange", "red"))(20)

###############
#
# create individual species outputs maps 
#
###############

# get the .asc file for the fish
ascfile = read.asc(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, ".asc", sep=""))

# add the space from the fish name to search for occurrence records
fish = gsub("_", " ", fish.nospace)

# get the occurence data for the fish and the number of occurences
fish.occur = fish.all[fish.all$SNAME==fish,]
fish.n = nrow(fish.occur)

# get the auc
me.results = read.csv(file=paste("/home/jc140298/jorge/", fish.nospace, "/output_maxent/maxentResults.csv", sep=""))
fish.auc = me.results$Training.AUC

# create a subtitle with both AUC and num of occurrence records
subtitle = paste("AUC:", fish.auc, " n =", fish.n, sep=" ")

# get list of ordered variable names and percent contributions to add to plot
l_orderedVars = returnOrderedVariables(fish.nospace)

# create .pdf to save figure
pdf(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, ".pdf", sep=""))
	
# create matrix for figure layout	
mat = matrix(c(1,1,2,
	1,1,3,
	1,1,4,
	1,1,5,
	1,1,6),nrow=5,ncol=3,byrow=TRUE)
layout(mat)
	
# change margins to make pretty
par(mar=c(2,0,2,0))

# add image file to plot
image(ascfile, col=cols, main=fish, axes=FALSE, xlab="", ylab="", cex=2)
	
# add subtitle, occurence points, and legend
mtext(subtitle, side=1, cex=0.8)
points(fish.occur$Longitude, fish.occur$Latitude, pch=15)
pnts = cbind(x =c(-114,-113.75,-113.75,-114), y =c(24,24,21,21))
legend.gradient(pnts, cols=cols, title="")

# now add top 5 variable reponse curves
# get the list of all plots
all.plots = list.files(paste("/home/jc140298/jorge/", fish.nospace, "/output_maxent/plots/", sep=""), full.names=TRUE)

# add the top 5 variables to figure
for (i in 1:5) {

	# get the associated response curve thumbnail figure
	var = paste(l_orderedVars[[1]][i], "_thumb.png", sep="")
	img = readPNG(all.plots[grep(var, all.plots)])

	# create a title with it's percent contribution
	infotitle = paste("% Contribution:", l_orderedVars[[2]][i])

	# change margins for more space between figures and plot
	par(mar=c(0.5,0,0.75,0))
	plot(c(0,1), c(0,1), main=infotitle, type = "n", xlab = "", 
		ylab = "", axes=FALSE, cex=0.5)
	rasterImage(img, 0, 0, 1, 1)
}

# close the .pdf 
dev.off()

# create species threshold asc
# first make copy of original asc
tr_ascfile = ascfile

# get the threshold value from maxent
tr = me.results$Balance.training.omission..predicted.area.and.threshold.value.logistic.threshold

# change asc files based on threshold value
tr_ascfile[which(tr_ascfile<tr)]=0
tr_ascfile[which(tr_ascfile>=tr)]=1

# save threshold asc
write.asc(tr_ascfile, file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, "_threshold.asc", sep=""))

# create new subtitle with threshold value 
subtitle = paste("Threshold:", tr, sep=" ")

# make new pdf for threshold figure
pdf(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, "_threshold.pdf", sep=""))
# change margins to make pretty
par(mar=c(2,0,2,0))

# add image file to plot
image(tr_ascfile, col=cols, main=fish, axes=FALSE, xlab="", ylab="", cex=2)

# add subtitle, occurence points, and legend
mtext(subtitle, side=1, cex=0.8)
points(fish.occur$Longitude, fish.occur$Latitude, pch=15)
legend.gradient(pnts, cols=cols, title="")

# close the .pdf
dev.off()
