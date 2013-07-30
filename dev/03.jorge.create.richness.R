#script to create species richness asciis and pdfs from jorge's maxent analysis

library(png)
library(SDMTools)

# read in fish occurrence data
fish.all = read.csv("/home/jc140298/jorge_setup/fish_all.csv", stringsAsFactors=FALSE)

# get a list of fishnames
fish.names = unique(fish.all$SNAME)

# create asc from first species asc
# remove the space from the fish name
fish.nospace = gsub(" ", "_", fish.names[1])

# get the .asc files for the first fish to start
richness.ascfile = read.asc(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, ".asc", sep=""))
richness.tr_ascfile = read.asc(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, "_threshold.asc", sep=""))

# for the rest of the fish
for (fish in fish.names[-1]) {

	# remove the space from the fish name
	fish.nospace = gsub(" ", "_", fish)

	# get the .asc files for the fish
	ascfile = read.asc(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, ".asc", sep=""))
	tr_ascfile = read.asc(file=paste("/home/jc140298/jorge/", fish.nospace, "/", fish.nospace, "_threshold.asc", sep=""))

	richness.ascfile = richness.ascfile + ascfile
	richness.tr_ascfile = richness.tr_ascfile + tr_ascfile
}

# save richness ascs
write.asc(richness.ascfile, file="/home/jc140298/jorge_setup/Richness.asc")
write.asc(richness.tr_ascfile, file="/home/jc140298/jorge_setup/Richness_threshold.asc")

# save the logistic ascii as pdf
pdf(file="/home/jc140298/jorge_setup/Species_richness.pdf")

# set the figure cols
cols = colorRampPalette(c("violet", "blue", "green", "yellow", "orange", "red"))(20)
# add asc image
image(richness.ascfile, col=cols, main="Species Richness \nlogistic", axes=FALSE, xlab="", ylab="", cex=2)
# create legend
pnts = cbind(x =c(-114,-113.75,-113.75,-114), y =c(24,24,21,21))
legend.gradient(pnts, cols=cols, title="")
# EMG I want to have divisions between 0 and 1
dev.off()

# save the threshold ascii as pdf
pdf(file="/home/jc140298/jorge_setup/Species_richness_threshold.pdf")
image(richness.tr_ascfile, col=cols, main="Species Richness \nthreshold", axes=FALSE, xlab="", ylab="", cex=2)
legend.gradient(pnts, cols=cols, title="")
dev.off()
