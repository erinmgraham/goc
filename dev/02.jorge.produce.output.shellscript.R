# create the shell script to set the arguments and run the models for each species

################# data setup  ###############

# read in fish occurrence data
fish.all = read.csv("/home/jc140298/jorge_setup/fish_all.csv", stringsAsFactors=FALSE)

# get a list of fishnames
fish.names = unique(fish.all$SNAME)

for (fish in fish.names) { #cycle through each of the species

	# remove the space from the fish name
	fish.nospace = gsub(" ", "_", fish)
	
	# set the species specific working directory argument and create it
	wd.arg = paste("/home/jc140298/jorge/", fish.nospace, "/", sep="") 
	# set the species arg
	fish.arg = fish.nospace

	# set the working directory to be the species directory
	setwd(wd.arg)
	# create the shell file
	shell.file = file(paste("/home/jc140298/jorge/", fish.nospace, "/02.jorge.produce.output.", fish.nospace, ".sh", sep=""), "w")
		cat('#!/bin/bash\n', file=shell.file)
		cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
		cat('cd $PBS_O_WORKDIR\n', file=shell.file)
		cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
		cat('module load java\n', file=shell.file) # need for maxent
		cat('module load R\n', file=shell.file) # need for R
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" fish.nospace=\"", fish.arg, "\"' /home/jc140298/jorge_setup/02.jorge.produce.output.R 02.jorge.produce.output.", fish.nospace, ".Rout \n", sep="", file=shell.file)
	close(shell.file)

	shell.file.name = paste("/home/jc140298/jorge/", fish.nospace, "/02.jorge.produce.output.", fish.nospace, ".sh", sep="")
	system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
} # end for species