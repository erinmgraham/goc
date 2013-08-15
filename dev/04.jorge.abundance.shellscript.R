# create the shell script to set the arguments and run the models for each species

################# data setup  ###############

# read in abundance data
fish.abundance = read.csv("/home/jc140298/jorge_setup/DataMares_2007_2010_Abundance_250m2.csv",
	header=TRUE)

# get a list of fishnames
fish.abundance.names = colnames(fish.abundance[,6:70])

for (fish in fish.abundance.names) { #cycle through each of the species

	# set the species specific working directory argument and create it
	wd.arg = "/home/jc140298/jorge_abundance/"

	# set the species arg
	fish.arg = fish

	# set the working directory to be the species directory
	setwd(wd.arg)

	# create the shell file
	shell.file = file(paste("/home/jc140298/jorge/", fish, "/04.jorge.abundance.output.", fish, ".sh", sep=""), "w")
		cat('#!/bin/bash\n', file=shell.file)
		cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
		cat('cd $PBS_O_WORKDIR\n', file=shell.file)
		cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
		cat('module load java\n', file=shell.file) # need for maxent
		cat('module load R\n', file=shell.file) # need for R
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" fish=\"", fish.arg, "\"' /home/jc140298/jorge_setup/04.jorge.abundance.R 04.jorge.abundance.output.", fish, ".Rout \n", sep="", file=shell.file)
	close(shell.file)

	shell.file.name = paste("/home/jc140298/jorge/", fish, "/04.jorge.abundance.output.", fish, ".sh", sep="")
	system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
} # end for species