# create the shell script to set the arguments and run the models for each species

# get a list of species directories
# these were created by jorge.helper.R
all.species = list.files("/home/jc140298/jorge/") #get a list of all the species

for (sp in all.species) { #cycle through each of the species

	# set the species specific working directory argument and create it
	wd.arg = paste("/home/jc140298/jorge/", sp, "/", sep="") 
	# set the species arg
	species.arg = sp	

	# set the working directory to be the species directory
	setwd(wd.arg) 
	# create the shell file
	shell.file = file(paste("/home/jc140298/jorge/", sp, "/01.jorge.model.current.", sp, ".sh", sep=""), "w")
		cat('#!/bin/bash\n', file=shell.file)
		cat('#PBS -j oe\n', file=shell.file) # combine stdout and stderr into one file
		cat('cd $PBS_O_WORKDIR\n', file=shell.file)
		cat('source /etc/profile.d/modules.sh\n', file=shell.file) # need for java
		cat('module load java\n', file=shell.file) # need for maxent
		cat('module load R\n', file=shell.file) # need for R
		cat("R CMD BATCH --no-save --no-restore '--args wd=\"", wd.arg, "\" species=\"", species.arg, "\"' /home/jc140298/jorge_setup/01.jorge.model.current.R 01.jorge.model.current.", sp, ".Rout \n", sep="", file=shell.file)
	close(shell.file)

	shell.file.name = paste("/home/jc140298/jorge/", sp, "/01.jorge.model.current.", sp, ".sh", sep="")
	system(paste("qsub -l nodes=1:ppn=1 -l pmem=2gb ", shell.file.name, sep=""))
} # end for species