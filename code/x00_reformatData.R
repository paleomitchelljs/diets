###################
setwd("C:\\Users\\jsm0036\\Dropbox\\Research\\diets\\data")
library(MASS)

####################
#######For diet:
# log(prey mass) - log(predator mass)
# what is the shape of that distribution?
# is it normal? log normal? Estimate using skewed normal distr?
# estimate for each study, species, genus, family, clade, all predators
# then try and determine where dinosaurs fall 

####################
genDistro <- function(preycount, preymasses)	{
	if (class(preycount) != "numeric")	{
		preycount <- as.numeric(preycount)
	}
	outVec <- 0
	for (i in 1:length(preycount))	{
		if (!is.na(preycount[i]))	{
			Deads <- rep(preymasses[i], preycount[i])
			outVec <- c(outVec, Deads)
		}
	}
	outVec <- outVec[-1]
	outVec <- unlist(outVec)
	outVec <- as.numeric(outVec)
	return(outVec)
}
####################
####################
Table <- read.csv('raw_data.csv', header=F, stringsAsFactors=FALSE)
StartCol <- 31

Nobs <- Table[4:nrow(Table), which(Table[3,] == "Nobservations")]
ObsType <- Table[4:nrow(Table), which(Table[3,] == "ObsType")]
Class <- Table[4:nrow(Table), which(Table[3,] == "Class")]
Order <- Table[4:nrow(Table), which(Table[3,] == "Clade")]
Genus <- Table[4:nrow(Table), which(Table[3,] == "Genus")]
Species_epithet <- apply(Table[4:nrow(Table), c(which(Table[3,] == "Genus"), which(Table[3,] == "Species"))], 1, paste, sep="_", collapse="_")
PredMass <- Table[4:nrow(Table), which(Table[3,] == "Mass")]
PreyMass <- Table[3, StartCol:ncol(Table)]

PreyGenus <- as.character(Table[1, StartCol:ncol(Table)])
PreySpecies <- as.character(Table[2, StartCol:ncol(Table)])
PreySp <- apply(cbind(PreyGenus, PreySpecies), 1, paste, sep="_", collapse="_")

Data <- Table[4:nrow(Table), StartCol:ncol(Table)]
RefN <- Table[4:nrow(Table), 1]

##### Make long-format data
Means <- c()
SDs <- c()
for (j in 1:nrow(Data))	{
	preyMassList <- genDistro(Data[j,], PreyMass)
	preyMassList <- na.omit(preyMassList)
	pPrey <- log(as.numeric(preyMassList)) - log(as.numeric(PredMass[j]))
	Means[j] <- mean(pPrey, na.rm=T)
	SDs[j] <- sd(pPrey, na.rm=T)
	studyN <- rep(RefN[j], length(preyMassList))
	studyN <- as.factor(studyN)
	predMat <- data.frame(preyMass=log(as.numeric(preyMassList)), ppreymass = pPrey, predmass=rep(log(as.numeric(PredMass[j])), length(preyMassList)), species=rep(Species_epithet[j], length(preyMassList)), genus=rep(Genus[j], length(preyMassList)), subclade=rep(Order[j], length(preyMassList)), clade=rep(Class[j], length(preyMassList)), obstype=rep(ObsType[j], length(preyMassList)), study=studyN)
	if (j == 1)	{
		outMat <- predMat
	}
	else if (j > 1)	{
		outMat <- rbind(outMat, predMat)
	}

}

write.csv(outMat, 'longform.csv', quote=F, row.names=F)