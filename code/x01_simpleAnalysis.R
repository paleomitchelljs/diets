setwd("C:\\Users\\jsm0036\\Dropbox\\Research\\diets\\data")
outMat <- read.csv("longform.csv", stringsAsFactors=F)

########## NON BAYESIAN ##########
library(nlme)
sMat <- outMat[order(as.character(outMat$genus)),]
library(lme4)
Model <- lmer(ppreymass ~ predmass + (1 | clade/subclade/species) + ( 1 | obstype) + (1 | study), data = sMat, control=lmerControl(optimizer="nloptwrap"))

########## SIMPLE   ##############
library(brms)
bform <- bf( ppreymass ~ predmass + (1 | clade) + (1 | obstype/study) )

Prior <- c(
	prior(student_t(3, 0, 5), class="Intercept", coef=""),
	prior(student_t(3, 0, 5), class=b),
	prior(cauchy(0, 5), class=sd)
	)

fit1 <- brm(
		bform, 
		data = outMat, 
		family = gaussian(), 
		chains = 2, 
		warmup = 1000, 
		iter = 1100, 
		control=list(max_treedepth = 11, adapt_delta=0.9), 
		prior = Prior, 
		cores=1
		)
