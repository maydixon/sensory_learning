# Standard Anova on these data

mod1 <- lm(value ~  variable* Species, data = mLocVSound)
ANOVA <- summary(aov(mod1))
cat( " The standard ANOVA for these data follows ","\n")
Fsize <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
Fmonths <-  ANOVA[[1]]$"F value"[2]
Finteract <-  ANOVA[[1]]$"F value"[3]
print(ANOVA, "\n")
cat( "\n")
cat( "\n")

print( "Resampling as in Manly with unrestricted sampling of observations. ")
# Now start resampling
nreps <- 5000 
Fcue <- numeric(nreps)    #Set up space to store F values as calculated.
FSpec <- numeric(nreps)  
FCS <- numeric(nreps)
Fcue[1] <- Fcue          # The first F of our 5000 
FSpec[1] <- FSpecies
FCS[1] <- Finteract
for (i in 2:nreps) {
      newLocVSound <- sample(mLocVSound$value, 72)
      mod2 <- lm(value ~  variable* Species, data = mLocVSound)
      b <- summary(aov(mod2))
      FS[i] <- b[[1]]$"F value"[1]
      FM[i] <- b[[1]]$"F value"[2]
      FSM[i] <- b[[1]]$"F value"[3]
}
probS <- length(FS[FS >= Fsize + .Machine$double.eps ^0.5])/nreps
probM <- length(FM[FM >= Fmonths+ .Machine$double.eps ^0.5])/nreps       
probSM  <-  length(FSM[FSM >= Finteract + .Machine$double.eps ^0.5])/nreps
### The addition of "+ .Machine$double.eps" is an aid against two numbers that differ only by
### floating point computer calculations at the extreme.

cat(" The probability value for the interaction is ",probSM, "\n")
cat(" The probability value for Size is ", probS, "\n")
cat(" The probability value for Months is ", probM, "\n")