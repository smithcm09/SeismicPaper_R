#building linear modeling dfs
#start with ElectricOnly with Infs already removed
#detach everything else
#attach(ElectricOnlyDF_withNLS1)
#remove rows with 0's only
test <- ElectricOnlyDF_withNLS1
test2 <- subset(test, !test$NLS == "0")
ElectricOnlyDF_missingNLS0 <- test2
USE_THIS_ElectricDF <- ElectricOnlyDF_missingNLS0
#make new variable where Electric Activity has a start value of ((+4.85e-5)/2)
#detach everything else
attach(USE_THIS_ElectricDF)
x = ((+4.85e-5)/2)
USE_THIS_ElectricDF$start_ElectricActivity <- ElectricActivity+x
min(USE_THIS_ElectricDF$start_ElectricActivity)

#create new factorized vairable of 0 and 1 for ltgAll variable
USE_THIS_ElectricDF$Factor_ltgAll <- ltgAll

USE_THIS_ElectricDF$Factor_ltgAll<-ifelse(USE_THIS_ElectricDF$ltgAll == "0",0,1)

USE_THIS_ElectricDF$Factor_ltgAll<-as.factor(USE_THIS_ElectricDF$Factor_ltgAll)
summary(USE_THIS_ElectricDF$Factor_ltgAll)
