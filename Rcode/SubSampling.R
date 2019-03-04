#Random SubSampling of Events for Visual Analysis

#replicate dataframe so don't mess it up and take out wonky row
test <- ForRMatchedData_04032018[-c(769,827,1473),]
attach(test)
#use sample function to subsample out a portion of the events (ALL)
Test_samples <- sample(SIDateTime,10)

#Subset out the DataFrame for only those events that have CRF
CRFonlyDF <- subset(test, CRF_yn == 'y')
View(CRFonlyDF)


#Subset out the DataFrame for only those events that DON'T have CRF
NoCRFonlyDF <- subset(test, CRF_yn == 'n')
View(NoCRFonlyDF)

#Subset out the DataFrame for only those events that have no electrical data at all
NoElectricalDF <- subset(test, NLS == 0)
View(NoElectricalDF)
