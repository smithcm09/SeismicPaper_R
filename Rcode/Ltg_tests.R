# data:
# CRFonlyDF
# NoCRFonlyDF
# NoElectricalDF

attach(CRFonlyDF)
scatterplot(log10(OverallABmedInfra),log10(OverallABmedSeis))
identify(log10(OverallABmedInfra),log10(OverallABmedSeis), col='red')
SIDateTime[58]
detach(CRFonlyDF)

attach(NoCRFonlyDF)
scatterplot(log10(OverallABmedInfra),log10(OverallABmedSeis))
identify(log10(OverallABmedInfra),log10(OverallABmedSeis), col='red')
SIDateTime[319]
detach(NoCRFonlyDF)

attach(NoElectricalDF)
scatterplot(log10(OverallABmedInfra),log10(OverallABmedSeis))
identify(log10(OverallABmedInfra),log10(OverallABmedSeis), col='red')
SIDateTime[618]
detach(NoElectricalDF)

#########
View(post_CRF_ltgCounts)
#verify that indexes line up properly
plot(post_CRF_ltgCounts$NLS,CRFonlyDF$NLS)
#save data file
#save.image("~/SeismicPaper_R.RData")

# compare ltg3plus total and after crf
plot(post_CRF_ltgCounts$ltg3plus,post_CRF_ltgCounts$ltg3plus__1)
#can see that is not perfeclty linear and that for events with 20-40 flashes especially there is a discrepency

test1 <- post_CRF_ltgCounts$ltg3plus-post_CRF_ltgCounts$ltg3plus__1
boxplot(test1)
hist(test1,breaks=50)
summary(test1)

#make new variable of totalpostCRFltg
total_postCRF_ltg <- post_CRF_ltgCounts$ltg3plus__1 + post_CRF_ltgCounts$ltg3__1

#boxplot differences in # of ltg for CRF no CRF and no electrical

Boxplot(total_postCRF_ltg)
Boxplot(NoCRFonlyDF$ltgAll)
Boxplot(NoElectricalDF$ltgAll)

t.test(NoCRFonlyDF$ltgAll, total_postCRF_ltg)
#results show that you can reject the null that they are the same and accept the alternative that the true diff in means is not equal to 0
t.test(NoCRFonlyDF$ltgAll, total_postCRF_ltg, alternative='less')
#the NoCRF group has statistically less ltg then the CRF group with a p-value of 5.165e-0.8

