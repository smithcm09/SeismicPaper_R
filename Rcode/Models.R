m1 <- lm(NLS~MaxVASR, data=ForRMatchedData_04032018)
summary(m1)
m2 <- lm(NLS~VASR_AB, data=ForRMatchedData_04032018)
summary(m2)
m3 <- lm(NLS~OverallABmedInfra+OverallABmedSeis, data=ForRMatchedData_04032018)
summary(m3)
vif(m3)
m4 <- lm(NLS~VerticalABmedSeis+HorzABmedSeis, data=ForRMatchedData_04032018)
summary(m4)
m5 <- lm(NLS~OverallABmedSeis+VerticalABmedSeis+HorzABmedSeis, data=ForRMatchedData_04032018)
summary(m5)

mea1 <- lm(ElectricActivity~MaxVASR, data=ForRMatchedData_04032018)
summary(mea1)
mea2 <- lm(ElectricActivity~VASR_AB, data=ForRMatchedData_04032018)
summary(mea2)
mea3 <- lm(ElectricActivity~OverallABmedInfra+OverallABmedSeis, data=ForRMatchedData_04032018)
summary(mea3)
vif(mea3)
mea4 <- lm(ElectricActivity~VerticalABmedSeis+HorzABmedSeis, data=ForRMatchedData_04032018)
summary(mea4)
mea5 <- lm(ElectricActivity~OverallABmedSeis+VerticalABmedSeis+HorzABmedSeis, data=ForRMatchedData_04032018)
summary(mea5)



#transformed OverallMeds - Log
LOI <- log(OverallABmedInfra)
LOS <- log(OverallABmedSeis)
#remove Inf and NANs that result from the log
LOI[is.infinite(LOI) | is.nan(LOI) ] <- NA
summary(LOI)
LOS[is.infinite(LOS) | is.nan(LOS) ] <- NA
summary(LOS)

#run model
mod1 <- lm(NLS~LOI+LOS)
summary(mod1)
vif(mod1)

mod2 <- lm(ElectricActivity~LOI+LOS)
summary(mod2)

mod3 <- lm(ltgAll~LOI+LOS)
summary(mod3)

mod4 <- lm(ltg3plus~LOI+LOS)
summary(mod4)

mod5 <- lm(ltgDur~LOI+LOS)
summary(mod5)

mod6 <- lm(CRF_nls~LOI+LOS)
summary(mod6)

influenceIndexPlot(mod1,id.n=3)
influencePlot(mod1,id.n=3)
influencePlot(mod2,id.n=3)
influencePlot(mod3,id.n=3)
influencePlot(mod4,id.n=3)
influencePlot(mod5,id.n=3)
influencePlot(mod6,id.n=3)

#############
#Run without #202
#run model
mod1a <- lm(NLS~LOI+LOS, subset=-c(202))
summary(mod1a)
vif(mod1a)

mod2a <- lm(ElectricActivity~LOI+LOS, subset=-c(202))
summary(mod2a)

mod3a <- lm(ltgAll~LOI+LOS, subset=-c(202))
summary(mod3a)

mod4a <- lm(ltg3plus~LOI+LOS, subset=-c(202))
summary(mod4a)

mod5a <- lm(ltgDur~LOI+LOS, subset=-c(202))
summary(mod5a)

mod6a <- lm(CRF_nls~LOI+LOS, subset=-c(202))
summary(mod6a)

influenceIndexPlot(mod1,id.n=3)
influencePlot(mod1a,id.n=3)
influencePlot(mod2a,id.n=3)
influencePlot(mod3a,id.n=3)
influencePlot(mod4a,id.n=3)
influencePlot(mod5a,id.n=3)
influencePlot(mod6a,id.n=3)

#################
#take log of the MaxVASR variable
LMV <- log(MaxVASR)
#take all of the observations that are inf or nan and make na
LMV[is.infinite(LMV) | is.nan(LMV) ] <- NA
summary(LMV)
#look only at events where NLS>0
NLS_g0 <- NLS[NLS>0]
LMV_g0nls <- LMV[NLS>0]
summary(NLS_g0)
#make a model
m1a <- lm(NLS_g0~LMV_g0nls)
#plot the model with regression line
plot(LMV_g0nls,NLS_g0)
abline(m1a)
#check summary of the model
summary(m1a)
#LVAB <- log(VASR_AB+0.01)
#LVAB[is.infinite(LVAB) | is.nan(LVAB) ] <- NA
#m1b <- lm(NLS~LVAB)
#summary(m1b)

###################

model_used <- m1a

residualPlots(model_used)
ncvTest(model_used)
spreadLevelPlot(model_used)
inverseResponsePlot(model_used)
crPlots(model_used)

