#Linear Modeling
# (1) Data Exploration and (2) Logistic Modeling for the seismic and infrasound data w.r.t. Electrical Y/N properties
# (2) Data Exploration

attach(ElectricOnlyDF_withNLS1)

#summary and exploration ----
#as.factor(CRF_yn)
ElectricOnlyDF$CRF_yn2 <- factor(ElectricOnlyDF$CRF_yn)
summary(ElectricOnlyDF$CRF_yn2)
summary(ElectricActivity)
summary(NLS)
summary(ltgAll)

hist(NLS, breaks=50)

summary(VASR_AB)
summary(OverallABmedInfra)
summary(OverallABmedSeis)
summary(maxseisDur)
summary(maxinfraDur)

plot(VASR_AB)
plot(OverallABmedInfra)
plot(OverallABmedSeis)
plot(maxseisDur)
plot(maxinfraDur)

hist(VASR_AB, breaks = 50)
hist(OverallABmedInfra, breaks = 50)
hist(OverallABmedSeis, breaks = 50)
hist(maxseisDur, breaks = 50)
hist(maxinfraDur, breaks = 50)

Boxplot(VASR_AB)
Boxplot(OverallABmedInfra)
Boxplot(OverallABmedSeis)
Boxplot(maxseisDur)
Boxplot(maxinfraDur)

#symbox and powerTransform stats ----

symbox(VASR_AB)
symbox(OverallABmedInfra)
symbox(OverallABmedSeis)
symbox(maxseisDur)
symbox(maxinfraDur)

powerTransform(VASR_AB)
powerTransform(OverallABmedInfra)
powerTransform(OverallABmedSeis)
powerTransform(maxseisDur)
powerTransform(maxinfraDur)

#Factored Boxplots ----
Boxplot(log(VASR_AB)~CRF_yn2, notch=TRUE, id.n=0)
Boxplot(log(OverallABmedInfra)~CRF_yn2, notch=TRUE, id.n=0)
Boxplot(log(OverallABmedSeis)~CRF_yn2, notch=TRUE, id.n=0)
Boxplot(sqrt(maxseisDur)~CRF_yn2, notch=TRUE, id.n=0)
Boxplot(sqrt(maxinfraDur)~CRF_yn2, notch=TRUE, id.n=0)

#transform variables ----
L_VASR <- log(VASR_AB)
L_Infra <- log(OverallABmedInfra)
L_Seis <- log(OverallABmedSeis)
Sq_Sdur <- sqrt(maxseisDur)
Sq_Idur <- sqrt(maxinfraDur)

#initial models - 1 logistic and 3 linear----

m.log_yn <- glm(CRF_yn2~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
m.lin_EA <- lm(ElectricActivity~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)
m.lin_NLS <- lm(NLS~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)
m.lin_ltgAll <- lm(ltgAll~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)

#summary stats----
summary(m.log_yn)
summary(m.lin_EA)
summary(m.lin_NLS)
summary(m.lin_ltgAll)

#Anova stats----
Anova(m.log_yn)
Anova(m.lin_EA)
Anova(m.lin_NLS)
Anova(m.lin_ltgAll)

#Confidence Intervals----
round(exp(cbind(Estimate=coef(m.log_yn), confint(m.log_yn))),2)
round(exp(cbind(Estimate=coef(m.lin_EA), confint(m.lin_EA))),2)
round(exp(cbind(Estimate=coef(m.lin_NLS), confint(m.lin_NLS))),2)
round(exp(cbind(Estimate=coef(m.lin_ltgAll), confint(m.lin_ltgAll))),2)


#Effectsplot----
plot(allEffects(m.log_yn))
plot(allEffects(m.lin_EA))
plot(allEffects(m.lin_NLS))
plot(allEffects(m.lin_ltgAll))

#stepwise regression to look at potentially removing a variable to lower AIC ----
m.log_yn_back <- step(m.log_yn, direction = 'backward')
m.lin_EA_back <- step(m.lin_EA, direction = 'backward')
m.lin_NLS_back <- step(m.lin_NLS, direction = 'backward')
m.lin_ltgAll_back <- step(m.lin_ltgAll, direction = 'backward')

m.log_yn_back <- step(m.log_yn, direction = 'forward')
m.lin_EA_back <- step(m.lin_EA, direction = 'forward')
m.lin_NLS_back <- step(m.lin_NLS, direction = 'forward')
m.lin_ltgAll_back <- step(m.lin_ltgAll, direction = 'forward')

#check new vs old models----
#correlation of residuals
#CRF_yn
plot(fitted(m.log_yn)~fitted(m.log_yn_back))
abline(0,1, col = 'red')
cor(fitted(m.log_yn),fitted(m.log_yn_back))
#correlated at 0.9925538 so _____________

#ElectricActivity
plot(fitted(m.lin_EA)~fitted(m.lin_EA_back))
abline(0,1, col = 'red')
cor(fitted(m.lin_EA),fitted(m.lin_EA_back))
#correlated at 0.9222149 so _____________

#NLS
plot(fitted(m.lin_NLS)~fitted(m.lin_NLS_back))
abline(0,1, col = 'red')
cor(fitted(m.lin_NLS),fitted(m.lin_NLS_back))
#correlated at 0.9999303 so _____________

#ltgAll
plot(fitted(m.lin_ltgAll)~fitted(m.lin_ltgAll_back))
abline(0,1, col = 'red')
cor(fitted(m.lin_ltgAll),fitted(m.lin_ltgAll_back))
#correlated at 0.9969834 so _____________

#summary values
summary(m.log_yn_back)
summary(m.lin_EA_back)
summary(m.lin_NLS_back)
summary(m.lin_ltgAll_back)

#Anovas
Anova(m.log_yn_back)
Anova(m.lin_EA_back)
Anova(m.lin_NLS_back)
Anova(m.lin_ltgAll_back)

#Effect Plots
plot(allEffects(m.log_yn_back))
plot(allEffects(m.lin_EA_back))
plot(allEffects(m.lin_NLS_back))
plot(allEffects(m.lin_ltgAll_back))

#outlier tests ----
#m.log_yn
#full models outlier tests
outlierTest(m.log_yn)
dfbetasPlots(m.log_yn)
influenceIndexPlot(m.log_yn, id.n=3)
influencePlot(m.log_yn, id.n=3) #one I like
#reduced models outlier tests
outlierTest(m.log_yn_back)
dfbetasPlots(m.log_yn_back)
influenceIndexPlot(m.log_yn_back, id.n=3)
influencePlot(m.log_yn_back, id.n=3) #one I like

#m.lin_EA
#full models outlier tests
outlierTest(m.lin_EA)
dfbetasPlots(m.lin_EA)
influenceIndexPlot(m.lin_EA, id.n=3)
influencePlot(m.lin_EA, id.n=3) #one I like
#reduced models outlier tests
outlierTest(m.lin_EA_back )
dfbetasPlots(m.lin_EA_back )
influenceIndexPlot(m.lin_EA_back , id.n=3)
influencePlot(m.lin_EA_back , id.n=3) #one I like

#m.lin_NLS
#full models outlier tests
outlierTest(m.lin_NLS)
dfbetasPlots(m.lin_NLS)
influenceIndexPlot(m.lin_NLS, id.n=3)
influencePlot(m.lin_NLS, id.n=3) #one I like
#reduced models outlier tests
outlierTest(m.lin_NLS_back )
dfbetasPlots(m.lin_NLS_back )
influenceIndexPlot(m.lin_NLS_back , id.n=3)
influencePlot(m.lin_NLS_back , id.n=3) #one I like

#m.lin_ltgAll
#full models outlier tests
outlierTest(m.lin_ltgAll)
dfbetasPlots(m.lin_ltgAll)
influenceIndexPlot(m.lin_ltgAll, id.n=3)
influencePlot(m.lin_ltgAll, id.n=3) #one I like
#reduced models outlier tests
outlierTest(m.lin_ltgAll_back )
dfbetasPlots(m.lin_ltgAll_back )
influenceIndexPlot(m.lin_ltgAll_back , id.n=3)
influencePlot(m.lin_ltgAll_back , id.n=3) #one I like

#residualPlots----
residualPlots(m.log_yn)
residualPlots(m.lin_EA)
residualPlots(m.lin_NLS)
residualPlots(m.lin_ltgAll)

residualPlots(m.log_yn_back)
residualPlots(m.lin_EA_back)
residualPlots(m.lin_NLS_back)
residualPlots(m.lin_ltgAll_back)

#VIFS----
vif(m.log_yn)
vif(m.lin_EA)
vif(m.lin_NLS)
vif(m.lin_ltgAll)

vif(m.log_yn_back)
vif(m.lin_EA_back)
vif(m.lin_NLS_back)
vif(m.lin_ltgAll_back)

#pick out outlier event times----
thetimes[8]
thetimes[153]
thetimes[149]
thetimes[190]
thetimes[141]
thetimes[87]
thetimes[4]
thetimes[62]
thetimes[11]
thetimes[9]
thetimes[71]
thetimes[310]
thetimes[133]
thetimes[173]

#build new models without the outliers to see how it goes----
m.log_yn_outliers <- update(m.log_yn, subset=-c(8))
m.lin_EA_outliers <- update(m.lin_EA, subset=-c(133,87,173))
m.lin_ltgAll_outliers <- update(m.lin_ltgAll, subset=-c(9))

summary(m.log_yn_outliers)
summary(m.lin_EA_outliers)
summary(m.lin_ltgAll_outliers)

Anova(m.log_yn_outliers)
Anova(m.lin_EA_outliers)
Anova(m.lin_ltgAll_outliers)

plot(allEffects(m.log_yn_outliers))
plot(allEffects(m.lin_EA_outliers))
plot(allEffects(m.lin_ltgAll_outliers))

vif(m.log_yn_outliers)
vif(m.lin_EA_outliers)
vif(m.lin_ltgAll_outliers)

#backwards model - outliers ----
m.log_yn_back_outliers <- update(m.log_yn_back, subset=-c(8))
m.lin_EA_back_outliers <- update(m.lin_EA_back, subset=-c(133,173,87))
m.lin_ltgAll_back_outliers <- update(m.lin_ltgAll_back, subset=-c(9))

summary(m.log_yn_back_outliers)
summary(m.lin_EA_back_outliers)
summary(m.lin_ltgAll_back_outliers)

Anova(m.log_yn_back_outliers)
Anova(m.lin_EA_back_outliers)
Anova(m.lin_ltgAll_back_outliers)

plot(allEffects(m.log_yn_back_outliers))
plot(allEffects(m.lin_EA_back_outliers))
plot(allEffects(m.lin_ltgAll_back_outliers))

#Initial - outliers y-adjustments ----
vif(m.log_yn_outliers)
vif(m.lin_EA_outliers)
vif(m.lin_ltgAll_outliers)
vif(m.lin_NLS)

avPlots(m.log_yn_outliers)
avPlots(m.lin_EA_outliers)
avPlots(m.lin_ltgAll_outliers)
avPlots(m.lin_NLS)

residualPlots(m.lin_EA_outliers)
residualPlots(m.lin_ltgAll_outliers)
residualPlots(m.lin_NLS)
# all sig so current model is a poor fit

qqPlot(m.lin_EA_outliers)
qqPlot(m.lin_ltgAll_outliers)
qqPlot(m.lin_NLS)

spreadLevelPlot(m.log_yn_outliers)
spreadLevelPlot(m.lin_EA_outliers)
spreadLevelPlot(m.lin_ltgAll_outliers)
spreadLevelPlot(m.lin_NLS)

crPlots(m.lin_EA_outliers)
crPlots(m.lin_ltgAll_outliers)
crPlots(m.lin_NLS)

#problem with boxcox and powerTransform and iRP for ltgAll - saying need 1st argument strictly positive
# fixed for rest by removing 0 lines and 1 lines (for NLS) and restarting whole operation
boxCox(m.lin_EA_outliers)
boxCox(m.lin_ltgAll_outliers)
boxCox(m.lin_NLS)

summary(tm.lin_EA_out <- powerTransform(m.lin_EA_outliers))
summary(tm.lin_ltgAll_out <- powerTransform(m.lin_ltgAll_outliers))
summary(tm.lin_NLS <- powerTransform(m.lin_NLS))

inverseResponsePlot(m.lin_EA_outliers)
inverseResponsePlot(m.lin_ltgAll_outliers)
inverseResponsePlot(m.lin_NLS)

#new y transformed models----

m.lin_EA_out_trans <- lm(sqrt(ElectricActivity)~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, subset=-c(133,173,87))
m.lin_NLS_trans <- lm(sqrt(NLS)~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)

summary(m.lin_EA_out_trans)
summary(m.lin_NLS_trans)

Anova(m.lin_EA_out_trans)
Anova(m.lin_NLS_trans)

vif(m.lin_EA_out_trans)
vif(m.lin_NLS_trans)

plot(allEffects(m.lin_EA_out_trans))
plot(allEffects(m.lin_NLS_trans))

qqPlot(m.lin_EA_out_trans)
qqPlot(m.lin_NLS_trans)

residualPlots(m.lin_EA_out_trans)
residualPlots(m.lin_NLS_trans)

##### -----

m.testing_interactions <- lm(NLS~L_Seis*L_Infra+Sq_Sdur)
summary(m.testing_interactions)
plot(allEffects(m.testing_interactions), row = 2, cols = 1)
coplot(log(NLS+1)~L_Seis|L_Infra, panel=panel.car, row=1, col='red', pch=16)
