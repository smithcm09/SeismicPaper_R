attach(USE_THIS_ElectricDF)
view(dfSummary(USE_THIS_ElectricDF))
#summary statistics ----
summary(USE_THIS_ElectricDF)

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

#transform variables ----
L_VASR <- log10(VASR_AB)
L_Infra <- log10(OverallABmedInfra)
L_Seis <- log10(OverallABmedSeis)
Sq_Sdur <- sqrt(maxseisDur)
Sq_Idur <- sqrt(maxinfraDur)

#inital models ----
m.log_CRFyn <- glm(CRF_yn2~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
m.log_LTGyn <- glm(Factor_ltgAll~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
m.lin_EA <- lm(start_ElectricActivity~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)
m.lin_NLS <- lm(NLS~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)

#summary stats----
summary(m.log_CRFyn)
summary(m.log_LTGyn)
summary(m.lin_EA)
summary(m.lin_NLS)

#Anova stats----
Anova(m.log_CRFyn)
Anova(m.log_LTGyn)
Anova(m.lin_EA)
Anova(m.lin_NLS)

#VIFS----
vif(m.log_CRFyn)
vif(m.log_LTGyn)
vif(m.lin_EA)
vif(m.lin_NLS)


#Exhaustive Model Selection Linear Only----
Reg_EA <- regsubsets(start_ElectricActivity~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, data=USE_THIS_ElectricDF, method=c("exhaustive", "backward", "forward", "seqrep"))
Reg_NLS <- regsubsets(NLS~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, data=USE_THIS_ElectricDF, method=c("exhaustive", "backward", "forward", "seqrep"))
summary(Reg_EA)
summary(Reg_NLS)

#https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
plot(Reg_EA, scale = "adjr2", main = "Adjusted R^2")
plot(Reg_NLS, scale = "adjr2", main = "Adjusted R^2")

#Want High Adj. R2 and Low Mallow Cp (analog to AIC)
#EA
layout(matrix(1:2, ncol = 2))
subsets(Reg_EA, statistic="adjr2", legend = FALSE, main = "Adjusted R^2")
subsets(Reg_EA, statistic="cp", legend = FALSE, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)

#NLS
layout(matrix(1:2, ncol = 2))
subsets(Reg_NLS, statistic="adjr2", legend = FALSE, main = "Adjusted R^2")
subsets(Reg_NLS, statistic="cp", legend = FALSE, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)

#reset layout to normal----
layout(matrix(1:1, ncol = 1))

#Stepwise model selection - both directions - for GLM----
#http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
CRFnothing <- glm(CRF_yn2~1, family = binomial(link=logit))
CRFfullmod <- glm(CRF_yn2~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
CRF_bothways = step(CRFnothing, list(lower=formula(CRFnothing),upper=formula(CRFfullmod)), direction="both")

LTGnothing <- glm(Factor_ltgAll~1, family = binomial(link=logit))
LTGfullmod <- glm(Factor_ltgAll~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
LTG_bothways = step(LTGnothing, list(lower=formula(LTGnothing),upper=formula(LTGfullmod)), direction="both")

#Stepwise model selection - both directions - for LM----
EAnothing <- lm(start_ElectricActivity~1)
EAfullmod <- lm(start_ElectricActivity~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)
EA_bothways = step(EAnothing, list(lower=formula(EAnothing),upper=formula(EAfullmod)), direction="both")

NLSnothing <- lm(NLS~1)
NLSfullmod <- lm(NLS~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)
NLS_bothways = step(NLSnothing, list(lower=formula(NLSnothing),upper=formula(NLSfullmod)), direction="both")

#Interactions ----
Anova(lm(ElectricActivity~L_VASR*L_Infra*L_Seis*Sq_Sdur*Sq_Idur))
Anova(lm(NLS~L_VASR*L_Infra*L_Seis*Sq_Sdur*Sq_Idur))
Anova(glm(CRF_yn2 ~ L_VASR*L_Infra*L_Seis*Sq_Sdur*Sq_Idur, family = binomial(link = logit)))
Anova(glm(Factor_ltgAll ~ L_VASR*L_Infra*L_Seis*Sq_Sdur*Sq_Idur, family = binomial(link = logit)))

test <- lm(ElectricActivity~L_VASR*L_Infra*L_Seis*Sq_Idur*))
summary(test)

#shorted models - using the both direction steps test results----
m.log_CRFyn_short <- glm(CRF_yn2~L_VASR+L_Seis, family = binomial(link=logit))
m.log_LTGyn_short <- glm(Factor_ltgAll~L_Infra+Sq_Idur, family = binomial(link=logit))
m.lin_EA_short <- lm(start_ElectricActivity~L_Seis+Sq_Idur)
m.lin_NLS_short <- lm(NLS~L_Infra+L_Seis+Sq_Sdur)

summary(m.log_CRFyn_short)
summary(m.log_LTGyn_short)
summary(m.lin_EA_short)
summary(m.lin_NLS_short)

Anova(m.log_CRFyn_short)
Anova(m.log_LTGyn_short)
Anova(m.lin_EA_short)
Anova(m.lin_NLS_short)

vif(m.log_CRFyn_short)
vif(m.log_LTGyn_short)
vif(m.lin_EA_short)
vif(m.lin_NLS_short)

#shorted models including interactions ----
m.log_CRFyn_short_interact <- glm(CRF_yn2~L_VASR*L_Seis, family = binomial(link=logit))
m.log_LTGyn_short_interact <- glm(Factor_ltgAll~L_Infra*Sq_Idur, family = binomial(link=logit))
m.lin_EA_short_interact <- lm(start_ElectricActivity~L_Seis*Sq_Idur)
m.lin_NLS_short_interact <- lm(NLS~L_Infra*L_Seis+Sq_Sdur)
#switch order of interaction term for later plotting ease
m.lin_NLS_short_interact2 <- lm(NLS~L_Seis*L_Infra+Sq_Sdur)

summary(m.log_CRFyn_short_interact)
summary(m.log_LTGyn_short_interact)
summary(m.lin_EA_short_interact)
summary(m.lin_NLS_short_interact2)

Anova(m.log_CRFyn_short_interact)
Anova(m.log_LTGyn_short_interact)
Anova(m.lin_EA_short_interact)
Anova(m.lin_NLS_short_interact)

vif(m.log_CRFyn_short_interact)
vif(m.log_LTGyn_short_interact)
vif(m.lin_EA_short_interact)
vif(m.lin_NLS_short_interact)

#plot effects models of choosen sets - shortened logistic, shortened and interactions linear----
plot(allEffects(m.log_CRFyn_short))
plot(allEffects(m.log_LTGyn_short))
plot(allEffects(m.lin_EA_short_interact))
plot(allEffects(m.lin_NLS_short_interact))

#plot just the interaction term at designated levels ----
#https://stackoverflow.com/questions/31526290/control-how-interaction-term-in-effect-plot-is-displayed
#EA
plot(effect("L_Seis:Sq_Idur",m.lin_EA_short_interact,xlevels=list(Sq_Idur=c(1,5,6.5,8,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,150,150))
#NLS
plot(effect("L_Infra:L_Seis",m.lin_NLS_short_interact,xlevels=list(L_Seis=c(5.5,11,12,13,20))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
plot(effect("L_Seis:L_Infra",m.lin_NLS_short_interact2,xlevels=list(L_Infra=c(8.5,12,13,14,21))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)
     
     

#Outlier Detection in Shortened/interaction models----

#Choosen Models at this point
#m.log_CRFyn_short
#m.log_LTGyn_short
#m.lin_EA_short_interact
#m.lin_NLS_short_interact

outlierTest(m.log_CRFyn_short)
outlierTest(m.log_LTGyn_short)
outlierTest(m.lin_EA_short_interact)
outlierTest(m.lin_NLS_short_interact)

influencePlot(m.log_CRFyn_short,id.n=3)
influencePlot(m.log_LTGyn_short,id.n=3)
influencePlot(m.lin_EA_short_interact,id.n=3)
influencePlot(m.lin_NLS_short_interact,id.n=3)

thetimes[189]
thetimes[590]
thetimes[4]
thetimes[87]
thetimes[141]
thetimes[71]
thetimes[30]
thetimes[188]
thetimes[62]
thetimes[11]
thetimes[9]
thetimes[153]
thetimes[121]

thetimes[8]
thetimes[149]
thetimes[80]
thetimes[594]

#Rebuild new models without outlier values that proved to be mismatch times ----

m.log_CRFyn_short_o <- update(m.log_CRFyn_short, subset=-c(590,87,141,188,9,8,594))
m.log_LTGyn_short_o <- update(m.log_LTGyn_short, subset=-c(590,87,141,188,9,8,594))
m.lin_EA_short_interact_o <- update(m.lin_EA_short_interact, subset=-c(590,87,141,188,9,8,594))
m.lin_NLS_short_interact_o <- update(m.lin_NLS_short_interact, subset=-c(590,87,141,188,9,8,594))
#switch order of interaction term for later plotting ease
m.lin_NLS_short_interact2_o <- update(m.lin_NLS_short_interact2, subset=-c(590,87,141,188,9,8,594))

summary(m.log_CRFyn_short_o)
summary(m.log_LTGyn_short_o) 
summary(m.lin_EA_short_interact_o)
summary(m.lin_NLS_short_interact_o)

Anova(m.log_CRFyn_short_o)
Anova(m.log_LTGyn_short_o) 
Anova(m.lin_EA_short_interact_o)
Anova(m.lin_NLS_short_interact_o)

vif(m.log_CRFyn_short_o)
vif(m.log_LTGyn_short_o) 
vif(m.lin_EA_short_interact_o)
vif(m.lin_NLS_short_interact_o)

#Plot new effects plots----
plot(allEffects(m.log_CRFyn_short_o))
plot(allEffects(m.log_LTGyn_short_o))
plot(allEffects(m.lin_EA_short_interact_o))
plot(allEffects(m.lin_NLS_short_interact_o))


#Just the interaction Term
#use summary stats to determine break points of plots - aim to span whole range and divide ~evenly
summary(L_Seis)
summary(L_Infra)
summary(Sq_Idur)
#EA
plot(effect("L_Seis:Sq_Idur",m.lin_EA_short_interact_o,xlevels=list(Sq_Idur=c(1.5,4,6.5,8.25,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,150,150))
#NLS
plot(effect("L_Infra:L_Seis",m.lin_NLS_short_interact_o,xlevels=list(L_Seis=c(2.5,4,5.5,7,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
plot(effect("L_Seis:L_Infra",m.lin_NLS_short_interact2_o,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)


# EA----
#Check underlying assumptions for possible Y-variable transforms (linear only)----
#m.lin_EA_short_interact_o
#m.lin_NLS_short_interact_o

#EA
symbox(start_ElectricActivity)
powerTransform(start_ElectricActivity)

#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m.lin_EA_short_interact_o)
spreadLevelPlot(m.lin_EA_short_interact_o)
ncvTest(m.lin_EA_short_interact_o) 

#normality
qqPlot(m.lin_EA_short_interact_o)
plot(density(rstudent(m.lin_EA_short_interact_o)))
curve(dt(x,97), col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m.lin_EA_short_interact_o) 
#ceresPlots(m.lin_EA_short_interact_o)
residualPlots(m.lin_EA_short_interact_o)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the vale of the quadratic predictor is 0

#what transform to fix any assumption violations
boxCox(m.lin_EA_short_interact_o) #normality -- sugg 0.2
summary(powerTransform(m.lin_EA_short_interact_o)) #normality -- sugg 0.2
inverseResponsePlot(m.lin_EA_short_interact_o) #geared toward linearity -- sugg. 0.5

#Try log and sqrt transforms of y data EA
#log_tran----
m.lin_EA_short_interact_o_logTy <- lm(log10(start_ElectricActivity)~L_Seis*Sq_Idur,subset=-c(590,87,141,188,9,8,594))

#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m.lin_EA_short_interact_o_logTy)
spreadLevelPlot(m.lin_EA_short_interact_o_logTy)
ncvTest(m.lin_EA_short_interact_o_logTy) 

#normality
qqPlot(m.lin_EA_short_interact_o_logTy)
plot(density(rstudent(m.lin_EA_short_interact_o_logTy)))
curve(dt(x,97), col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m.lin_EA_short_interact_o) 
#ceresPlots(m.lin_EA_short_interact_o)
residualPlots(m.lin_EA_short_interact_o_logTy)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the vale of the quadratic predictor is 0

Anova(m.lin_EA_short_interact_o)
Anova(m.lin_EA_short_interact_o_logTy)

plot(allEffects(m.lin_EA_short_interact_o_logTy))
plot(effect("L_Seis:Sq_Idur",m.lin_EA_short_interact_o_logTy,xlevels=list(Sq_Idur=c(1.5,4,6.5,8.25,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,150,150))

summary(m.lin_EA_short_interact_o_logTy)
vif(m.lin_EA_short_interact_o_logTy)

#sqrt_tran----
m.lin_EA_short_interact_o_sqrtTy <- lm(sqrt(start_ElectricActivity)~L_Seis*Sq_Idur,subset=-c(590,87,141,188,9,8,594))

#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m.lin_EA_short_interact_o_sqrtTy)
spreadLevelPlot(m.lin_EA_short_interact_o_sqrtTy)
ncvTest(m.lin_EA_short_interact_o_sqrtTy) 

#normality
qqPlot(m.lin_EA_short_interact_o_sqrtTy)
plot(density(rstudent(m.lin_EA_short_interact_o_sqrtTy)))
curve(dt(x,97), col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m.lin_EA_short_interact_o) 
#ceresPlots(m.lin_EA_short_interact_o)
residualPlots(m.lin_EA_short_interact_o_sqrtTy)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the value of the quadratic predictor is 0


Anova(m.lin_EA_short_interact_o)
Anova(m.lin_EA_short_interact_o_sqrtTy)

plot(allEffects(m.lin_EA_short_interact_o_sqrtTy))
plot(effect("L_Seis:Sq_Idur",m.lin_EA_short_interact_o_sqrtTy,xlevels=list(Sq_Idur=c(1.5,4,6.5,8.25,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,150,150))

summary(m.lin_EA_short_interact_o_sqrtTy)
summary(m.lin_EA_short_interact_o_logTy)

formula(m.lin_EA_short_interact_o_sqrtTy)
vif(m.lin_EA_short_interact_o_sqrtTy)

#NLS----
#m.lin_NLS_short_interact <- lm(NLS~L_Infra*L_Seis+Sq_Sdur)

symbox(NLS)
powerTransform(NLS)

#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m.lin_NLS_short_interact_o)
spreadLevelPlot(m.lin_NLS_short_interact_o)
ncvTest(m.lin_NLS_short_interact_o) # if sig then means that it IS heteroscedastic

#normality
qqPlot(m.lin_NLS_short_interact_o)
plot(density(rstudent(m.lin_NLS_short_interact_o)),xlim=c(-3,3))
curve(dt(x,97), -3,3, col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m.lin_NLS_short_interact_o) 
#ceresPlots(m.lin_NLS_short_interact_o)
residualPlots(m.lin_NLS_short_interact_o)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the vale of the quadratic predictor is 0

#what transform to fix any assumption violations
boxCox(m.lin_NLS_short_interact_o) #normality -- sugg 0.0
summary(powerTransform(m.lin_NLS_short_interact_o)) #normality -- sugg 0.0
inverseResponsePlot(m.lin_NLS_short_interact_o) #geared toward linearity -- sugg. 0.5



#Try log and sqrt transforms of y data NLS
#log_tran----
m.lin_NLS_short_interact_o_logTy <- lm(log10(NLS)~L_Seis*L_Infra+Sq_Sdur,subset=-c(590,87,141,188,9,8,594))
#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m.lin_NLS_short_interact_o_logTy)
spreadLevelPlot(m.lin_NLS_short_interact_o_logTy)
ncvTest(m.lin_NLS_short_interact_o_logTy) 

#normality
qqPlot(m.lin_NLS_short_interact_o_logTy)
plot(density(rstudent(m.lin_NLS_short_interact_o_logTy)), xlim=c(-3,3))
curve(dt(x,97), -3,3, col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m.lin_NLS_short_interact_o) 
#ceresPlots(m.lin_NLS_short_interact_o)
residualPlots(m.lin_NLS_short_interact_o_logTy)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the value of the quadratic predictor is 0

Anova(m.lin_NLS_short_interact_o)
Anova(m.lin_NLS_short_interact_o_logTy)

plot(allEffects(m.lin_NLS_short_interact_o_logTy))
plot(effect("L_Seis:L_Infra",m.lin_NLS_short_interact_o_logTy,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)

summary(m.lin_NLS_short_interact_o_logTy)
vif(m.lin_NLS_short_interact_o_logTy)

#sqrt tran----
m.lin_NLS_short_interact_o_sqrtTy <- lm(sqrt(NLS)~L_Seis*L_Infra+Sq_Sdur,subset=-c(590,87,141,188,9,8,594))
#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m.lin_NLS_short_interact_o_sqrtTy)
spreadLevelPlot(m.lin_NLS_short_interact_o_sqrtTy)
ncvTest(m.lin_NLS_short_interact_o_sqrtTy) 

#normality
qqPlot(m.lin_NLS_short_interact_o_sqrtTy)
plot(density(rstudent(m.lin_NLS_short_interact_o_sqrtTy)), xlim=c(-3,3))
curve(dt(x,97), -3,3, col= 3, add=T)

#non-linNLSrity
#cr and ceres plots do not work for models with interactions
#crPlots(m.lin_NLS_short_interact_o) 
#ceresPlots(m.lin_NLS_short_interact_o)
residualPlots(m.lin_NLS_short_interact_o_sqrtTy)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the vale of the quadratic predictor is 0

Anova(m.lin_NLS_short_interact_o)
Anova(m.lin_NLS_short_interact_o_sqrtTy)

plot(allEffects(m.lin_NLS_short_interact_o_sqrtTy))
plot(effect("L_Seis:L_Infra",m.lin_NLS_short_interact_o_sqrtTy,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)

summary(m.lin_NLS_short_interact_o_sqrtTy)
summary(m.lin_NLS_short_interact_o_logTy)
vif(m.lin_NLS_short_interact_o_sqrtTy)

formula(m.lin_NLS_short_interact_o_sqrtTy)




## looking for need of polynomial terms in chosen models ----
residualPlots(m.log_LTGyn_short)
residualPlots(m.log_CRFyn_short)
residualPlots(m.lin_NLS_short_interact_o_sqrtTy)
residualPlots(m.lin_EA_short_interact_o_sqrtTy)

#look at poly of L_seis and L_Infra in NLS
m.lin_NLS_short_interact_o_sqrtTy <- lm(sqrt(NLS)~L_Seis*L_Infra+Sq_Sdur,subset=-c(590,87,141,188,9,8,594))
m.lin_NLS_short_interact_o_sqrtTy_polyS <- lm(sqrt(NLS)~poly(L_Seis,2)*L_Infra+Sq_Sdur,subset=-c(590,87,141,188,9,8,594))
m.lin_NLS_short_interact_o_sqrtTy_polyI <- lm(sqrt(NLS)~L_Seis*poly(L_Infra,2)+Sq_Sdur,subset=-c(590,87,141,188,9,8,594))
summary(m.lin_NLS_short_interact_o_sqrtTy_polyS)
summary(m.lin_NLS_short_interact_o_sqrtTy_polyI)
Anova(m.lin_NLS_short_interact_o_sqrtTy_polyS)
Anova(m.lin_NLS_short_interact_o_sqrtTy_polyI)
residualPlots(m.lin_NLS_short_interact_o_sqrtTy_polyS)
residualPlots(m.lin_NLS_short_interact_o_sqrtTy_polyI)

plot(allEffects(m.lin_NLS_short_interact_o_sqrtTy_polyS))
plot(effect("L_Seis:L_Infra",m.lin_NLS_short_interact_o_sqrtTy_polyS,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)


#look at poly of Sq_Idur in EA
m.lin_EA_short_interact_o_sqrtTy <- lm(sqrt(start_ElectricActivity)~L_Seis*Sq_Idur,subset=-c(590,87,141,188,9,8,594))
m.lin_EA_short_interact_o_sqrtTy_polyID <- lm(sqrt(start_ElectricActivity)~L_Seis*poly(Sq_Idur,2),subset=-c(590,87,141,188,9,8,594))
summary(m.lin_EA_short_interact_o_sqrtTy_polyID)
Anova(m.lin_EA_short_interact_o_sqrtTy_polyID)
residualPlots(m.lin_EA_short_interact_o_sqrtTy_polyID)
plot(allEffects(m.lin_EA_short_interact_o_sqrtTy_polyID))


scatterplot(L_Seis,sqrt(NLS))

#############
#sim_slopes(m.lin_EA_short_interact_o_sqrtTy, pred = L_Seis, modx = Sq_Idur, johnson_neyman = TRUE)
johnson_neyman(m.lin_EA_short_interact_o_sqrtTy, pred = L_Seis, modx = Sq_Idur, alpha = 0.01)
johnson_neyman(m.lin_EA_short_interact_o_sqrtTy, pred = Sq_Idur, modx = L_Seis, alpha = 0.01)

johnson_neyman(m.lin_NLS_short_interact_o_sqrtTy, pred = L_Seis, modx = L_Infra, alpha = 0.05)
johnson_neyman(m.lin_NLS_short_interact_o_sqrtTy, pred = L_Infra, modx = L_Seis, alpha = 0.05)




###############################
plot(allEffects(m.log_CRFyn_short_o))
plot(allEffects(m.log_LTGyn_short_o))
plot(allEffects(m.lin_NLS_short_interact_o_sqrtTy))
plot(effect("L_Seis:L_Infra",m.lin_NLS_short_interact_o_sqrtTy,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)
plot(allEffects(m.lin_EA_short_interact_o_sqrtTy))
plot(effect("L_Seis:Sq_Idur",m.lin_EA_short_interact_o_sqrtTy,xlevels=list(Sq_Idur=c(1.5,4,6.5,8.25,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,150,150))

