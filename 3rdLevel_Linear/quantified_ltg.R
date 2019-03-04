#3rd level linear ltg 
attach(DF_onlyLtgRows)
#view(dfSummary(DF_onlyLtgRows))
summary(DF_onlyLtgRows)

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


#build initial model

m3.lin_ltg <- lm(ltgAll~L_Infra+L_Seis+L_VASR+Sq_Idur+Sq_Sdur)
summary(m3.lin_ltg)
Anova(m3.lin_ltg)
vif(m3.lin_ltg)
Reg_LTG <- regsubsets(ltgAll~L_Infra+L_Seis+L_VASR+Sq_Idur+Sq_Sdur, data=DF_onlyLtgRows, method=c("exhaustive", "backward", "forward", "seqrep"))
summary(Reg_LTG)
plot(Reg_LTG, scale = "adjr2", main = "Adjusted R^2")
# best for R2 is L_Infra, L_Sieis

layout(matrix(1:2, ncol = 2))
subsets(Reg_LTG, statistic="adjr2", legend = FALSE, main = "Adjusted R^2")
subsets(Reg_LTG, statistic="cp", legend = FALSE, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)
layout(matrix(1:1, ncol = 1))
# best for R2 is L_Infra, L_Sieis

LTGnothing <- lm(ltgAll~1)
LTGfullmod <- lm(ltgAll~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur)
LTG_bothways = step(LTGnothing, list(lower=formula(LTGnothing),upper=formula(LTGfullmod)), direction="both")
#best model result from stepwise both directions
#Step:  AIC=1703.96
#ltgAll ~ L_Infra + L_Seis

m3.lin_ltg_short <- lm(ltgAll~L_Infra+L_Seis)
m3.lin_ltg_short_interact <- lm(ltgAll~L_Infra*L_Seis)
m3.lin_ltg_short_interact2 <- lm(ltgAll~L_Seis*L_Infra)

summary(m3.lin_ltg_short)
summary(m3.lin_ltg_short_interact)

Anova(m3.lin_ltg_short)
Anova(m3.lin_ltg_short_interact)

plot(allEffects(m3.lin_ltg_short_interact))

outlierTest(m3.lin_ltg_short_interact)
influencePlot(m3.lin_ltg_short_interact)

#next step for wednesday
#check outlier times
thetimes[69]
thetimes[225]
thetimes[143]
thetimes[4]
thetimes[7]
thetimes[407]
thetimes[78]
thetimes[302]
thetimes[125]
#some are fuzzy events but no 'bad picks' so none removed

#pick break points for efect plots
summary(L_Seis)
summary(L_Infra)
plot(effect("L_Infra:L_Seis",m3.lin_ltg_short_interact,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
plot(effect("L_Seis:L_Infra",m3.lin_ltg_short_interact2,xlevels=list(L_Infra=c(4,5.5,5.9,6.2,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)

## looking for y transforms
symbox(ltgAll)
powerTransform(ltgAll)
#sugg -0.5
boxCox(m3.lin_ltg_short_interact) sugg -1/3

#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m3.lin_ltg_short_interact)
spreadLevelPlot(m3.lin_ltg_short_interact) #sugg -0.12
ncvTest(m3.lin_ltg_short_interact) 

#normality
qqPlot(m3.lin_ltg_short_interact)
plot(density(rstudent(m3.lin_ltg_short_interact)))
curve(dt(x,97), col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m3.lin_ltg_short_interact) 
#ceresPlots(m3.lin_ltg_short_interact)
residualPlots(m3.lin_ltg_short_interact)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the vale of the quadratic predictor is 0

#what transform to fix any assumption violations
boxCox(m3.lin_ltg_short_interact) #normality -- sugg -0.33
summary(powerTransform(m3.lin_ltg_short_interact)) #normality -- sugg -0.33
inverseResponsePlot(m3.lin_ltg_short_interact) #geared toward linearity -- sugg. 0.5


# try -sqrt transform on y variable
m3.lin_ltg_short_interact_neg13T <- lm((ltgAll^(-1/3))~L_Infra*L_Seis)
summary(m3.lin_ltg_short_interact_neg13T)
Anova(m3.lin_ltg_short_interact_neg13T)
#Heteroscedasticity aka nonconstant error varriance
#only look at pattern of residuals, is there trumpet shape?
residualPlots(m3.lin_ltg_short_interact_neg13T)
spreadLevelPlot(m3.lin_ltg_short_interact_neg13T)
ncvTest(m3.lin_ltg_short_interact_neg13T) 

#normality
qqPlot(m3.lin_ltg_short_interact_neg13T)
plot(density(rstudent(m3.lin_ltg_short_interact_neg13T)))
curve(dt(x,97), col= 3, add=T)

#non-linearity
#cr and ceres plots do not work for models with interactions
#crPlots(m3.lin_ltg_short_interact_negsqrtT) 
#ceresPlots(m3.lin_ltg_short_interact_negsqrtT)
residualPlots(m3.lin_ltg_short_interact_neg13T)
#look at residual plot output this time - 
#if tukey test is sig (<0.05) it indicates that the current model is a poor fit
#the t-test on the variables is for the null hypo that the vale of the quadratic predictor is 0
boxTidwell(ltgAll~L_Infra*L_Seis)
boxTidwell(m3.lin_ltg_short_interact_neg13T)







####----
m3.lin_ltg_short_interact_neg13T_polyI <- lm((ltgAll^(-1/3))~poly(L_Infra,2)*L_Seis)
summary(m3.lin_ltg_short_interact_neg13T_polyI)
Anova(m3.lin_ltg_short_interact_neg13T_polyI)
residualPlots(m3.lin_ltg_short_interact_neg13T_polyI)
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyI))
plot(effect("poly(L_Infra,2):L_Seis",m3.lin_ltg_short_interact_neg13T_polyI,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyI", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
crPlots(m3.lin_ltg_short_interact_neg13T_polyI)
formula(m3.lin_ltg_short_interact_neg13T_polyI)

#m3.lin_ltg_short_interact_neg13T_polyIS <- lm((ltgAll^(-1/3))~poly(L_Infra,2)*poly(L_Seis,2))
##summary(m3.lin_ltg_short_interact_neg13T_polyIS)
#Anova(m3.lin_ltg_short_interact_neg13T_polyIS)
#residualPlots(m3.lin_ltg_short_interact_neg13T_polyIS)
#plot(effect("poly(L_Infra,2):poly(L_Seis,2)",m3.lin_ltg_short_interact_neg13T_polyIS,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
#crPlots(m3.lin_ltg_short_interact_neg13T_polyIS)
#formula(m3.lin_ltg_short_interact_neg13T_polyIS)

#best polynomial option
m3.lin_ltg_short_interact_neg13T_polyS <- lm((ltgAll^(-1/3))~L_Infra*poly(L_Seis,2))
summary(m3.lin_ltg_short_interact_neg13T_polyS)
Anova(m3.lin_ltg_short_interact_neg13T_polyS)
residualPlots(m3.lin_ltg_short_interact_neg13T_polyS)
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyS))
plot(effect("L_Infra:poly(L_Seis,2)",m3.lin_ltg_short_interact_neg13T_polyS,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyS", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
crPlots(m3.lin_ltg_short_interact_neg13T_polyS)
formula(m3.lin_ltg_short_interact_neg13T_polyS)
confint(m3.lin_ltg_short_interact_neg13T_polyS, level=0.95)


##########
m3.lin_ltg_short_interact_neg13T_polyI_raw <- lm((ltgAll^(-1/3))~poly(L_Infra,2)*L_Seis)
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyI_raw))
plot(effect("poly(L_Infra,2):L_Seis",m3.lin_ltg_short_interact_neg13T_polyI,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyI", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
summary(m3.lin_ltg_short_interact_neg13T_polyI)
Anova(m3.lin_ltg_short_interact_neg13T_polyI)

m3.lin_ltg_short_interact_neg13T_polyI_reg <- lm((ltgAll^(-1/3))~poly(L_Infra,2)*L_Seis)
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyI_reg))
plot(effect("poly(L_Infra,2):L_Seis",m3.lin_ltg_short_interact_neg13T_polyI_reg,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyI", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
summary(m3.lin_ltg_short_interact_neg13T_polyI)
Anova(m3.lin_ltg_short_interact_neg13T_polyI)

m3.lin_ltg_short_interact_neg13T_polyS <- lm((ltgAll^(-1/3))~L_Infra*poly(L_Seis,2))
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyS))
plot(effect("L_Infra:poly(L_Seis,2)",m3.lin_ltg_short_interact_neg13T_polyS,xlevels=list(L_Seis=c(4.1,5,5.5,6,8.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyS", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
summary(m3.lin_ltg_short_interact_neg13T_polyS)
Anova(m3.lin_ltg_short_interact_neg13T_polyS)

##
m3.lin_ltg_short_interact_neg13T_polyI2_raw <- lm((ltgAll^(-1/3))~L_Seis*poly(L_Infra,2, raw=TRUE))
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyI2_raw))
plot(effect("L_Seis:poly(L_Infra,2)",m3.lin_ltg_short_interact_neg13T_polyI2_raw), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyI2", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
summary(m3.lin_ltg_short_interact_neg13T_polyI2_raw)
Anova(m3.lin_ltg_short_interact_neg13T_polyI2_raw)

m3.lin_ltg_short_interact_neg13T_polyI2_reg <- lm((ltgAll^(-1/3))~L_Seis*poly(L_Infra,2))
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyI2_reg))
plot(effect("L_Seis:poly(L_Infra,2)",m3.lin_ltg_short_interact_neg13T_polyI2), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyI2", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
summary(m3.lin_ltg_short_interact_neg13T_polyI2_reg)
Anova(m3.lin_ltg_short_interact_neg13T_polyI2)

vif(m3.lin_ltg_short_interact_neg13T_polyI2_raw)
vif(m3.lin_ltg_short_interact_neg13T_polyI2_reg)

m3.lin_ltg_short_interact_neg13T_polyS2 <- lm((ltgAll^(-1/3))~poly(L_Seis,2)*L_Infra)
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyS2))
plot(effect("poly(L_Seis,2):L_Infra",m3.lin_ltg_short_interact_neg13T_polyS2), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on LTG_polyS2", rotx=45, more=FALSE, grid=FALSE, lwd=1) #, ylim=seq(0,4500,4500))
summary(m3.lin_ltg_short_interact_neg13T_polyS2)
Anova(m3.lin_ltg_short_interact_neg13T_polyS2)



####################
johnson_neyman(m3.lin_ltg_short_interact_neg13T_polyI, pred = poly(L_Infra,2), modx = L_Seis, alpha = 0.05)










plot(effect("poly(L_Infra,2):L_Seis",m3.lin_ltg_short_interact_neg13T_polyI_reg,
            xlevels=list(L_Seis=c(4.1,5,5.5,6,7.5))), layout=c(5,1), alternating=FALSE, 
     main="Interaction of L_Infra and L_Seis on LTG_polyI", 
     rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,1), trans=list(link= NULL, inverse = function(x) x^(1/3)))

#attach(S3_DataFrame)
attach(DF_onlyLtgRows)
m3.lin_ltg_short_interact_neg13T_polyI_reg <- lm((ltgAll^(-1/3))~poly(L_Infra,2)*L_Seis)


start <- ltgAll^(-1/3)
inverse <- ltgAll^(1/3)

try <- start*inverse 
try

transformation=list(link= NULL, inverse = function(x) x^(1/3))






#safe copy
plot(effect("poly(L_Infra,2):L_Seis",m3.lin_ltg_short_interact_neg13T_polyI_reg,
            xlevels=list(L_Seis=c(4.1,5,5.5,6,7.5))), layout=c(5,1), alternating=FALSE, 
     main="Interaction of L_Infra and L_Seis on LTG_polyI", 
     rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,1), transform =list(link= NULL, inverse = function(x) x^(1/3)))

