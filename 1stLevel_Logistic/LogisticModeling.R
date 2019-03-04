# (1) Data Exploration and (2) Logistic Modeling for the seismic and infrasound data w.r.t. Electrical Y/N properties
# (2) Data Exploration

attach(Binary_DataFrame)

# summary and exploration ----
#as.factor(Electrical_Factor)
summary(as.factor(Electrical_Factor))

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
Boxplot(log(VASR_AB)~Electrical_Factor, subset=-c(591,752), notch=TRUE, id.n=0)
Boxplot(log(OverallABmedInfra)~Electrical_Factor, notch=TRUE, id.n=0)
Boxplot(log(OverallABmedSeis)~Electrical_Factor, notch=TRUE, id.n=0)
Boxplot(sqrt(maxseisDur)~Electrical_Factor, notch=TRUE, id.n=0)
Boxplot(sqrt(maxinfraDur)~Electrical_Factor, notch=TRUE, id.n=0)

#transform variables ----
L_VASR <- log10(VASR_AB)
L_Infra <- log10(OverallABmedInfra)
L_Seis <- log10(OverallABmedSeis)
Sq_Sdur <- sqrt(maxseisDur)
Sq_Idur <- sqrt(maxinfraDur)

#inital model----
g1 <- glm(Electrical_Factor~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
#Wald Tests
summary(g1)

#look at confidence intervals of the odds
round(exp(cbind(Estimate=coef(g1), confint(g1))),2)

#Type II tests - Analysis of Devence Table - liklihood ratio tests - prefered over Wald
Anova(g1)

#Effectsplot
plot(allEffects(g1))

#stepwise regression to look at potentially removing a variable to lower AIC ----
gb <- step(g1, direction = 'backward')

#result is to remove L_Seis to lowe AIC to 1713
#check how similar to the models are if remove L_Seis
plot(fitted(g1)~fitted(gb))
abline(0,1, col = 'red')
cor(fitted(g1),fitted(gb))
#correlated at 0.9996906 so practically the same model so keep L_Seis

#Section D inital regression analysis ----
hatvalues(g1)
outlierTest(g1)
dfbetasPlots(g1)
influenceIndexPlot(g1, id.n=5)
influencePlot(g1, id.n=5) #one I like

#model comparisons ----
#compare models removing highly influential outliers
compareCoefs(g1, update(g1, subset=-c(591)))
compareCoefs(g1, update(g1, subset=-c(752)))
compareCoefs(g1, update(g1, subset=-c(1006)))
compareCoefs(g1, update(g1, subset=-c(591,752)))
compareCoefs(g1, update(g1, subset=-c(591,1006)))
compareCoefs(g1, update(g1, subset=-c(752,1006)))
compareCoefs(g1, update(g1, subset=-c(591,752,1006)))

#compare model coefficients removing each variable at a time
compareCoefs(g1, update(g1, .~. -L_VASR))
compareCoefs(g1, update(g1, .~. -L_Infra))
compareCoefs(g1, update(g1, .~. -L_Seis))
compareCoefs(g1, update(g1, .~. -Sq_Sdur))
compareCoefs(g1, update(g1, .~. -Sq_Idur))


#component residual plots 
crPlots(g1, id.n=1)
#looking at the models if remove large influence points
crPlots(update(g1, subset=-c(591,752,1006)), id.n=1)

#Section E
residualPlots(g1)
vif(g1)
vif(update(g1, subset=-c(591,752,1006)))

#determine whats going on with the three outliers 591,752,1006 ----
thetimes[591]
#[1] (07-June-2015 17:12:06)
thetimes[752]
#[1] (30-May-2015 11:25:17)
thetimes[1006]
#[1] (04-June-2015 12:04:11)
# one of the large key reference events - double checked waveform and ltg data - all looks good
#View Outliers
View(Binary_DataFrame[c(591,752,1006),])
#plotted in Matlab and determined that 591 and 752 are bad data points based on infrasound being picked prior to seismic so likely the mixing of two separate event picks
#therefore will remove 591 and 752 from model moving forward, 1006 is wonky but correct
# update model to remove outliers ----
g2 <- update(g1, subset=-c(591,752))

#Wald Tests
summary(g2)
summary(g1)
#look at confidence intervals of the odds
round(exp(cbind(Estimate=coef(g2), confint(g2))),2)

#Type II tests - Analysis of Devence Table - liklihood ratio tests - prefered over Wald
Anova(g2)

#Effectsplot
plot(allEffects(g2))

#step model
gb2 <- step(g2, direction = 'backward')
#drops 2 points if remove L_Seis - doesn't seem like enough...

#FULL BOTH DIRECTION STEP MODEL----
fullmod <- glm(Electrical_Factor~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
nothing <- glm(Electrical_Factor~1, family = binomial(link=logit))
bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)), direction="both")
#remove outliers
bothways2 <- update(bothways, subset=-c(591,752))
summary(bothways2)
Anova(bothways2)
# look at interactions
Anova(glm(Electrical_Factor~L_VASR*L_Infra*Sq_Sdur*Sq_Idur, family = binomial(link=logit)))

plot(allEffects(bothways2))
vif(bothways2)

scatterplot(Sq_Sdur,L_VASR)
scatterplot(Sq_Sdur,L_Infra)
scatterplot(Sq_Sdur,Sq_Idur)
scatterplot(Sq_Idur,L_VASR)
scatterplot(Sq_Idur,L_Infra)

test <- glm(Electrical_Factor ~ L_Infra + L_VASR + Sq_Sdur)
test2 <- glm(Electrical_Factor ~ L_Infra + L_VASR + Sq_Idur)

summary(bothways2)
summary(test)
summary(test2)
vif(test)
vif(test2)
1-(294.44/363.03)
1-(295.33/364.03)

# redo regression analysis with updated g2 model ----
hatvalues(g2)
outlierTest(g2)
dfbetasPlots(g2)
influenceIndexPlot(g2, id.n=5)
influencePlot(g2, id.n=2)
vif(g2)


#model comparisons ----
compareCoefs(g2, update(g2, .~. -L_VASR))
compareCoefs(g2, update(g2, .~. -L_Infra))
compareCoefs(g2, update(g2, .~. -L_Seis))
compareCoefs(g2, update(g2, .~. -Sq_Sdur))
compareCoefs(g2, update(g2, .~. -Sq_Idur))


#temporal plots to check independence ----
residuals_g1 <- rstudent(g1)
plot(thetimes,residuals_g1)
hv_g1 <-hatvalues(g1)
plot(thetimes,hv_g1)
cd_g1 <- cooks.distance(g1)
plot(thetimes,log(cd_g1))


#Investigating the multicollineratiy issue----
#regress each predictor against the other predictors

scatterplotMatrix(~L_VASR+L_Seis+L_Infra+Sq_Sdur+Sq_Idur, subset=-c(591,752))

m.VASR <- lm(L_VASR~L_Seis+L_Infra+Sq_Sdur+Sq_Idur)
m.Seis <- lm(L_Seis~L_VASR+L_Infra+Sq_Sdur+Sq_Idur)
m.Infra <- lm(L_Infra~L_VASR+L_Seis+Sq_Sdur+Sq_Idur)
m.Sdur <- lm(Sq_Sdur~L_VASR+L_Seis+L_Infra+Sq_Idur)
m.Idur <- lm(Sq_Idur~L_VASR+L_Seis+L_Infra+Sq_Sdur)

summary(m.VASR)
summary(m.Seis)
summary(m.Infra)
summary(m.Sdur)
summary(m.Idur)

vif(m.VASR)
vif(m.Seis)
vif(m.Infra)
vif(m.Sdur)
vif(m.Idur)

