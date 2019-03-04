#CRFonlyDF----
attach(CRFonlyDF)
#summary stats----
summary(CRFonlyDF$NLS)
summary(CRFonlyDF$ElectricActivity)
summary(CRFonlyDF$postCRFltgDur)
summary(CRFonlyDF$CRF_nls)
summary(CRFonlyDF$postCRF_NLS)
summary(CRFonlyDF$total_postCRF_ltg)
summary(CRFonlyDF$VASR_AB)
summary(CRFonlyDF$OverallABmedInfra)
summary(CRFonlyDF$OverallABmedSeis)
summary(CRFonlyDF$maxseisDur)
summary(CRFonlyDF$maxinfraDur)

plot(CRFonlyDF$NLS)
plot(CRFonlyDF$ElectricActivity)
plot(CRFonlyDF$postCRFltgDur+0.001)
plot(CRFonlyDF$CRF_nls)
plot(CRFonlyDF$postCRF_NLS)
plot(CRFonlyDF$total_postCRF_ltg)
plot(CRFonlyDF$VASR_AB)
plot(CRFonlyDF$OverallABmedInfra)
plot(CRFonlyDF$OverallABmedSeis)
plot(CRFonlyDF$maxseisDur)
plot(CRFonlyDF$maxinfraDur)

Boxplot(CRFonlyDF$NLS)
Boxplot(CRFonlyDF$ElectricActivity)
Boxplot(CRFonlyDF$postCRFltgDur+0.001)
Boxplot(CRFonlyDF$CRF_nls)
Boxplot(CRFonlyDF$postCRF_NLS)
Boxplot(CRFonlyDF$total_postCRF_ltg)
Boxplot(CRFonlyDF$VASR_AB)
Boxplot(CRFonlyDF$OverallABmedInfra)
Boxplot(CRFonlyDF$OverallABmedSeis)
Boxplot(CRFonlyDF$maxseisDur)
Boxplot(CRFonlyDF$maxinfraDur)

#symbox and powerTransform stats ----
symbox(CRFonlyDF$NLS)
symbox(CRFonlyDF$ElectricActivity)
symbox(CRFonlyDF$postCRFltgDur+0.001)
symbox(CRFonlyDF$CRF_nls)
symbox(CRFonlyDF$postCRF_NLS)
symbox(CRFonlyDF$total_postCRF_ltg)
symbox(CRFonlyDF$VASR_AB)
symbox(CRFonlyDF$OverallABmedInfra)
symbox(CRFonlyDF$OverallABmedSeis)
symbox(CRFonlyDF$maxseisDur)
symbox(CRFonlyDF$maxinfraDur)

powerTransform(CRFonlyDF$NLS)
powerTransform(CRFonlyDF$ElectricActivity)
powerTransform(CRFonlyDF$postCRFltgDur+0.001)
powerTransform(CRFonlyDF$CRF_nls)
powerTransform(CRFonlyDF$postCRF_NLS)
powerTransform(CRFonlyDF$total_postCRF_ltg)
powerTransform(CRFonlyDF$VASR_AB)
powerTransform(CRFonlyDF$OverallABmedInfra)
powerTransform(CRFonlyDF$OverallABmedSeis)
powerTransform(CRFonlyDF$maxseisDur)
powerTransform(CRFonlyDF$maxinfraDur)


#Scatterplotmatrix ----
#raw
scatterplotMatrix(~ElectricActivity+postCRFltgDur+NLS+CRF_nls+total_postCRF_ltg+VASR_AB+OverallABmedInfra+OverallABmedSeis+maxseisDur+maxinfraDur)
#transformed
pClD <- postCRFltgDur+0.001
scatterplotMatrix(~ElectricActivity+pClD+NLS+CRF_nls+total_postCRF_ltg+VASR_AB+OverallABmedInfra+OverallABmedSeis+maxseisDur+maxinfraDur,transform=TRUE)


#1-on-1 linear regressions of continuous variables ----
EA_1 <- lm(ElectricActivity~log(VASR_AB))
EA_2 <- lm(ElectricActivity~log(OverallABmedInfra))
EA_3 <- lm(ElectricActivity~log(OverallABmedSeis))
EA_4 <- lm(ElectricActivity~maxseisDur)
EA_5 <- lm(ElectricActivity~maxinfraDur)
pCDur_1 <- lm(postCRFltgDur~log(VASR_AB))
pCDur_2 <- lm(postCRFltgDur~log(OverallABmedInfra))
pCDur_3 <- lm(postCRFltgDur~log(OverallABmedSeis))
pCDur_4 <- lm(postCRFltgDur~maxseisDur)
pCDur_5 <- lm(postCRFltgDur~maxinfraDur)
NLS_1 <- lm(NLS~log(VASR_AB))
NLS_2 <- lm(NLS~log(OverallABmedInfra))
NLS_3 <- lm(NLS~log(OverallABmedSeis))
NLS_4 <- lm(NLS~maxseisDur)
NLS_5 <- lm(NLS~maxinfraDur)
CNLS_1 <- lm(CRF_nls~log(VASR_AB))
CNLS_2 <- lm(CRF_nls~log(OverallABmedInfra))
CNLS_3 <- lm(CRF_nls~log(OverallABmedSeis))
CNLS_4 <- lm(CRF_nls~maxseisDur)
CNLS_5 <- lm(CRF_nls~maxinfraDur)
pCNLS_1 <- lm(postCRF_NLS~log(VASR_AB))
pCNLS_2 <- lm(postCRF_NLS~log(OverallABmedInfra))
pCNLS_3 <- lm(postCRF_NLS~log(OverallABmedSeis))
pCNLS_4 <- lm(postCRF_NLS~maxseisDur)
pCNLS_5 <- lm(postCRF_NLS~maxinfraDur)
tpCltg_1 <- lm(total_postCRF_ltg~log(VASR_AB))
tpCltg_2 <- lm(total_postCRF_ltg~log(OverallABmedInfra))
tpCltg_3 <- lm(total_postCRF_ltg~log(OverallABmedSeis))
tpCltg_4 <- lm(total_postCRF_ltg~maxseisDur)
tpCltg_5 <- lm(total_postCRF_ltg~maxinfraDur)

#summary statistics of each model ----
summary(EA_1) 
summary(EA_2)
summary(EA_3) 
summary(EA_4)
summary(EA_5)
summary(pCDur_1)
summary(pCDur_2)
summary(pCDur_3)
summary(pCDur_4)
summary(pCDur_5)
summary(NLS_1)
summary(NLS_2)
summary(NLS_3)
summary(NLS_4)
summary(NLS_5)
summary(CNLS_1) 
summary(CNLS_2)
summary(CNLS_3) 
summary(CNLS_4) 
summary(CNLS_5) 
summary(pCNLS_1)
summary(pCNLS_2)
summary(pCNLS_3)
summary(pCNLS_4)
summary(pCNLS_5)
summary(tpCltg_1)
summary(tpCltg_2)
summary(tpCltg_3)
summary(tpCltg_4)
summary(tpCltg_5)

# multivariate regressions of continuous variables ----
EA_multi <- lm(ElectricActivity~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+maxseisDur+maxinfraDur)
pCDur_multi <- lm(postCRFltgDur~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+maxseisDur+maxinfraDur)
NLS_multi <- lm(NLS~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+maxseisDur+maxinfraDur)
CNLS_multi <- lm(CRF_nls~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+maxseisDur+maxinfraDur)
pCNLS_multi <- lm(postCRF_NLS~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+maxseisDur+maxinfraDur)
tpCltg_multi <- lm(total_postCRF_ltg~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+maxseisDur+maxinfraDur)

#summary stats of each multivariate model ----
summary(EA_multi)
summary(pCDur_multi)
summary(NLS_multi)
summary(CNLS_multi)
summary(pCNLS_multi)
summary(tpCltg_multi)
vif(EA_multi)
vif(pCDur_multi)
vif(NLS_multi)
vif(CNLS_multi)
vif(pCNLS_multi)
vif(tpCltg_multi)
#----
detach(CRFonlyDF)

#NoCRFonlyDF ----
attach(NoCRFonlyDF)
#summary stats----
summary(NLS)
summary(ElectricActivity)
summary(ltgAll)
summary(VASR_AB)
summary(OverallABmedInfra)
summary(OverallABmedSeis)
summary(maxseisDur)
summary(maxinfraDur)

plot(NLS)
plot(ElectricActivity)
plot(ltgAll)
plot(VASR_AB)
plot(OverallABmedInfra)
plot(OverallABmedSeis)
plot(maxseisDur)
plot(maxinfraDur)

Boxplot(NLS)
Boxplot(ElectricActivity)
Boxplot(ltgAll)
Boxplot(VASR_AB)
Boxplot(OverallABmedInfra)
Boxplot(OverallABmedSeis)
Boxplot(maxseisDur)
Boxplot(maxinfraDur)

#symbox and powerTransform stats ----
symbox(NLS+0.001)
symbox(ElectricActivity+0.001)
symbox(ltgAll+0.001)
symbox(VASR_AB)
symbox(OverallABmedInfra)
symbox(OverallABmedSeis)
symbox(maxseisDur)
symbox(maxinfraDur)

powerTransform(NLS+0.001)
powerTransform(ElectricActivity+0.001)
powerTransform(ltgAll+0.001)
powerTransform(VASR_AB)
powerTransform(OverallABmedInfra)
powerTransform(OverallABmedSeis)
powerTransform(maxseisDur)
powerTransform(maxinfraDur)

#1-on-1 linear regressions of continuous variables ----
nEA_1 <- lm(ElectricActivity~log(VASR_AB))
nEA_2 <- lm(ElectricActivity~log(OverallABmedInfra))
nEA_3 <- lm(ElectricActivity~log(OverallABmedSeis))
nEA_4 <- lm(ElectricActivity~sqrt(maxseisDur))
nEA_5 <- lm(ElectricActivity~sqrt(maxinfraDur))
nNLS_1 <- lm(NLS~log(VASR_AB))
nNLS_2 <- lm(NLS~log(OverallABmedInfra))
nNLS_3 <- lm(NLS~log(OverallABmedSeis))
nNLS_4 <- lm(NLS~sqrt(maxseisDur))
nNLS_5 <- lm(NLS~sqrt(maxinfraDur))
nLA_1 <- lm(ltgAll~log(VASR_AB))
nLA_2 <- lm(ltgAll~log(OverallABmedInfra))
nLA_3 <- lm(ltgAll~log(OverallABmedSeis))
nLA_4 <- lm(ltgAll~sqrt(maxseisDur))
nLA_5 <- lm(ltgAll~sqrt(maxinfraDur))
#summary statistics of each model ----
summary(nEA_1) 
summary(nEA_2)
summary(nEA_3) 
summary(nEA_4)
summary(nEA_5)
summary(nNLS_1)
summary(nNLS_2)
summary(nNLS_3)
summary(nNLS_4)
summary(nNLS_5)
summary(nLA_1)
summary(nLA_2)
summary(nLA_3)
summary(nLA_4)
summary(nLA_5)

#multivariate regressions of continuous variables ----
nEA_multi <- lm(ElectricActivity~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur))
nNLS_multi <- lm(NLS~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur))
nLA_multi <- lm(ltgAll~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur))
#summary statistics of multivar models----
summary(nEA_multi)
summary(nNLS_multi)
summary(nLA_multi)
vif(nEA_multi)
vif(nNLS_multi)
vif(nLA_multi)

#----
detach(NoCRFonlyDF)

#NoElectricalDL----
attach(NoElectricalDF)
#summary stats----
summary(VASR_AB)
summary(OverallABmedInfra)
summary(OverallABmedSeis)
summary(maxseisDur)
summary(maxinfraDur)

plot(NLS)
plot(ElectricActivity)
plot(ltgAll)
plot(VASR_AB)
plot(OverallABmedInfra)
plot(OverallABmedSeis)
plot(maxseisDur)
plot(maxinfraDur)

Boxplot(NLS)
Boxplot(ElectricActivity)
Boxplot(ltgAll)
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

#----
detach(NoElectricalDF)

# copying and combining the DFs ----
a <- subset(CRFonlyDF, select=c(ElectricActivity,NLS,total_postCRF_ltg,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur))
#rename the total_postCRF_ltg count to ltgALL so can compare with other datasets
#copied from stackoverflow
# df = dataframe
# old.var.name = The name you don't like anymore
# new.var.name = The name you want to get
names(a)[names(a) == 'total_postCRF_ltg'] <- 'ltgAll'

b <- subset(NoCRFonlyDF, select=c(ElectricActivity,NLS,ltgAll,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur))
c <- subset(NoElectricalDF, select=c(ElectricActivity,NLS,ltgAll,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur))

AllCombinedElectricalDF <- rbind(a,b,c)




#AllCombinedElectricalDF ----
attach(AllCombinedElectricalDF)
#summary stats ----
summary(NLS)
summary(ElectricActivity)
summary(ltgAll)
summary(VASR_AB)
summary(OverallABmedInfra)
summary(OverallABmedSeis)
summary(maxseisDur)
summary(maxinfraDur)

plot(NLS)
plot(ElectricActivity)
plot(ltgAll)
plot(VASR_AB)
plot(OverallABmedInfra)
plot(OverallABmedSeis)
plot(maxseisDur)
plot(maxinfraDur)

Boxplot(NLS)
Boxplot(ElectricActivity)
Boxplot(ltgAll)
Boxplot(VASR_AB)
Boxplot(OverallABmedInfra)
Boxplot(OverallABmedSeis)
Boxplot(maxseisDur)
Boxplot(maxinfraDur)

#symbox and powerTransform stats ----
symbox(NLS+0.001)
symbox(ElectricActivity+0.001)
symbox(ltgAll+0.001)
symbox(VASR_AB)
symbox(OverallABmedInfra)
symbox(OverallABmedSeis)
symbox(maxseisDur)
symbox(maxinfraDur)

powerTransform(NLS+0.001)
powerTransform(ElectricActivity+0.001)
powerTransform(ltgAll+0.001)
powerTransform(VASR_AB)
powerTransform(OverallABmedInfra)
powerTransform(OverallABmedSeis)
powerTransform(maxseisDur)
powerTransform(maxinfraDur)


##
plot(log(OverallABmedInfra),log(OverallABmedSeis))
test <- lm(log(OverallABmedInfra)~log(OverallABmedSeis))
#1-on-1 linear regressions of each continuous variable----
AllEA_1 <- lm(ElectricActivity~log(VASR_AB))
AllEA_2 <- lm(ElectricActivity~log(OverallABmedInfra))
AllEA_3 <- lm(ElectricActivity~log(OverallABmedSeis))
AllEA_4 <- lm(ElectricActivity~sqrt(maxseisDur))
AllEA_5 <- lm(ElectricActivity~sqrt(maxinfraDur))
AllNLS_1 <- lm(NLS~log(VASR_AB))
AllNLS_2 <- lm(NLS~log(OverallABmedInfra))
AllNLS_3 <- lm(NLS~log(OverallABmedSeis))
AllNLS_4 <- lm(NLS~sqrt(maxseisDur))
AllNLS_5 <- lm(NLS~sqrt(maxinfraDur))
AllLA_1 <- lm(ltgAll~log(VASR_AB))
AllLA_2 <- lm(ltgAll~log(OverallABmedInfra))
AllLA_3 <- lm(ltgAll~log(OverallABmedSeis))
AllLA_4 <- lm(ltgAll~sqrt(maxseisDur))
AllLA_5 <- lm(ltgAll~sqrt(maxinfraDur))
#summary stats of each model----
summary(AllEA_1) 
summary(AllEA_2)
summary(AllEA_3) 
summary(AllEA_4)
summary(AllEA_5)
summary(AllNLS_1)
summary(AllNLS_2)
summary(AllNLS_3)
summary(AllNLS_4)
summary(AllNLS_5)
summary(AllLA_1)
summary(AllLA_2)
summary(AllLA_3)
summary(AllLA_4)
summary(AllLA_5)

#multivariate regressions of continuous variables ----
AllEA_multi <- lm(ElectricActivity~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur))
AllNLS_multi <- lm(NLS~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur))
AllLA_multi <- lm(ltgAll~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur))
#summary statistics of multivar models----
summary(AllEA_multi)
summary(AllNLS_multi)
summary(AllLA_multi)
vif(AllEA_multi)
vif(AllNLS_multi)
vif(AllLA_multi)

#combining all the DFs including the factor of crf
#----
detach(AllCombinedElectricalDF)
# copying and combining the DFs ----
a <- subset(CRFonlyDF, select=c(ElectricActivity,NLS,total_postCRF_ltg,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur,CRF_yn))
#rename the total_postCRF_ltg count to ltgALL so can compare with other datasets
#copied from stackoverflow
# df = dataframe
# old.var.name = The name you don't like anymore
# new.var.name = The name you want to get
names(a)[names(a) == 'total_postCRF_ltg'] <- 'ltgAll'

b <- subset(NoCRFonlyDF, select=c(ElectricActivity,NLS,ltgAll,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur,CRF_yn))
c <- subset(NoElectricalDF, select=c(ElectricActivity,NLS,ltgAll,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur,CRF_yn))

AllCombinedElectricalDF2 <- rbind(a,b,c)
as.factor(AllCombinedElectricalDF2$CRF_yn)
summary(AllCombinedElectricalDF2)

#AllCombinedElectricalDF2 ----
attach(AllCombinedElectricalDF2)
#Multi-Var Regression adding in the factor of CRFyno ----
AllEA_multi2 <- lm(ElectricActivity~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur)+CRF_yn)
AllNLS_multi2 <- lm(NLS~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur)+CRF_yn)
AllLA_multi2 <- lm(ltgAll~log(VASR_AB)+log(OverallABmedInfra)+log(OverallABmedSeis)+sqrt(maxseisDur)+sqrt(maxinfraDur)+CRF_yn)

step(nEA_multi2,direction="backward")
step(nNLS_multi2,direction="backward")
step(nLA_multi2,direction="backward")

summary(AllEA_multi2)
summary(AllNLS_multi2)
summary(AllLA_multi2)
vif(AllEA_multi2)
vif(AllNLS_multi2)
vif(AllLA_multi2)
#Multi-Var Regression adding in factor of CRFyno and taking out all VIF failing variables except OverallABmedInfra sinc ein individual regressions it had the highest p and R2 values ----
#still using the AllCombinedElectrical DF2 datafram----
AllEA_multi3 <- lm(ElectricActivity~log(OverallABmedInfra)+CRF_yn)
AllNLS_multi3 <- lm(NLS~log(OverallABmedInfra)+CRF_yn)
AllLA_multi3 <- lm(ltgAll~log(OverallABmedInfra)+CRF_yn)
summary(AllEA_multi3)
summary(AllNLS_multi3)
summary(AllLA_multi3)
vif(AllEA_multi3)
vif(AllNLS_multi3)
vif(AllLA_multi3)
plot(allEffects(AllEA_multi3))
plot(allEffects(AllNLS_multi3))
plot(allEffects(AllLA_multi3))


#lmStats----
attach(AllCombinedElectricalDF)
model_testing <- AllLA_multi
summary(model_testing)
#detach(AllCombinedElectricalDF2)
#Section C
plot(allEffects(model_testing))
#cov2cor(model_testing)#error
#anova(model_testing)
#linearHypothesis(model_testing)#error
step(model_testing, direction = 'backward')
#regsubsets(model_testing)#error
#subsets(model_testing)#error
Anova(model_testing)

#Section D
#hatvalues(model_testing)
outlierTest(model_testing)
#dfbetasPlots(model_testing)
#influenceIndexPlot(model_testing)
influencePlot(model_testing, id.n=5) #one I like
#covratio(model_testing)
#avPlots(model_testing)

#Section E
residualPlots(model_testing)
ncvTest(model_testing)
spreadLevelPlot(model_testing)
qqPlot(model_testing)
crPlots(model_testing)
ceresPlots(model_testing)
inverseResponsePlot(model_testing)
vif(model_testing)
boxCox(model_testing) #won't plot bc not positive
#next line gives the numerical output thats ~same as boxcox plot output
summary(x1<-powerTransform(model_testing))
boxTidwell(model_testing)

#fixing heteroscadsicity
vc <- hccm(model_testing)
coeftest(model_testing, vcov=vc)#error

#######################################
# look at duration of crf in relation to duration of seis and infra signals



####check which dataframes are attached##########
#check which datasets are attached
intersect(search(), objects())
#if character(0) then none attached - run until see this as a result
# detach unneeded files
detach(AllCombinedElectricalDF)


##

# build two factor Yes Electrical / No Electrical for logistic Regression
# copying and combining the DFs ----
a <- subset(CRFonlyDF, select=c(SIDateTime, ElectricActivity,NLS,total_postCRF_ltg,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur,CRF_yn))
#rename the total_postCRF_ltg count to ltgALL so can compare with other datasets
#copied from stackoverflow
# df = dataframe
# old.var.name = The name you don't like anymore
# new.var.name = The name you want to get
names(a)[names(a) == 'total_postCRF_ltg'] <- 'ltgAll'

b <- subset(NoCRFonlyDF, select=c(SIDateTime, ElectricActivity,NLS,ltgAll,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur,CRF_yn))
intemediateDF <- rbind(a,b)
ElectricOnlyDF <- rbind(a,b)
#add variable to intermediateDF to indicate that they are yes (aka 1) electrical events
Electrical_Factor <- rep(1,615)
intemediateDF$Electrical_Factor <- Electrical_Factor


c <- subset(NoElectricalDF, select=c(SIDateTime, ElectricActivity,NLS,ltgAll,VASR_AB,OverallABmedInfra,OverallABmedSeis,maxseisDur,maxinfraDur))
Electrical_Factor2 <- rep(0,910)
c$Electrical_Factor <- Electrical_Factor2


Binary_DataFrame <- rbind(intemediateDF,c)
as.factor(Binary_DataFrame$Electrical_Factor)
summary(Binary_DataFrame)

save(Binary_DataFrame, file = 'Binary_DataFrame.RData')

save(ElectricOnlyDF, file = 'ElectricOnlyDF.RData')
