##Model 1----

attach(DF_S1)
summary(ModelS1.1)

L_VASR <- log10(VASR_AB)
L_Infra <- log10(OverallABmedInfra)
L_Seis <- log10(OverallABmedSeis)
Sq_Sdur <- sqrt(maxseisDur)
Sq_Idur <- sqrt(maxinfraDur)

#1.1----
plot(allEffects(ModelS1.1))

MS1.1 <- glm(formula = Electrical_Factor ~ log10(OverallABmedInfra) + log10(VASR_AB) + sqrt(maxseisDur) + 
               sqrt(maxinfraDur), family = binomial(link = logit), subset = -c(591, 
                                                                     752))
summary(MS1.1)

plot(Effect("VASR_AB", MS1.1, transform.x=list(VASR_AB=list(trans=log10, inverse=exp))), ylim=c(-2.5,2.5), ylab = "Probability of Electrical Activity", xlab = "VASR", main = "")
plot(Effect("OverallABmedInfra", MS1.1, transform.x=list(OverallABmedInfra=list(trans=log10, inverse=exp))), ylim=c(-2.5,2.5), ylab = "Probability of Electrical Activity", xlab = "Infrasound Energy (J)", main = "")
plot(Effect("maxseisDur", MS1.1, transform.x=list(maxseisDur=list(trans=sqrt, inverse=function(x) x**2))), ylim=c(-2.5,2.5), ylab = "Probability of Electrical Activity", xlab = "Seismic Duration (s)", main = "")
plot(Effect("maxinfraDur", MS1.1, transform.x=list(maxinfraDur=list(trans=sqrt, inverse=function(x) x**2))), ylim=c(-2.5,2.5), ylab = "Probability of Electrical Activity", xlab = "Infrasound Duration (s)", main = "")

detach(DF_S1)

## Example http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.1001&rep=rep1&type=pdf
#plot(eff.pres[1],
#     transform.x=list(income=list(trans=log10, inverse=function(x) 10^x)),
#     ticks.x=list(at=c(1000, 2000, 5000, 10000, 20000)))
##
    
## Model 2s----
attach(DF_S2)

L_VASR <- log10(VASR_AB)
L_Infra <- log10(OverallABmedInfra)
L_Seis <- log10(OverallABmedSeis)
Sq_Sdur <- sqrt(maxseisDur)
Sq_Idur <- sqrt(maxinfraDur)

#2.1----
plot(allEffects(ModelS2.1), ylim=c(-2.5,2.5))
summary(ModelS2.1)
MS2.1 <- glm(formula = CRF_yn2 ~ log10(VASR_AB) + log10(OverallABmedSeis), family = binomial(link = logit), 
             subset = -c(590, 87, 141, 188, 9, 8, 594))
summary(MS2.1)

plot(Effect("VASR_AB", MS2.1, transform.x=list(VASR_AB=list(trans=log10, inverse=exp))), ylim=c(-2.5,2.5), ylab = "Probability of CRF", xlab = "VASR", main = "")
plot(Effect("OverallABmedSeis", MS2.1, transform.x=list(OverallABmedSeis=list(trans=log10, inverse=exp))), ylim=c(-2.5,2.5), ylab = "Probability of CRF", xlab = "Seismic Energy (J)", main = "")


#2.2----
plot(allEffects(ModelS2.2), ylim=c(-2.5,2.5))
summary(ModelS2.2)
MS2.2 <- glm(formula = Factor_ltgAll ~ log10(OverallABmedInfra) + sqrt(maxinfraDur), family = binomial(link = logit), 
             subset = -c(590, 87, 141, 188, 9, 8, 594))
summary(MS2.2)

plot(Effect("OverallABmedInfra", MS2.2, transform.x=list(OverallABmedInfra=list(trans=log10, inverse=exp))), ylim=c(-2.5,2.5), ylab = "Probability of Lightning", xlab = "Infrasound Energy (J)", main = "")
plot(Effect("maxinfraDur", MS2.2, transform.x=list(maxinfraDur=list(trans=sqrt, inverse=function(x) x**2))), ylim=c(-2.5,2.5), ylab = "Probability of Lightning", xlab = "Infrasound Duration (s)", main = "")


#2.3----
plot(effect("L_Seis:Sq_Idur",ModelS2.3,xlevels=list(Sq_Idur=c(1.5,4,6.5,8.25,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1)
summary(ModelS2.3)
MS2.3 <- lm(formula = sqrt(start_ElectricActivity) ~ log10(OverallABmedSeis) * sqrt(maxinfraDur), 
            subset = -c(590, 87, 141, 188, 9, 8, 594))
summary(MS2.3)

plot(Effect(c("OverallABmedSeis", "maxinfraDur"), MS2.3, 
            transformation=list(start_ElectricActivity=list(trans=sqrt, inverse=function(x) x**2), maxinfraDur=list(trans=sqrt, inverse=function(x) x**2), OverallABmedSeis=list(trans=log, inverse=exp))), 
     layout=c(5,1), ylim=c(0,15),log="x",
     alternating=FALSE, xlab = "Seismic Energy (J)", 
     main="Interaction of Seis and Infra Duration on Electrical Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1)


#2.4----
IE <- DF_S2$OverallABmedInfra
plot(effect("L_Seis:L_Infra",ModelS2.4,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)
summary(ModelS2.4)
MS2.4 <- lm(formula = sqrt(NLS) ~ log10(OverallABmedSeis) * log10(IE) + sqrt(maxseisDur), subset = -c(590, 87, 141, 188, 9, 8, 594))
summary(MS2.4)

#Interaction
plot(Effect(c("OverallABmedSeis", "IE"), MS2.4, 
            transformation=list(NLS=list(trans=sqrt, inverse=function(x) x**2), IE=list(trans=log, inverse=exp), OverallABmedSeis=list(trans=log, inverse=exp)),
            xlevels=list(IE=c(5e4,5e5,5e6,5e7,5e8))), 
     layout=c(5,1), ylim=c(0,60),
     alternating=FALSE, xlab = "Seismic Energy (J)", 
     main="Interaction of Infra and Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)

#SeisDur
plot(Effect("maxseisDur", MS2.4,transform=list(trans=sqrt, inverse=function(x) x**2)), ylab = "Number of Located Sources", xlab = "Seismic Duration (s)", main = "")


## MODEL 3---
attach(DF_S3)

L_VASR <- log10(VASR_AB)
L_Infra <- log10(OverallABmedInfra)
L_Seis <- log10(OverallABmedSeis)
Sq_Sdur <- sqrt(maxseisDur)
Sq_Idur <- sqrt(maxinfraDur)

#3.1
ModelS3.1<- lm(formula = (ltgAll^(-1/3)) ~ poly(L_Infra, 2)*L_Seis)
summary(ModelS3.1)
plot(effect("poly(L_Infra,2):L_Seis",ModelS3.1,
            xlevels=list(L_Seis=c(4.1,5,5.5,6,7.5))), layout=c(5,1), alternating=FALSE, 
     main="Interaction of L_Infra and L_Seis on LTG_polyI", 
     rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,1), trans=list(link= NULL, inverse = function(x) x^(1/3)))

MS3.1 <- lm(formula = (ltgAll^(-1/3)) ~ log10(OverallABmedSeis) * poly(log10(OverallABmedInfra), 2))
summary(MS3.1)

# Testing Model 3
attach(DF_onlyLtgRows)
ModelS3.1_test<- lm(formula = (I(ltgAll^(-1/3))) ~ poly(log10(OverallABmedInfra), 2)*log10(OverallABmedSeis))
plot(allEffects(ModelS3.1_test))
plot(allEffects(m3.lin_ltg_short_interact_neg13T_polyI))
