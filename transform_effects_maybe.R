mod <- lm(log(prestige) ~ income:type + education, data=Prestige)

plot(Effect(c("income", "type"), mod, transformation=list(link=log, inverse=exp)),
     axes=list(y=list(lab="prestige")))


attach(DF_S1)

MS1.1 <- glm(formula = Electrical_Factor ~ log10(OverallABmedInfra) + 
      log10(VASR_AB) + sqrt(maxseisDur) + sqrt(maxinfraDur), family = binomial(link = logit), 
    subset = -c(591, 752))
summary(MS1.1)

plot(allEffects(MS1.1), ylim=qlogis(c(0.1,0.9)))

detach(DF_S1)

attach(DF_S2)

summary(MS2.2)
plot(allEffects(MS2.2), ylim=qlogis(c(0.1,0.9)))


MS2.4 <- lm(formula = sqrt(NLS) ~ log10(OverallABmedSeis) * log10(OverallABmedInfra) + 
     sqrt(maxseisDur), subset = -c(590, 87, 141, 188, 9, 8, 594))

summary(MS2.4)
plot(allEffects(MS2.4))

plot(effect("log10(OverallABmedSeis):log10(OverallABmedInfra)",MS2.4,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim=c(0,60))
plot(effect("sqrt(maxseisDur)", MS2.4), ylim=c(0,30))
