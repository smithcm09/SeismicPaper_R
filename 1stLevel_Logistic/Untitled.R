attach(Binary_DataFrame)
fullmod <- glm(Electrical_Factor~L_VASR+L_Infra+L_Seis+Sq_Sdur+Sq_Idur, family = binomial(link=logit))
nothing <- glm(Electrical_Factor~1, family = binomial(link=logit))
bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)), direction="both")
#remove outliers
bothways2 <- update(bothways, subset=-c(591,752))
summary(bothways2)
Anova(bothways2)
# look at interactions
Anova(glm(Electrical_Factor~L_VASR*L_Infra*Sq_Sdur*Sq_Idur, family = binomial(link=logit)))

summary(glm(Electrical_Factor~L_VASR+L_Infra*Sq_Sdur+Sq_Idur, family = binomial(link=logit)))
summary(glm(Electrical_Factor~L_VASR+L_Infra*Sq_Sdur*Sq_Idur, family = binomial(link=logit)))

1-1681.5/2037.8
1-1659.1/2040.6
1-1678.7/2040

plot(allEffects((glm(Electrical_Factor~L_VASR+L_Infra*Sq_Sdur*Sq_Idur, family = binomial(link=logit)))))
plot(allEffects((glm(Electrical_Factor~L_VASR*L_Infra*Sq_Sdur*Sq_Idur, family = binomial(link=logit)))))

