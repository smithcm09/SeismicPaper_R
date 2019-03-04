
Model3.1 <- lm((ltgAll^(-1/3))~poly(L_Infra,2)*L_Seis)
#where L_Infra is log10(OverallABmedInfra) and L_Seis is log10(OverallABmedSeis)

plot(effect("poly(L_Infra,2):L_Seis",m3.lin_ltg_short_interact_neg13T_polyI_reg,
            xlevels=list(L_Seis=c(4.5,5,5.5,6,7))), layout=c(5,1), alternating=FALSE, 
     main="Interaction of L_Infra and L_Seis on LTG_polyI",
     rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(-5,50), axes=list(y=list(transform=function(x)(1/x)^3,lab="All Lightning")))

#Dr. Readers script
attach(DF_onlyLtgRows)

S3_model <- lm((ltgAll^(-1/3))~poly(log10(OverallABmedInfra),2)*log10(OverallABmedSeis))


summary(S3_model)

# e1 <- effect("poly(log10(OverallABmedInfra), 2):log10(OverallABmedSeis)",S3_model, se.type = "Scheffe")
# plot(e1, xlevels=list(L_Seis=c(4,5,6,7,8)),layout=c(5,1), alternating=FALSE, 
#      main="Interaction of L_Infra and L_Seis on LTG_polyI", 
#      rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,70),
#      axes=list(y=list(transform=function(x)(1/x)^3,lab="All Lightning")))


S3_model_short <- lm(ltgAll^(-1/3)~(poly(L_Infra,2)*L_Seis))
e2 <- effect("poly(L_Infra, 2):L_Seis",S3_model_short, xlevels=list(L_Seis=c(4.5,5,5.5,6), L_Infra=seq(4,8,0.25)), transformation = list(link=function(x)(x^(-1/3)), inverse=function(x)(1/x)^3))
sum_e2 <- summary(e2)
sum_e2
#plot(e2)

plot(e2, layout=c(4,1), alternating=FALSE, 
     main="Interaction of L_Infra and L_Seis", ci.style = "bands",
     rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,15),
     axes=list(y=list(transform=function(x)(1/x)^3,lab="All Lightning")))
# 
# ## Fixing S2.3
# attach(DF_S2)
# L_Seis <- log10(OverallABmedSeis)
# Sq_Idur <- sqrt(maxinfraDur)
# ModelS2.3
# e3 <- effect("L_Seis:Sq_Idur",ModelS2.3)
# plot(e3, alternating=FALSE, xlevels=list(Sq_Idur=c(1.5, 4, 6.5, 8.25, 10)),layout=c(5,1),
#      main="Interaction of L_Seis and Sq_InfraDur", 
#      rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,100),
#      axes=list(y=list(transform=function(x)(x)^2,lab="Electrical Duration")))
# detach(DF_S2)
# 
# #Fixing S2.4
# attach(DF_S2)
# L_Seis <- log10(OverallABmedSeis)
# L_Infra <- log10(OverallABmedInfra)
# Sq_Sdur <- sqrt(maxseisDur)
# 
# summary(ModelS2.4)
# 
# e4 <- effect("L_Seis:L_Infra",ModelS2.4)
# plot(e4, alternating=FALSE, xlevels=list(L_Infra=c(4, 5, 6, 7, 8)), layout=c(5,1),
#      main="Interactions S2.M4", 
#      rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,3600),
#      axes=list(y=list(transform=function(x)(x)^2,lab="Number of Located SOurces")))
# 
# IE <- OverallABmedInfra
# e5 <- effect("sqrt(maxseisDur)", MS2.4)
# plot(e5, main="Interactions S2.M4", 
#      rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim=c(0,1000),
#      axes=list(y=list(transform=function(x)(x)^2,lab="Number of Located SOurces")))
# 
# detach(DF_S2)
