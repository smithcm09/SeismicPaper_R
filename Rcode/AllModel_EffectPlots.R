#plotting the effect plots for all models
#load the OnlyModels.RData Files

#S1.1 ----
attach(S1_DataFrame)

#S1_model <- bothways2
summary(S1_model)
plot(allEffects(S1_model), ylim=c(-2.5,2.5))
detach(S1_DataFrame)

#S2 ----
attach(S2_DataFrame)

#S2.1
#S2.1_model <- m.log_CRFyn_short_o
summary(S2.1_model)
plot(allEffects(S2.1_model), ylim=c(-2.5,2.5))

#S2.2
#S2.2_model <- m.log_LTGyn_short_o
summary(S2.2_model)
plot(allEffects(S2.2_model), ylim=c(-2.5,2.5))

#S2.3
#S2.3_model <- m.lin_EA_short_interact_o_sqrtTy
summary(S2.3_model)
plot(effect("L_Seis:Sq_Idur",S2.3_model,xlevels=list(Sq_Idur=c(1.5,4,6.5,8.25,10))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Seis and Sq_Sdur on Electric Activity Duration", rotx=45, more=FALSE, grid=FALSE, lwd=1)

#S2.4
#S2.4_model <- m.lin_NLS_short_interact_o_sqrtTy
summary(S2.4_model)
plot(effect("L_Seis:L_Infra",S2.4_model,xlevels=list(L_Infra=c(3.5,5,6.5,8,9.5))), layout=c(5,1), alternating=FALSE, main="Interaction of L_Infra and L_Seis on NLS", rotx=45, more=FALSE, grid=FALSE, lwd=1)

detach(S2_DataFrame)

#S3.1 ----
attach(S3_DataFrame)

#S3_model <- m3.lin_ltg_short_interact_neg13T_polyI_reg

summary(S3_model)
plot(effect("poly(L_Infra,2):L_Seis",S3_model,
            xlevels=list(L_Seis=c(4.1,5,5.5,6,7.5))), layout=c(5,1), alternating=FALSE, 
     main="Interaction of L_Infra and L_Seis on LTG_polyI", 
     rotx=45, more=FALSE, grid=FALSE, lwd=1, ylim = c(0,1), trans=list(link= NULL, inverse = function(x) x^(1/3)))

detach(S3_DataFrame)

# Figure 12
plot(S2_DataFrame$L_VASR,S2_DataFrame$L_Seis, col = CRF_yn2)
