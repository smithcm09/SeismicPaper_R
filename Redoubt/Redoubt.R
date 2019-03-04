attach(RedoubtData)

#Transforms following original models ----
L_VASR <- log10(VASR)
L_Infra <- log10(Infra)
L_Seis <- log10(Seis)
Sq_Sdur <- sqrt(Sdur)
Sq_Idur <- sqrt(Idur)

Trans_Redoubt <- data.frame(L_VASR,L_Infra,L_Seis,Sq_Sdur,Sq_Idur)

#Attempts at Predictions ----

S1.1_pred <- predict.glm(bothways2, newdata = Trans_Redoubt, interval = "confidence")
S2.2_pred <- predict.glm(m.log_LTGyn_short_o, newdata = Trans_Redoubt, interval = "confidence")
S2.3_pred <- predict.lm(m.lin_EA_short_interact_o_sqrtTy, newdata = Trans_Redoubt, interval = "confidence")
S2.4_pred <- predict.lm(m.lin_NLS_short_interact_o_sqrtTy, newdata = Trans_Redoubt, interval = "confidence")
S3.1_pred <- predict.lm(m3.lin_ltg_short_interact_neg13T_polyI2_reg, newdata = Trans_Redoubt, interval = "confidence")

Predicted_Redoubt <- cbind(Trans_Redoubt,S1.1_pred)
Predicted_Redoubt <- cbind(Predicted_Redoubt,S2.2_pred)
Predicted_Redoubt <- cbind(Predicted_Redoubt,S2.3_pred[1:30])
Predicted_Redoubt <- cbind(Predicted_Redoubt,S2.4_pred[1:30])
Predicted_Redoubt <- cbind(Predicted_Redoubt,S3.1_pred[1:30])

#Plot Linear Model comparison of Measured and Predicted Electrical Values
plot(log10(S2.3_pred[1:30]^2), log10(RedoubtData$ElectDur))
plot(log10(S2.4_pred[1:30]^2), log10(RedoubtData$NLS))
plot(log10((S3.1_pred[1:30]^-1)^3), log10(RedoubtData$ltg))

plot(S1.1_pred)
plot(S2.2_pred)

#ttest to compare
t.test((S2.3_pred[1:30]^2), (RedoubtData$ElectDur))
t.test((S2.4_pred[1:30]^2), (RedoubtData$NLS))
t.test(((S3.1_pred[1:30]^-1)^3), (RedoubtData$ltg))
#all of them fail - p is sig so different distributions means not a good model...?



#determine transforms based on Redoubt data ----
symbox(VASR)
symbox(Infra)
symbox(Seis)
symbox(Sdur)
symbox(Idur)

powerTransform(VASR)
powerTransform(Infra)
powerTransform(Seis)
powerTransform(Sdur)
powerTransform(Idur)

RL_VASR <- log10(VASR)
RL_Infra <- log10(Infra)
RL_Seis <- log10(Seis)
RL_Sdur <- log10(Sdur)
RL_Idur <- log10(Idur)

# First Round Redoubt model----
Rm.lin_EA <- lm(RedoubtData$ElectDur~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur)
Rm.lin_NLS <- lm(RedoubtData$NLS~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur)
Rm.lin_ltgAll <- lm(RedoubtData$ltg~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur)

summary(Rm.lin_EA)
summary(Rm.lin_NLS)
summary(Rm.lin_ltgAll)

#RegSubsets to look at potentially removing a variable to lower AIC ----

Reg_EA <- regsubsets(RedoubtData$ElectDur~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur, data=RedoubtData, method=c("exhaustive", "backward", "forward", "seqrep"))
Reg_NLS <- regsubsets(RedoubtData$NLS~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur, data=RedoubtData, method=c("exhaustive", "backward", "forward", "seqrep"))
Reg_LTG <- regsubsets(RedoubtData$ltg~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur, data=RedoubtData, method=c("exhaustive", "backward", "forward", "seqrep"))

summary(Reg_EA)
summary(Reg_NLS)
summary(Reg_LTG)

plot(Reg_EA, scale = "adjr2", main = "Adjusted R^2")
plot(Reg_NLS, scale = "adjr2", main = "Adjusted R^2")
plot(Reg_LTG, scale = "adjr2", main = "Adjusted R^2")

#Stepwise Selection ----

REAnothing <- lm(RedoubtData$ElectDur~1)
REAfullmod <- lm(RedoubtData$ElectDur~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur)
REA_bothways = step(REAnothing, list(lower=formula(REAnothing),upper=formula(REAfullmod)), direction="both")

RNLSnothing <- lm(RedoubtData$NLS~1)
RNLSfullmod <- lm(RedoubtData$NLS~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur)
RNLS_bothways = step(RNLSnothing, list(lower=formula(RNLSnothing),upper=formula(RNLSfullmod)), direction="both")

RLTGnothing <- lm(RedoubtData$ltg~1)
RLTGfullmod <- lm(RedoubtData$ltg~RL_VASR+RL_Infra+RL_Seis+RL_Sdur+RL_Idur)
RLTG_bothways = step(RLTGnothing, list(lower=formula(RLTGnothing),upper=formula(RLTGfullmod)), direction="both")
