#find difference % between Ron and my numbers of NLS
Ron <- c(70090, 33668, 8083, 5170, 1720, 5675, 8346, 22, 4417, 160, 107, 362, 10)
Cass <- c(9394, 6248, 1837, 774, 1491, 243, 393, 469, 497, 38, 41, 90, 15)

sd(Ron)
sd(Cass)

mean(Ron)-min(Cass)/sd(Ron)
mean(Ron)-max(Cass)/sd(Ron)

#if 1 sd is 34% from the mean then even the bigest difference between Rons mean and Cassandras Minimum was only 53% of a sd or 18% difference
#therefore test +- 20% of the NLS on the model to see if the change in beta values falls outside already given standard errors

#Result - does not fall outside already given standard errors - up to a +- 20% change in the NLS does not change the outcome of the model
#NLS Test----
NLS <- USE_THIS_ElectricDF$NLS
x <- c(0.80, 0.82, 0.84, 0.86, 0.88, 0.90, 0.92, 0.94, 0.96, 0.98, 1.00, 1.02, 1.04, 1.06, 1.08, 1.10, 1.12, 1.14, 1.16, 1.18, 1.2)
z<-as.numeric(length(NLS))
zL<- length(NLS)
randNLS <-vector("numeric",zL)

beta = matrix(NA, 15000, ncol=5)
colnames(beta) = c("(Intercept)", "L_Seis", "L_Infra", "Sq_Sdur", "L_Seis:L_Infra")

for (i in 1:15000) {
for (j in 1:z) {
  y=(sample(x,1,replace=TRUE))
  randNLS[j] <- NLS[j]*y
}
  w <- lm(formula = sqrt(randNLS) ~ L_Seis * L_Infra + Sq_Sdur, subset = -c(590, 87, 141, 188, 9, 8, 594))
  coefs <- coef(w)
  beta[i,colnames(beta) %in% names(coefs)] <- coefs[names(coefs)%in% colnames(beta)]
}

#Summary Stats ----

Interaction_Beta <- beta[1:15000,5]
Sq_SeisDur_Beta <- beta[1:15000,4]

stat <- Interaction_Beta
stat <- Sq_SeisDur_Beta

hist(stat)
summary(stat)
sd(stat)

sd(stat)/mean(stat)

