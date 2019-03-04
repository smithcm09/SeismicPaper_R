#Infra Energy vs duration corr----
plot(log10(DF_S1$OverallABmedInfra), sqrt(DF_S1$maxinfraDur))

vif(ModelS2.2)
vif(MS2.2)

Seve_Ques <- lm(log10(DF_S1$OverallABmedInfra) ~ sqrt(DF_S1$maxinfraDur))
summary(Seve_Ques)
abline(Seve_Ques)

#### ----
plot(log10(DF_S2$OverallABmedInfra), sqrt(DF_S2$maxinfraDur))

vif(ModelS2.2)
vif(MS2.2)

Steve_Ques <- lm(log10(DF_S2$OverallABmedInfra) ~ sqrt(DF_S2$maxinfraDur))
summary(Steve_Ques)
abline(Steve_Ques)
## ----

# Kinetic vs Electrical Energy ----
#values transferred from matlab code
My_KinEn <- c(750000000000000, 
              1910000000000.00, 
              311000000000000, 
              96100000000.0000, 
              1450000000.00000, 
              171000000000000, 
              30200000000000.0, 
              593018304, 
              8340000000.00000)
ElecEn <- c(1912.82188076459,
            402.967266556644,
            374.052990734995,
            157.603165394059,
            88.3718007506739,
            80.0233126613245,
            29.9323841252283,
            18.5295711251413,
            3.25794657145342)

EnReg <- lm(ElecEn~My_KinEn)
scatterplot(My_KinEn,ElecEn, log = "xy", smooth = FALSE,col = "black", cex = 2, xlab = "Kinetic Energy", ylab = "Electrical Energy")
summary(EnReg)
