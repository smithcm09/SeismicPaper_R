
#find maximum duration in infra/seis for each event in each subgroup of CRF noCRF and noElectrical ----
attach(CRFonlyDF)
df_seisdur_CRF <- data.frame(AHE_Duration,AHZ_Duration,AHN_Duration,BHE_Duration,BHZ_Duration,BHN_Duration)
df_seisdur_CRF$max<-apply(X=df_seisdur_CRF, MARGIN=1, na.rm=TRUE, FUN=max)

df_infradur_CRF <- data.frame(AB1_Duration,AB2_Duration,AB3_Duration,BB1_Duration,BB2_Duration,BB3_Duration)
df_infradur_CRF$max<-apply(X=df_infradur_CRF, MARGIN=1, na.rm=TRUE, FUN=max)

detach(CRFonlyDF)

attach(NoCRFonlyDF)
df_seisdur_NoCRF <- data.frame(AHE_Duration,AHZ_Duration,AHN_Duration,BHE_Duration,BHZ_Duration,BHN_Duration)
df_seisdur_NoCRF$max<-apply(X=df_seisdur_NoCRF, MARGIN=1, na.rm=TRUE, FUN=max)

df_infradur_NoCRF <- data.frame(AB1_Duration,AB2_Duration,AB3_Duration,BB1_Duration,BB2_Duration,BB3_Duration)
df_infradur_NoCRF$max<-apply(X=df_infradur_NoCRF, MARGIN=1, na.rm=TRUE, FUN=max)
detach(NoCRFonlyDF)

attach(NoElectricalDF)
df_seisdur_NoElectrical <- data.frame(AHE_Duration,AHZ_Duration,AHN_Duration,BHE_Duration,BHZ_Duration,BHN_Duration)
df_seisdur_NoElectrical$max<-apply(X=df_seisdur_NoElectrical, MARGIN=1, na.rm=TRUE, FUN=max)

df_infradur_NoElectrical <- data.frame(AB1_Duration,AB2_Duration,AB3_Duration,BB1_Duration,BB2_Duration,BB3_Duration)
df_infradur_NoElectrical$max<-apply(X=df_infradur_NoElectrical, MARGIN=1, na.rm=TRUE, FUN=max)
detach(NoElectricalDF)

#add maxvalues to original dataframes ----
CRFonlyDF$maxseisDur <- df_seisdur_CRF$max
CRFonlyDF$maxinfraDur <- df_infradur_CRF$max

NoCRFonlyDF$maxseisDur <- df_seisdur_NoCRF$max
NoCRFonlyDF$maxinfraDur <- df_infradur_NoCRF$max

NoElectricalDF$maxseisDur <- df_seisdur_NoElectrical$max
NoElectricalDF$maxinfraDur <- df_infradur_NoElectrical$max

#t.test durations of seismic/infrasound signals between CRF and noCRF and noElectrical event groups ----
#infrasound durations
Boxplot(CRFonlyDF$maxinfraDur)
Boxplot(NoCRFonlyDF$maxinfraDur)
Boxplot(NoElectricalDF$maxinfraDur)

t.test(CRFonlyDF$maxinfraDur, NoCRFonlyDF$maxinfraDur)
t.test(CRFonlyDF$maxinfraDur, NoElectricalDF$maxinfraDur)
t.test(NoCRFonlyDF$maxinfraDur, NoElectricalDF$maxinfraDur)

#seismic durations
Boxplot(CRFonlyDF$maxseisDur)
Boxplot(NoCRFonlyDF$maxseisDur)
Boxplot(NoElectricalDF$maxseisDur)

t.test(CRFonlyDF$maxseisDur, NoCRFonlyDF$maxseisDur)
t.test(CRFonlyDF$maxseisDur, NoElectricalDF$maxseisDur)
t.test(NoCRFonlyDF$maxinfraDur, NoElectricalDF$maxinfraDur)

