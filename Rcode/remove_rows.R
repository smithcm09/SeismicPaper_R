#remove rows of a datafram based on contents
#make a copy of the dataframe for safety
test <- ElectricOnlyDF
#use subset command to remove rows
test2 <- subset(test, !test$maxinfraDur == "-Inf")
test3 <- subset(test2, !test2$maxseisDur == "-Inf")
test4 <- subset(test3, !test3$VASR_AB == "NaN")
test5 <- subset(test4, !test4$OverallABmedSeis == "NaN")
test6 <- subset(test5, !test5$OverallABmedInfra == "NaN")
test7 <- subset(test6, !test6$CRF_yn == "y-caveat")
test8 <- subset(test7, !test7$CRF_yn == "0")
test9 <- subset(test8, !test8$CRF_yn == " ")


#replace original DF with new and keep old for records
ElctricOnlyDF_WithInf <- ElectricOnlyDF
ElectricOnlyDF <- test9

#make a copy of the dataframe for safety
test <- ElectricOnlyDF_withNLS1
#use subset command to remove rows

test <- ElectricOnlyDF_withNLS1
test2 <- subset(test, !test$NLS == "0")
ElectricOnlyDF_missingNLS0 <- test2

###################################
test <- USE_THIS_ElectricDF
#use subset command to remove rows

#remove outlier rows determined to be bad data in 2nd level
test <- test[-c(590,87,141,188,9,8,594),]

test2 <- subset(test, !test$ltgAll == "0")
DF_onlyLtgRows <- test2


