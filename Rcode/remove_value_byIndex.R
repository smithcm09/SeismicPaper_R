#the subset line removes the value at index 108 from the model
m108 <- lm(rate~hs, data=mar28, subset=c(-108))

#finding which values of cv (covratio) may be outliers using the conditions set by the powerpoint
cv[abs(cv)-1 > 6/308]

#gives confidence intervals for the model (95%)
confint(m1)
