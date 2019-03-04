# Single predictor/regressor pair

# To fold section - option-command-L
# To un-fold section - shift-option-command-L
# Replace L with O for expand or collapse all of the sections

# NLS ~ MaxVASR----
# Set up the variables----
#take log of the MaxVASR variable
LMV <- log(MaxVASR)
#take all of the observations that are inf or nan and make na
LMV[is.infinite(LMV) | is.nan(LMV) ] <- NA
summary(LMV)
#look only at events where NLS>0
NLS_g0 <- NLS[NLS>0]
LMV_g0nls <- LMV[NLS>0]
summary(NLS_g0)
summary(LMV_g0nls)
length(NLS_g0)
length(LMV_g0nls)


# Make a model----
m1a <- lm(NLS_g0~LMV_g0nls)

# Plot the model with regression line----
plot(LMV_g0nls,NLS_g0)
abline(m1a)

#check summary of the model
summary(m1a)

# Run regression diagnostics on the model----
model_used <- m1a

residualPlots(model_used)
ncvTest(model_used)
spreadLevelPlot(model_used)
inverseResponsePlot(model_used)
crPlots(model_used)


# NLS ~ MaxVASR

# NLS ~ VASR_AB


# NLS ~ VASR_AB----
# Set up the variables----
#take log of the MaxVASR variable
LVAB <- log(VASR_AB)
#take all of the observations that are inf or nan and make na
LVAB[is.infinite(LVAB) | is.nan(LVAB) ] <- NA
summary(LVAB)
#look only at events where NLS>0
NLS_g0 <- NLS[NLS>0]
LVAB_g0nls <- LVAB[NLS>0]
summary(NLS_g0)
summary(LVAB_g0nls)
length(NLS_g0)
length(LVAB_g0nls)


# Make a model----
m1b <- lm(NLS_g0~LVAB_g0nls)

# Plot the model with regression line----
plot(LVAB_g0nls,NLS_g0)
abline(m1b)

#check summary of the model
summary(m1a)

# Run regression diagnostics on the model----
model_used <- m1b

residualPlots(model_used)
ncvTest(model_used)
spreadLevelPlot(model_used)
inverseResponsePlot(model_used)
crPlots(model_used)

