#multicollinearity
#in 3D want a stable coffe table top
#high multicollinearity = high standard error on the Beta values because the slope of the plane can vary A LOT - unstable table top
#use varience inflation factor - vif() on the model - if value is above 4 then alarm bells should go off, if more than 1 have vifs above 3 then they might be related to each other


#incremental F-Tests
#k is the number of predictors
#to do
#make regression of all data
#make regression of limited model
#anova of second,first models
#if signif than can reject the null and assume that the tested variable(s) is/are important


#Impacts of violating assumptions - slide
#assumptions of linearity, constant variance, and normality


##BoxCox transforms
invTranPlot(y~x)