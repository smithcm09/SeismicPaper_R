#Data Exploration for the individual variables
attach(ForRMatchedData_04032018)
Var2explore = HorzABmedSeis # <- change this to run with other response variables

#run summary of the data
summary(Var2explore)

#if min is 0 or negative add a start value
Var2explore <- Var2explore+0.01 # <-- if needed change this line or comment out

#plot the variable along index values
plot(Var2explore)
identify(Var2explore)

hist(Var2explore, breaks=50)
rug(Var2explore)

qqPlot(Var2explore)

#look for possible transformations
symbox(Var2explore)
powerTransform(Var2explore)

#redo with suggested transform
trans_Var2explore <- log(Var2explore) # <-- change this line to match suggested transform

plot(trans_Var2explore)
identify(trans_Var2explore)
hist(trans_Var2explore, breaks=50)
rug(trans_Var2explore)
qqPlot(trans_Var2explore)

