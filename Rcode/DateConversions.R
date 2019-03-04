#converting datetime list into R-readable date times
#make sure chron package is loaded
#assign dtimes the variable containing the dates, make sure as a character value
dtimes = as.character(Binary_DataFrame$SIDateTime)
dtparts = t(as.data.frame(strsplit(dtimes,' ')))
row.names(dtparts) = NULL
#change format to fit whatever format the input date is in
#use this website for proper codes: https://www.stat.berkeley.edu/~s133/dates.html
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('d-month-y','h:m:s'))

Binary_DataFrame$DTime <- thetimes
