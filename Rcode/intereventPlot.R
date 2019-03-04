attach(ordered)

ordered$DTime <- ordered$a
ordered$Electrical_Factor <- ordered$b

#loop to calculate differences in datetimes between concurrent events
for (i in 1:1514) {

j <- i+1

ordered$datediff[j]<-ordered$DTime[j]-ordered$DTime[i]

}

a<-plot(datediff[c(-1,-1515)],Electrical_Factor[c(-1)])
hist(datediff,breaks=100)
Boxplot(log10(datediff[c(-1,-1515)])~Electrical_Factor[c(-1)], notch=TRUE, id.n=0)
t.test((datediff[c(-1,-1515)])~Electrical_Factor[c(-1)])

x <- datediff[c(-1,-2,-4,-377,-309)]
y <- Electrical_Factor[c(-1,-2,-4,-377,-309)]
Boxplot(log10(x)~y, notch=TRUE, id.n=3)
Boxplot(chron(x)~y, notch=TRUE, id.n=3)
t.test(x~y)

blah <- data.frame(x,y)


c<- datediff[c(-1,-1515)]
summary(c)

## making ordered set
a <- Binary_DataFrame$DTime
b <- Binary_DataFrame$Electrical_Factor
testing <- data.frame(a,b)
ordered <- testing[do.call(order,testing),]

