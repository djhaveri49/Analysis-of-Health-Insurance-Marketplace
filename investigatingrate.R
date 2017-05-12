#Cleasing of Individual Rate
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
dim(RawRateFile)
RawRateFile$countrate <- 1
RawRateFile$IndividualRate <-as.factor(RawRateFile$IndividualRate) 

aggrerate <- aggregate(RawRateFile $countrate , list(RawRateFile $IndividualRate), count)
aggreratenew <- aggrerate[order(-aggrerate $x),] 
summary(aggrerate $Group.1)
aggreratenew$rate <-as.numeric(aggrerate $Group.1) 
boxplot(RawRateFile $IndividualRate)