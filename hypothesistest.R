#to extract from rate file plan id ,individual rate,business year
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
ratefile <- data.frame(RawRateFile $BusinessYear ,RawRateFile $PlanId , RawRateFile $IndividualRate)
ratefile <- subset(ratefile , ratefile $RawRateFile.IndividualRate < 6000)
names(ratefile) <- c("businessyear","planid","Rate")
ratefile2014 <- subset(ratefile , ratefile $businessyear == "2014")
ratefile2014 <- ratefile2014[,c(2,3)]
ratefile2015 <- subset(ratefile , ratefile $businessyear == "2015")
ratefile2015 <- ratefile2015[,c(2,3)]

ratefile2016 <- subset(ratefile , ratefile $businessyear == "2016")
ratefile2016 <- ratefile2016[,c(2,3)]



#to extract from plan attr file plan ids and flag for medical files 
RawPlanattrFile  <-  read.csv('PlanAttributes.csv', header = T, sep= ',', stringsAsFactors = F)
planattrfile <- data.frame(RawPlanattrFile $BusinessYear, RawPlanattrFile $StandardComponentId ,RawPlanattrFile $DentalOnlyPlan )
planattrfile <- subset(planattrfile ,planattrfile $RawPlanattrFile.DentalOnlyPlan == "No")
names(planattrfile) <- c("businessyear2","planid","Dentalplanonly")
planattrfile <- data.frame(planattrfile $planid ,planattrfile $Dentalplanonly )
names(planattrfile) <- c("planid","Dentalplanonly")

#merge the medical plans only

ratemedicalplans2014 <- merge(ratefile2014,planattrfile)
sample2014ratemean <- mean(ratemedicalplans2014 $Rate)



ratemedicalplans2015 <- merge(ratefile2015,planattrfile)
sample2015ratemean <- mean(ratemedicalplans2015 $Rate)

ratemedicalplans2016 <- merge(ratefile2016,planattrfile)
sample2016ratemean <- mean(ratemedicalplans2016 $Rate)


ratesallyear <- c(ratemedicalplans2014[,2] , ratemedicalplans2015[,2] , ratemedicalplans2016[,2])
ratesallyearmean <- mean(ratesallyear)


#t-test for 2014 sample data and population for means

mean.test2014 <- t.test(x= ratemedicalplans2014 $Rate,
                        mu=ratesallyearmean,
                        conf.level= 0.95)
mean.test2014$statistic
mean.test2014$p.value
mean.test2014$conf.int

#t-test for 2015  sample data and population for means

mean.test2015 <- t.test(x= ratemedicalplans2015 $Rate,
                        mu=ratesallyearmean,
                        conf.level= 0.95)
mean.test2015$statistic
mean.test2015$p.value
mean.test2015$conf.int


#t-test for 2016 sample data and population for means

mean.test2016 <- t.test(x= ratemedicalplans2016 $Rate,
                        mu=ratesallyearmean,
                        conf.level= 0.95)
mean.test2016$statistic
mean.test2016$p.value
mean.test2016$conf.int




