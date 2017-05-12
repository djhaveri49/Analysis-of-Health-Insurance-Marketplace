#Regression model for Number of benefits vs Rate
RawBenefitsFile  <-  read.csv('BenefitsCostSharing.csv', header = T, sep= ',', stringsAsFactors = F)
RawBenefitsFile $StandardComponentId <- as.factor(RawBenefitsFile $StandardComponentId)
summary(RawBenefitsFile $StandardComponentId)
no_of_benefitsperplan <- aggregate(x = RawBenefitsFile, 
                               by = list(unique.values = RawBenefitsFile $StandardComponentId), 
                               FUN = length)
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
rateperplan <-  data.frame(RawRateFile $PlanId, RawRateFile $IndividualRate)
names(rateperplan)
names(rateperplan) <- c("planid","rate")
rateperplan <- subset(rateperplan , rateperplan $rate < 9000)

no_of_benefitsperplan <- data.frame(no_of_benefitsperplan $unique.values,no_of_benefitsperplan $BenefitName  )
names(no_of_benefitsperplan)
names(no_of_benefitsperplan) <- c("planid","noofbenefits")
rateperplanmean <- aggregate(x = rateperplan $rate, 
                             by = list(unique.values = rateperplan $planid), 
                             FUN = mean)
names(rateperplanmean) <- c("planid","rate")
#Merge Two files together 
ratevsbenefits <- merge(rateperplanmean,no_of_benefitsperplan)
noofben <- ratevsbenefits $noofbenefits
rate <- ratevsbenefits $rate 
plot(noofben ,rate)
#Regression model of rate vs number of benefits 
lr1 <- lm(rate ~ noofben)
abline(lr1)
pred.confidence <- predict(lr1,
                           data.frame(noofben = 100),
                           interval = "confidence")
pred.prediction <- predict(lr1,
                           data.frame(noofben = 100),
                           interval = "prediction")
pred.confidence
pred.prediction
summary(lr1)
