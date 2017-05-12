#Rate vs age 
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
rateage <- subset(RawRateFile, RawRateFile $Age != "Family Option")
rateage $Age <- gsub('0-20', '20', rateage $Age)
rateage $Age <- gsub('65 and over', '65', rateage $Age)
rateage $Age <- as.numeric(rateage $Age)
summary(rateage $Age)

rateage <- subset(rateage, rateage $IndividualRate < 6000)
hist(rateage $IndividualRate)
#Z-score standardization 
zscore.rate <- (rateage $IndividualRate- mean(rateage $IndividualRate))/sd(rateage $IndividualRate)
zscore.age <- (rateage $Age - mean(rateage $Age))/sd(rateage $Age)
summary(rateage $IndividualRate)
hist(rateage $Age)
#sampling data year wise 
rateage2014 <- subset(rateage, rateage $BusinessYear  == '2014')
rateage2015 <- subset(rateage, rateage $BusinessYear  == '2015')
rateage2016 <- subset(rateage, rateage $BusinessYear  == '2016')

plot(rateage $Age, rateage $IndividualRate)
lr <- lm(rateage $IndividualRate ~ rateage $Age)
abline(lr)
summary(lr)
#regression model for sample belonging to 2014
rate2014 <-rateage2014 $IndividualRate
age2014 <- rateage2014 $Age
plot(age2014, rate2014)
lr1 <- lm(rate2014  ~ age2014 )
abline(lr1)
pred.confidence1 <- predict(lr1,
                            data.frame(age2014=10) ,
                            interval = "confidence")
pred.prediction1 <- predict(lr1,
                            data.frame(age2014 = 10),
                            interval = "prediction")
pred.confidence1
pred.prediction1
summary(lr1)

#regression model for sample belonging to  year 2015
rate2015 <-rateage2015 $IndividualRate
age2015 <- rateage2015 $Age

plot(age2015, rate2015)

lr2 <- lm(rate2015 ~ age2015)
abline(lr2)
pred.confidence2 <- predict(lr2,
                            data.frame(age2015=10) ,
                            interval = "confidence")
pred.prediction2 <- predict(lr2,
                            data.frame(age2015 = 10),
                            interval = "prediction")
pred.confidence2
pred.prediction2
summary(lr2)


#regression model for sample belonging to  year 2016
rate2016 <-rateage2016 $IndividualRate
age2016 <- rateage2016 $Age 
plot(age2016,rate2016)
lr3 <- lm(rate2016 ~ age2016 )
abline(lr3)
summary(lr3)
pred.confidence3 <- predict(lr3,
                           data.frame(age2016=10) ,
                           interval = "confidence")
pred.prediction3 <- predict(lr3,
                           data.frame(age2016 = 10),
                           interval = "prediction")
pred.confidence3
pred.prediction3
summary(lr3)

mod1 <- lm(rateage $IndividualRate ~ rateage $Age)
abline(mod1)

summary(rateage)