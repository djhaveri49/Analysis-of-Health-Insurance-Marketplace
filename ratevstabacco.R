#non tabacco users rate
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
hist(RawRateFile $IndividualRate)
max(Individual $IndividualRate)
Individual <- subset(RawRateFile, RawRateFile $IndividualRate < 6000)
hist(Individual $IndividualRate)
summary(Individual $IndividualRate)


BusinessYear <- as.factor(BusinessYear)
summary(BusinessYear)
install.packages("plotly")
library(plotly)
plot_ly(ggplot2::diamonds, y = IndividualRate, color = BusinessYear, type = "box")
detach(Individual)

#rates for tabacco users
hist(RawRateFile $IndividualTobaccoRate)
summary(Individual $IndividualRate)

IndividualTobacco <- subset(RawRateFile, RawRateFile $Tobacco != 'No Preference')
IndividualTobacco $BusinessYear <- as.factor(IndividualTobacco $BusinessYear)
plot_ly(ggplot2::diamonds, y = IndividualTobacco $IndividualTobaccoRate  , color = IndividualTobacco $BusinessYear, type = "box")


#plan rates  vary across states
Individual $StateCode <- as.factor(Individual $StateCode)
stateIndividual <-  aggregate(Individual $IndividualRate, list(Individual $StateCode), mean)
stateIndividual$region<-stateIndividual$Group.1
stateIndividual$value <- stateIndividual$x
 a <- ggplot(stateIndividual, aes(x=reorder(factor(stateIndividual $region),stateIndividual $value), y=stateIndividual $value)) + 
   geom_bar(stat="identity") +
   ggtitle("Mean Individual Rate vs state ") +
   labs(x="State",y="Mean Individual Rate") 
  
 
                        a + scale_colour_brewer()