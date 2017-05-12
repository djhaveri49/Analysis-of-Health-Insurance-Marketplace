#Issuer plans with more than 10 ehb benefits statewise
RawBenefitsFile  <-  read.csv('BenefitsCostSharing.csv', header = T, sep= ',', stringsAsFactors = F)
ehbbenefits<- subset(RawBenefitsFile , RawBenefitsFile $IsEHB == "Yes")
dim(ehbbenefits)[1]
ehbbenefits$benefit  <- 1
planswithnobenefits <-  aggregate(ehbbenefits $benefit, list(ehbbenefits $StandardComponentId),sum )
planplusissuer <- data.frame(ehbbenefits $IssuerId ,ehbbenefits $StandardComponentId)
names(planplusissuer) <- c("issuerid","planid")
planplusissuerunique <- unique(planplusissuer)
names(planswithnobenefits) <- c("planid","noofbenefits")
issuerplusbenefits <- merge(planswithnobenefits,planplusissuerunique)
issuerwithnobenefitsmean <-  aggregate(issuerplusbenefits $noofbenefits, list(issuerplusbenefits $issuerid),mean )
names(issuerwithnobenefitsmean) <- c("issuer","noofbenefitsmean")
issuerwithnobenefitsmean $issuer <- as.factor(issuerwithnobenefitsmean $issuer)
issuerwithstates <- data.frame(ehbbenefits $IssuerId, ehbbenefits $StateCode)
issuerwithstatesunique <- unique(issuerwithstates)
names(issuerwithstatesunique) <- c("issuer","statecode")
stateplusbenefits <- merge(issuerwithnobenefitsmean,issuerwithstatesunique)
stateplusbenefitsmean <-  aggregate(stateplusbenefits $noofbenefitsmean , list(stateplusbenefits $statecode),mean )
names(stateplusbenefitsmean) <- c("statecode","benefits")
library(ggplot2)

a <- ggplot(stateplusbenefitsmean, aes(x=reorder(factor(stateplusbenefitsmean $statecode ),stateplusbenefitsmean $benefits), y=stateplusbenefitsmean $benefits)) + 
  geom_bar(stat="identity") +
  ggtitle("Mean No of ehb benefits vs state ") +
  labs(x="State",y="Mean No of EHB benfits")  

library(plotly)

a + scale_fill_gradient(low="blue", high="red")
