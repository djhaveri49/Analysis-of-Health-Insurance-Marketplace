#Choropleth map for Coinsurance  statewise
RawBenefitsFile  <-  read.csv('BenefitsCostSharing.csv', header = T, sep= ',', stringsAsFactors = F)
#cleaning teh data and converting to numeric variable 
Benefitsmorethan50coins <-  subset(RawBenefitsFile, RawBenefitsFile $CoinsInnTier1 != 'No Charge'  )
Benefitsmorethan50coins <-  subset(Benefitsmorethan50coins, Benefitsmorethan50coins $CoinsInnTier1 != 'No Charge after deductible'  )
Benefitsmorethan50coins <-  subset(Benefitsmorethan50coins, Benefitsmorethan50coins $CoinsInnTier1 != 'Not Applicable'  )
Benefitsmorethan50coins <-  subset(Benefitsmorethan50coins, Benefitsmorethan50coins $CoinsInnTier1 != '$0'  )

Benefitsmorethan50coins $CoinsInnTier1 <- as.factor(Benefitsmorethan50coins $CoinsInnTier1)
Benefitsmorethan50coins $CoinsInnTier1<- gsub('% Coinsurance after deductible', '', Benefitsmorethan50coins $CoinsInnTier1)
Benefitsmorethan50coins $CoinsInnTier1<- gsub('%', '', Benefitsmorethan50coins $CoinsInnTier1)

Benefitsmorethan50coins $CoinsInnTier1 <- as.numeric(Benefitsmorethan50coins $CoinsInnTier1)

summary(Benefitsmorethan50coins $CoinsInnTier1)
#coinsurances greater than 50 %
Benefitsmorethan50coins <-  subset(Benefitsmorethan50coins, Benefitsmorethan50coins $CoinsInnTier1 >=  50  )
dim(Benefitsmorethan50coins)
attach(Benefitsmorethan50coins)
library(ggplot2)
qplot(BusinessYear, data=Benefitsmorethan50coins, geom="bar", fill=StateCode, xlab = "Business Year", ylab="Number of Plans",
      main = "Health Insurers with more than 50 % COInsurance") 

detach(Benefitsmorethan50coins)