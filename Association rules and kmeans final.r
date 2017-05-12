#Association Rules and Kmeans
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
RawPlanAttributesFile<- read.csv('PlanAttributes.csv', header = T, sep= ',', stringsAsFactors = T)
planfile <- data.frame(RawPlanAttributesFile $StandardComponentId ,RawPlanAttributesFile $MetalLevel ,RawPlanAttributesFile $MarketCoverage, RawPlanAttributesFile $OutOfCountryCoverage) 
planfile <- planfile[!duplicated(planfile),]
#Code for cleaning the age column
rateage <- subset(RawRateFile, RawRateFile $Age != "Family Option")
rateage $Age <- gsub('0-20', '20', rateage $Age)
rateage $Age <- gsub('65 and over', '65', rateage $Age)
rateage $Age <- as.numeric(rateage $Age)
rateage <- subset(rateage, rateage $IndividualRate < 6000)
ragefilter <- data.frame(rateage$Age,rateage$IndividualRate,rateage $PlanId)
ragefilter <- ragefilter[!duplicated(ragefilter),]
names(ragefilter)
names(planfile)
#Giving names to the columns of the ragefilter as required 
names(ragefilter) <- c("age","rate","planid")

#Giving names to the columns of the planfile as required
names(planfile) <- c("planid","metallevel","marketcoverage","OutofCountryCoverage")

#Merging the datasets planfile and ragefilter
mergeplanrate <- merge(planfile,ragefilter)

#Taking only the rate and the age column from the mergeplanrate 
rateage1  <-  data.frame(mergeplanrate $rate , mergeplanrate $age)

#Taking only the planid, metallevel, marketcoverage and OutofCountryCoverage from mergeplanrate
planid<- data.frame(mergeplanrate $planid, mergeplanrate $metallevel, mergeplanrate $marketcoverage, mergeplanrate $OutofCountryCoverage)
planid$ID<- seq.int(nrow(planid))

#kmeans of the dataset rateage1 for the columns age and rate and also viewing there
#size and cluster
km<- kmeans(rateage1,centers = 2)
km $centers
km $size
library(cluster)

#Plotting the graph of the clusters for every column with respect
# to every other column in the rateage1 dataset
with(rateage1, pairs(rateage1, col=c(1:2)[km$cluster]))

#Adds a new column to clusterrateage mentioning the cluster value
clusterrateage<- cbind(rateage1,clusterNum=km$cluster)
clusterrateage$ID<- seq.int(nrow(clusterrateage))

#Converts clusterNum column to factor
clusterrateage $clusterNum <- as.factor(clusterrateage $clusterNum)

#Taking all the records which are in cluster 1 into rateagec1
rateagec1 <- subset(clusterrateage, clusterrateage $clusterNum == 1 )

#Merging rateagec1 and planid into mergec1
mergec1 <-  merge(rateagec1,planid)

#Converting metallevel, market coverage and OutofCountryCoverage as factors
mergec1 $mergeplanrate.metallevel <- as.factor(mergec1 $mergeplanrate.metallevel)
mergec1 $mergeplanrate.marketcoverage <- as.factor(mergec1 $mergeplanrate.marketcoverage)
mergec1 $mergeplanrate.OutofCountryCoverage <- as.factor(mergec1 $mergeplanrate.OutofCountryCoverage)

#deleting the columns from 1-5 in the mergec1 file
merge2<- mergec1[-c(1:5)]
#Using the apriori algorithm on merge2 dataset
rules <- apriori(merge2)
library("arules")
#Inspecting the association rules
inspect(sort(rules))
summary(rules)
library(arulesViz)
#Plotting the graph of the association rules
plot(rules,method="graph",interactive=TRUE,shading=NA)