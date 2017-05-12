#Clustering followed by classification of plans into metal level 
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
RawPlanAttributesFile<- read.csv('PlanAttributes.csv', header = T, sep= ',', stringsAsFactors = T)
planfile <- data.frame(RawPlanAttributesFile $StandardComponentId ,RawPlanAttributesFile $MetalLevel ,RawPlanAttributesFile $DentalOnlyPlan) 
planfile <- planfile[!duplicated(planfile),]
rateage <- subset(RawRateFile, RawRateFile $Age != "Family Option")
rateage $Age <- gsub('0-20', '20', rateage $Age)
rateage $Age <- gsub('65 and over', '65', rateage $Age)
rateage $Age <- as.numeric(rateage $Age)
rateage <- subset(rateage, rateage $IndividualRate < 6000)
ragefilter <- data.frame(rateage$Age,rateage$IndividualRate,rateage $PlanId)
ragefilter <- ragefilter[!duplicated(ragefilter),]
names(ragefilter)
names(planfile)
names(ragefilter) <- c("age","rate","planid")
names(planfile) <- c("planid","metallevel","dentalonlyplan")
mergeplanrate <- merge(planfile,ragefilter)
rateage1  <-  data.frame(mergeplanrate $rate , mergeplanrate $age)
#max(rateage $IndividualRate)
#rateage1$ID<- seq.int(nrow(rateage1))
planid<- data.frame(mergeplanrate $planid, mergeplanrate $metallevel, mergeplanrate $dentalonlyplan)
planid$ID<- seq.int(nrow(planid))
#remove(RawRateFile)
#Clustering based on rate and usa with two clusters
km<- kmeans(rateage1,centers = 2)
km$cluster
km
km$centers
clusterrateage<- cbind(rateage1,clusterNum=km$cluster)
clusterrateage$ID<- seq.int(nrow(clusterrateage))
clusterrateage $clusterNum <- as.factor(clusterrateage $clusterNum)
rateagec1 <- subset(clusterrateage, clusterrateage $clusterNum == 1 )
rateagec1 <- rateagec1[,-3]
mergec1 <-  merge(rateagec1,planid)
mergec1 $mergeplanrate.metallevel <- as.factor(mergec1 $mergeplanrate.metallevel)
mergec1 $mergeplanrate.dentalonlyplan <- as.factor(mergec1 $mergeplanrate.dentalonlyplan)
mergec1$rate.z <- (mergec1$mergeplanrate.rate - mean(mergec1$mergeplanrate.rate))/sd(mergec1$mergeplanrate.rate)
mergec1$age.z <- (mergec1$mergeplanrate.age - mean(mergec1$mergeplanrate.age))/sd(mergec1$mergeplanrate.age)
install.packages(c("rpart", "rpart.plot", "C50"))
library("rpart"); library("rpart.plot"); library("C50")
#Modelling classsification  with target as metal level and predictor variables as rate and dentalonlyplan
cartfit <- rpart(mergec1 $mergeplanrate.metallevel ~ mergec1 $mergeplanrate.rate + mergec1 $mergeplanrate.dentalonlyplan,
                 data = mergec1,
                 method = "class")
print(cartfit)
rpart.plot(cartfit)
#Building the confusion matrix
pred = predict(cartfit , type = "class")

table(pred)
table(pred,mergec1 $mergeplanrate.metallevel )