# Equal-width Binning
PlanAttributes <- read.csv("C:/Users/sujit/Desktop/KDD/health-insurance-marketplace-release-2016-01-20-15-52-37/health-insurance-marketplace/PlanAttributes.csv", header=FALSE)
View(PlanAttributes)
PlanAttributes = PlanAttributes[-1,]
# Put 0 in case of empty field values
PlanAttributes$V121[PlanAttributes$V121==""]<- 0
PlanAttributes_new <- as.numeric(PlanAttributes$V121)
range_plandata <- max(PlanAttributes_new) - min(PlanAttributes_new) + 1
# Get the sample size of the variable
n <- length(PlanAttributes_new)
#Declare number of bins and bin indicator
nbins <- 3
whichbin <- c(rep(0, n))
binwidth <- range_plandata/nbins
# run for loop
for (i in 1:nbins) {
  for (j in 1:n) {
    if((i-1)*binwidth < PlanAttributes_new[j] &&
       PlanAttributes_new[j] <= (i)*binwidth)
      whichbin[j] <- i
  }
}
whichbin