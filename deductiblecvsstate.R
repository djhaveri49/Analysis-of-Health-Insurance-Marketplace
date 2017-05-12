#Choropleth map for deductible statewise
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
#create subset of the 2015 dataset
rate2015 <- subset(RawRateFile, BusinessYear == "2015")
rate2014 <- subset(RawRateFile, BusinessYear == "2014")
rate2016 <- subset(RawRateFile, BusinessYear == "2016")
RawBenefitsFile  <-  read.csv('BenefitsCostSharing.csv', header = T, sep= ',', stringsAsFactors = F)
benefits2015<- subset(RawBenefitsFile, BusinessYear == "2015")
benefits2016<- subset(RawBenefitsFile, BusinessYear == "2016")
benefits2014<- subset(RawBenefitsFile, BusinessYear == "2014")

RawPlanattrFile  <-  read.csv('PlanAttributes.csv', header = T, sep= ',', stringsAsFactors = F)
planattr2015<- subset(RawPlanattrFile, BusinessYear == "2015")
planattr2016<- subset(RawPlanattrFile, BusinessYear == "2016")
planattr2014<- subset(RawPlanattrFile, BusinessYear == "2014")

#clean the deductible column 
MDEHBFile <- subset(RawPlanattrFile, RawPlanattrFile $TEHBDedInnTier1Individual != '') # remove the dental plans
MDEHBFile $TEHBDedInnTier1Individual<- gsub('Not Applicable', 0, MDEHBFile $TEHBDedInnTier1Individual) # make the not applicable fields as zero values
MDEHBFile $TEHBDedInnTier1Individual <- gsub('\\$', '', MDEHBFile $TEHBDedInnTier1Individual)
MDEHBFile $TEHBDedInnTier1Individual <- gsub(',', '', MDEHBFile $TEHBDedInnTier1Individual)
MDEHBFile $TEHBDedInnTier1Individual <- as.numeric(MDEHBFile $TEHBDedInnTier1Individual)
install.packages("dplyr")
suppressMessages(library(dplyr))
MDEHBFile $StateCode <- as.factor(MDEHBFile $StateCode)
#function from https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/
#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
  
}
install.packages("choroplethr")

install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(ggplot2)




stateDeductibles <-  aggregate(MDEHBFile $TEHBDedInnTier1Individual, list(MDEHBFile $StateCode), mean)
stateDeductibles$region<-stateFromLower(stateDeductibles$Group.1)
stateDeductibles$value <- stateDeductibles$x
choro = StateChoropleth$new(stateDeductibles)

choro$title = "Average Deductible  for Medical and Drug EHB Benefit Plans"
choro$set_num_colors(1)
myPalette <- colorRampPalette(brewer.pal(9, "Reds"))
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Deductible", colours = myPalette(9),na.value = "black")
choro$render()


