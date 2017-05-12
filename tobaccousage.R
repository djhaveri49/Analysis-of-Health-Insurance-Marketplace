#Tobacco Usage statewise 
RawRateFile  <-  read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
#to find the tobacco plans with states 
PlanswithTabacco <- subset(RawRateFile, RawRateFile $Tobacco == "Tobacco User/Non-Tobacco User")

suppressMessages(library(dplyr))
PlanswithTabacco $StateCode <- as.factor(PlanswithTabacco $StateCode)
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
PlanswithTabacco$tobaccofactor <- 1

library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(ggplot2)


stateplanstobacco <-  aggregate(PlanswithTabacco $tobaccofactor, list(PlanswithTabacco $StateCode), sum)
stateplanstobacco$region<-stateFromLower(stateplanstobacco$Group.1)
stateplanstobacco$value <- stateplanstobacco$x
stplantob  <-  data.frame(stateplanstobacco$region,stateplanstobacco$value)
names(stplantob) <- c("region","value")

state_choropleth(stplantob, title="Plans with tobacco preference by state ", legend="Plans with Tobacco ")

