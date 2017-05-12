#Market coverage yearwise and statewise
RawPlanattrFile  <-  read.csv('PlanAttributes.csv', header = T, sep= ',', stringsAsFactors = T)

counts <- table(RawPlanattrFile $MarketCoverage,RawPlanattrFile $BusinessYear,
                dnn=c("Churn", "Market Coverage"))
counts
sumtable <- addmargins(counts, FUN = sum)
sumtable
# Overlayed bar chart
barplot(counts,
        legend = rownames(counts),
        col = c("blue", "red"),
        ylim = c(0, 40000),
        ylab = "Count",
        xlab = "Business Year",
        main = "Comparison Bar Chart:
        Market coverage  Proportions by Business Year")
box(which = "plot",
    lty = "solid",
    col="black")