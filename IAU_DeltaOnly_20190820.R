####This document reads in the performance data of the ishareas Gold ETF from a chosen time interval
####And computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####And the percentage change between a previous close and a current close, or Delta

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'

#The dataset comes straight from Yahoo Finance
IAU_Historical <- read.csv("IAU_20070411_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


###################################
###################################
IAU_N <- length(IAU_Historical[[1]])

IAU_Historical <- rename(IAU_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of
#IAU_Historical$Date <- (as.Date(IAU_Historical$Date, format = "%m/%d/%y"))
IAU_Historical$Date <- (as.Date(IAU_Historical$Date))


####We just don't need these variables at this time
IAU_Historical$Open <- NULL
IAU_Historical$High <- NULL
IAU_Historical$Low <- NULL
####We will come back to them, for sure
IAU_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always


IAU_Historical$AbsChange <- 0
IAU_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####After we do all the usual work with the AdjClose


####This is really what we care most about, getting the deltas from the AdjClose's
####Once we do this, we can use *these* deltas to back into generating the NormClose
for(i in 2:IAU_N){
	IAU_Historical$AbsChange[i] <- (IAU_Historical$AdjClose[i] - IAU_Historical$AdjClose[i-1])
	IAU_Historical$Delta[i] <- (IAU_Historical$AbsChange[i]/IAU_Historical$AdjClose[i-1])
}

# for(i in 2:IAU_N){
	# IAU_Historical$NormClose[i] <- IAU_Historical$NormClose[i-1]*(1 + IAU_Historical$Delta[i])
# }

# plot(IAU_Historical$NormClose, type = "l")
