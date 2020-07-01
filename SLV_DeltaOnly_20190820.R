
####This document reads in the performance data of the ishares Silver ETF from a chosen time interval
####and computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####and the percentage change between a previous close and a current close, or Delta.

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'.

#The dataset comes straight from Yahoo Finance.
SLV_Historical <- read.csv("SLV_20060428_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
SLV_N <- length(SLV_Historical[[1]])

SLV_Historical <- rename(SLV_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of.
#SLV_Historical$Date <- (as.Date(SLV_Historical$Date, format = "%m/%d/%y"))
SLV_Historical$Date <- (as.Date(SLV_Historical$Date))


####We just don't need these variables at this time.
SLV_Historical$Open <- NULL
SLV_Historical$High <- NULL
SLV_Historical$Low <- NULL
####We will come back to them, for sure.
SLV_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always.


SLV_Historical$AbsChange <- 0
SLV_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####after we do all the usual work with the AdjClose.


####This is really what we care most about, getting the deltas from the AdjClose's.
####Once we do this, we can use *these* deltas to back into generating the NormClose.
for(i in 2:SLV_N){
	SLV_Historical$AbsChange[i] <- (SLV_Historical$AdjClose[i] - SLV_Historical$AdjClose[i-1])
	SLV_Historical$Delta[i] <- (SLV_Historical$AbsChange[i]/SLV_Historical$AdjClose[i-1])
}

# for(i in 2:SLV_N){
	# SLV_Historical$NormClose[i] <- SLV_Historical$NormClose[i-1]*(1 + SLV_Historical$Delta[i])
# }

# plot(SLV_Historical$NormClose, type = "l")
