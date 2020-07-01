
####This document reads in the performance data of the ishares Preferred Stock and Income ETF from a chosen time interval
####and computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####and the percentage change between a previous close and a current close, or Delta.

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'.

#The dataset comes straight from Yahoo Finance.
PFF_Historical <- read.csv("PFF_20070330_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
PFF_N <- length(PFF_Historical[[1]])

PFF_Historical <- rename(PFF_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of.
#PFF_Historical$Date <- (as.Date(PFF_Historical$Date, format = "%m/%d/%y"))
PFF_Historical$Date <- (as.Date(PFF_Historical$Date))


####We just don't need these variables at this time.
PFF_Historical$Open <- NULL
PFF_Historical$High <- NULL
PFF_Historical$Low <- NULL
####We will come back to them, for sure.
PFF_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always.


PFF_Historical$AbsChange <- 0
PFF_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####after we do all the usual work with the AdjClose.


####This is really what we care most about, getting the deltas from the AdjClose's.
####Once we do this, we can use *these* deltas to back into generating the NormClose.
for(i in 2:PFF_N){
	PFF_Historical$AbsChange[i] <- (PFF_Historical$AdjClose[i] - PFF_Historical$AdjClose[i-1])
	PFF_Historical$Delta[i] <- (PFF_Historical$AbsChange[i]/PFF_Historical$AdjClose[i-1])
}

# for(i in 2:PFF_N){
	# PFF_Historical$NormClose[i] <- PFF_Historical$NormClose[i-1]*(1 + PFF_Historical$Delta[i])
# }

# plot(PFF_Historical$NormClose, type = "l")
