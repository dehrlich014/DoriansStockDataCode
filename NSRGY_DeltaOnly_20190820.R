####This document reads in the performance data of Nestle S.A. from a chosen time interval
####and computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####and the percentage change between a previous close and a current close, or Delta.

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'.

#The dataset comes straight from Yahoo Finance.
NSRGY_Historical <- read.csv("NSRGY_20010102_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
NSRGY_N <- length(NSRGY_Historical[[1]])

NSRGY_Historical <- rename(NSRGY_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of.
#NSRGY_Historical$Date <- (as.Date(NSRGY_Historical$Date, format = "%m/%d/%y"))
NSRGY_Historical$Date <- (as.Date(NSRGY_Historical$Date))


####We just don't need these variables at this time.
NSRGY_Historical$Open <- NULL
NSRGY_Historical$High <- NULL
NSRGY_Historical$Low <- NULL
####We will come back to them, for sure
NSRGY_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always.


NSRGY_Historical$AbsChange <- 0
NSRGY_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####after we do all the usual work with the AdjClose.


####This is really what we care most about, getting the deltas from the AdjClose's.
####Once we do this, we can use *these* deltas to back into generating the NormClose.
for(i in 2:NSRGY_N){
	NSRGY_Historical$AbsChange[i] <- (NSRGY_Historical$AdjClose[i] - NSRGY_Historical$AdjClose[i-1])
	NSRGY_Historical$Delta[i] <- (NSRGY_Historical$AbsChange[i]/NSRGY_Historical$AdjClose[i-1])
}

# for(i in 2:NSRGY_N){
	# NSRGY_Historical$NormClose[i] <- NSRGY_Historical$NormClose[i-1]*(1 + NSRGY_Historical$Delta[i])
# }

# plot(NSRGY_Historical$NormClose, type = "l")
