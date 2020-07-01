####This document reads in the performance data of the Dow Industrial from a chosen time interval
####and computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####and the percentage change between a previous close and a current close, or Delta.

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'.

#The dataset comes straight from Yahoo Finance.
DJI_Historical <- read.csv("DJI_20010102_20190820.csv")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
DJI_N <- length(DJI_Historical[[1]])

DJI_Historical <- rename(DJI_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of
#DJI_Historical$Date <- (as.Date(DJI_Historical$Date, format = "%m/%d/%y")).
DJI_Historical$Date <- (as.Date(DJI_Historical$Date))


####We just don't need these variables at this time.
DJI_Historical$Open <- NULL
DJI_Historical$High <- NULL
DJI_Historical$Low <- NULL
####We will come back to them, for sure.
DJI_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always.


DJI_Historical$AbsChange <- 0
DJI_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####after we do all the usual work with the AdjClose.


####This is really what we care most about, getting the deltas from the AdjClose's.
####Once we do this, we can use *these* deltas to back into generating the NormClose.
for(i in 2:DJI_N){
	DJI_Historical$AbsChange[i] <- (DJI_Historical$AdjClose[i] - DJI_Historical$AdjClose[i-1])
	DJI_Historical$Delta[i] <- (DJI_Historical$AbsChange[i]/DJI_Historical$AdjClose[i-1])
}

# for(i in 2:DJI_N){
	# DJI_Historical$NormClose[i] <- DJI_Historical$NormClose[i-1]*(1 + DJI_Historical$Delta[i])
# }

# plot(DJI_Historical$NormClose, type = "l")
