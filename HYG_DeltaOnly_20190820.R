####This document reads in the performance data of the ishares High Yield Corporate Bond Fund ETF from a chosen time interval
####And computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####And the percentage change between a previous close and a current close, or Delta

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'

#The dataset comes straight from Yahoo Finance
HYG_Historical <- read.csv("HYG_20070411_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
HYG_N <- length(HYG_Historical[[1]])

HYG_Historical <- rename(HYG_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of
#HYG_Historical$Date <- (as.Date(HYG_Historical$Date, format = "%m/%d/%y"))
HYG_Historical$Date <- (as.Date(HYG_Historical$Date))


####We just don't need these variables at this time
HYG_Historical$Open <- NULL
HYG_Historical$High <- NULL
HYG_Historical$Low <- NULL
####We will come back to them, for sure
HYG_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always


HYG_Historical$AbsChange <- 0
HYG_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####After we do all the usual work with the AdjClose


####This is really what we care most about, getting the deltas from the AdjClose's
####Once we do this, we can use *these* deltas to back into generating the NormClose
for(i in 2:HYG_N){
	HYG_Historical$AbsChange[i] <- (HYG_Historical$AdjClose[i] - HYG_Historical$AdjClose[i-1])
	HYG_Historical$Delta[i] <- (HYG_Historical$AbsChange[i]/HYG_Historical$AdjClose[i-1])
}

# for(i in 2:HYG_N){
	# HYG_Historical$NormClose[i] <- HYG_Historical$NormClose[i-1]*(1 + HYG_Historical$Delta[i])
# }

# plot(HYG_Historical$NormClose, type = "l")
