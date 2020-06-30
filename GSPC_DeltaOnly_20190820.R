####This document reads in the performance data of the S&P from a chosen time interval
####And computes two statistics, the change in value from a previous close to a current close, or the AbsChange,
####And the percentage change between a previous close and a current close, or Delta

####Of equal significance, this document is read into by another document, 'NSRGYMerged8_NormClose_REVISED_20200627.R'

#The dataset comes straight from Yahoo Finance
GSPC_Historical <- read.csv("GSPC_20020730_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
GSPC_N <- length(GSPC_Historical[[1]])

GSPC_Historical <- rename(GSPC_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of
#GSPC_Historical$Date <- (as.Date(GSPC_Historical$Date, format = "%m/%d/%y"))
GSPC_Historical$Date <- (as.Date(GSPC_Historical$Date))


####We just don't need these variables at this time
GSPC_Historical$Open <- NULL
GSPC_Historical$High <- NULL
GSPC_Historical$Low <- NULL
####We will come back to them, for sure
GSPC_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always


GSPC_Historical$AbsChange <- 0
GSPC_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####After we do all the usual work with the AdjClose


####This is really what we care most about, getting the deltas from the AdjClose's
####Once we do this, we can use *these* deltas to back into generating the NormClose
for(i in 2:GSPC_N){
	GSPC_Historical$AbsChange[i] <- (GSPC_Historical$AdjClose[i] - GSPC_Historical$AdjClose[i-1])
	GSPC_Historical$Delta[i] <- (GSPC_Historical$AbsChange[i]/GSPC_Historical$AdjClose[i-1])
}

# for(i in 2:GSPC_N){
	# GSPC_Historical$NormClose[i] <- GSPC_Historical$NormClose[i-1]*(1 + GSPC_Historical$Delta[i])
# }

# plot(GSPC_Historical$NormClose, type = "l")
