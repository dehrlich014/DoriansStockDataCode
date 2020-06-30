SHY_Historical <- read.csv("SHY_20020730_20190820.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
#Loadin up the data and calling the needed packages

###################################
###################################
SHY_N <- length(SHY_Historical[[1]])

SHY_Historical <- rename(SHY_Historical,"AdjClose" = "Adj.Close")

####Reformatting the date into something that R can make sense of
#SHY_Historical$Date <- (as.Date(SHY_Historical$Date, format = "%m/%d/%y"))
SHY_Historical$Date <- (as.Date(SHY_Historical$Date))


####We just don't need these variables at this time
SHY_Historical$Open <- NULL
SHY_Historical$High <- NULL
SHY_Historical$Low <- NULL
####We will come back to them, for sure
SHY_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always


SHY_Historical$AbsChange <- 0
SHY_Historical$Delta <- 0
####I'll put the new NormClose variable here, and then come back to it
####After we do all the usual work with the AdjClose


####This is really what we care most about, getting the deltas from the AdjClose's
####Once we do this, we can use *these* deltas to back into generating the NormClose
for(i in 2:SHY_N){
	SHY_Historical$AbsChange[i] <- (SHY_Historical$AdjClose[i] - SHY_Historical$AdjClose[i-1])
	SHY_Historical$Delta[i] <- (SHY_Historical$AbsChange[i]/SHY_Historical$AdjClose[i-1])
}

# for(i in 2:SHY_N){
	# SHY_Historical$NormClose[i] <- SHY_Historical$NormClose[i-1]*(1 + SHY_Historical$Delta[i])
# }

# plot(SHY_Historical$NormClose, type = "l")