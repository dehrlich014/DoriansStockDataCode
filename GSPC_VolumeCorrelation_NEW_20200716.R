####This document looks at the S&P performance data from the beginning of 2000 to last week
####and computes the correlation coefficients for Delta_rDayPrior and Delta_tDayPost *each with trading volume*
####on the day of, for r,t \in {1,2,3,4,5,7,10}.
####The r^2 figures will be stored in a 2 X 7 data.frame.

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(prob)


GSPC_Historical <- read.csv("GSPC_20000103_20200703.csv")

GSPC_Historical <- rename(GSPC_Historical,"AdjClose" = "Adj.Close")
#Reformatting the date into something that R can make sense of.
GSPC_Historical$Date <- (as.Date(GSPC_Historical$Date, format = "%m/%d/%Y"))

#Unneeded variables will be set to NULL.
GSPC_Historical$High <- NULL
GSPC_Historical$Low <- NULL
GSPC_Historical$Close <- NULL

n <- length(GSPC_Historical$Date)

#Initializing the priors and the posts.
#rDayPrior for r = 1,2,3,4,5,7,10.
#tDayPost for t = 1,2,3,4,5,7,10.

GSPC_Historical$AbsChange_1DayPrior <- 0
GSPC_Historical$AbsChange_2DayPrior <- 0
GSPC_Historical$AbsChange_3DayPrior <- 0
GSPC_Historical$AbsChange_4DayPrior <- 0
GSPC_Historical$AbsChange_5DayPrior <- 0
GSPC_Historical$AbsChange_7DayPrior <- 0
GSPC_Historical$AbsChange_10DayPrior <- 0


###################################################

GSPC_Historical$Delta_1DayPrior <- 0
GSPC_Historical$Delta_2DayPrior <- 0
GSPC_Historical$Delta_3DayPrior <- 0
GSPC_Historical$Delta_4DayPrior <- 0
GSPC_Historical$Delta_5DayPrior <- 0
GSPC_Historical$Delta_7DayPrior <- 0
GSPC_Historical$Delta_10DayPrior <- 0


###################################################

GSPC_Historical$AbsChange <- 0
GSPC_Historical$AbsChange_2DayPost <- 0
GSPC_Historical$AbsChange_3DayPost <- 0
GSPC_Historical$AbsChange_4DayPost <- 0
GSPC_Historical$AbsChange_5DayPost <- 0
GSPC_Historical$AbsChange_7DayPost <- 0
GSPC_Historical$AbsChange_10DayPost <- 0


###################################################

GSPC_Historical$Delta <- 0
GSPC_Historical$Delta_2DayPost <- 0
GSPC_Historical$Delta_3DayPost <- 0
GSPC_Historical$Delta_4DayPost <- 0
GSPC_Historical$Delta_5DayPost <- 0
GSPC_Historical$Delta_7DayPost <- 0
GSPC_Historical$Delta_10DayPost <- 0

###################################

for(i in 3:n){
	
	#r,t = 1,2,3,4,5,7,10
	#First we tackle the 1-day posts and priors.
	GSPC_Historical$AbsChange_1DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-2)]]))
	GSPC_Historical$Delta_1DayPrior[i] <- (GSPC_Historical$AbsChange_1DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-2)]])
	
	GSPC_Historical$AbsChange[i] <- (GSPC_Historical$AdjClose[i] - GSPC_Historical$AdjClose[i-1])
	GSPC_Historical$Delta[i] <- (GSPC_Historical$AbsChange[i]/GSPC_Historical$AdjClose[i-1])
	
	########################################

	
	if(i>=4){
		GSPC_Historical$AbsChange_2DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-3)]]))
		GSPC_Historical$Delta_2DayPrior[i] <- (GSPC_Historical$AbsChange_1DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-3)]])
		
		GSPC_Historical$AbsChange_2DayPost[i-2] <- GSPC_Historical$AbsChange_2DayPrior[i]
		GSPC_Historical$Delta_2DayPost[i-2] <- GSPC_Historical$Delta_2DayPrior[i]	
		
	}
	#Idea is this:
	#The change "2 days pror" is the difference between the close yesterday, and the the close two days before that.
	#For this purpose, that would be saying the difference between AdjClose[i-1] AND AdjClose[i-3].
	#Since we calculate changes moving forward in time, we would take AdjClose[i-1] - AdjClose[i-3].
	#Notice that we must have an (i-3)rd day to avoid an error, so we need i to be at least 4 for this to work.
	
	########################################
	
	if(i>=5){
		GSPC_Historical$AbsChange_3DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-4)]]))
		GSPC_Historical$Delta_3DayPrior[i] <- (GSPC_Historical$AbsChange_3DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-4)]])
		
		GSPC_Historical$AbsChange_3DayPost[i-3] <- GSPC_Historical$AbsChange_3DayPrior[i]
		GSPC_Historical$Delta_3DayPost[i-3] <- GSPC_Historical$Delta_3DayPrior[i]	
	
	}
	
	########################################
	
	if(i>=6){
		GSPC_Historical$AbsChange_4DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-5)]]))
		GSPC_Historical$Delta_4DayPrior[i] <- (GSPC_Historical$AbsChange_4DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-5)]])
	
		GSPC_Historical$AbsChange_4DayPost[i-4] <- GSPC_Historical$AbsChange_4DayPrior[i]
		GSPC_Historical$Delta_4DayPost[i-4] <- GSPC_Historical$Delta_4DayPrior[i]	
	}
	
	########################################
	
	if(i>=7){
		GSPC_Historical$AbsChange_5DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-6)]]))
		GSPC_Historical$Delta_5DayPrior[i] <- (GSPC_Historical$AbsChange_5DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-6)]])
		
		GSPC_Historical$AbsChange_5DayPost[i-5] <- GSPC_Historical$AbsChange_5DayPrior[i]
		GSPC_Historical$Delta_5DayPost[i-5] <- GSPC_Historical$Delta_5DayPrior[i]	
		
	}
	

	
	######################################## 
	
	if(i>=9){
		GSPC_Historical$AbsChange_7DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-8)]]))
		GSPC_Historical$Delta_7DayPrior[i] <- (GSPC_Historical$AbsChange_7DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-8)]])
	
		GSPC_Historical$AbsChange_7DayPost[i-7] <- GSPC_Historical$AbsChange_7DayPrior[i]
		GSPC_Historical$Delta_7DayPost[i-7] <- GSPC_Historical$Delta_7DayPrior[i]	
	}
	
		########################################
	
	if(i>=12){
		GSPC_Historical$AbsChange_10DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-11)]]))
		GSPC_Historical$Delta_10DayPrior[i] <- (GSPC_Historical$AbsChange_10DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-11)]])
		
		GSPC_Historical$AbsChange_10DayPost[i-10] <- GSPC_Historical$AbsChange_10DayPrior[i]
		GSPC_Historical$Delta_10DayPost[i-10] <- GSPC_Historical$Delta_10DayPrior[i]	
	
	}

}

################################################################################

#Now we'll begin working on finding and storing the correlation coefficinets
#that are generated when taking the correlation between volume and each prior and post delta.

S <- GSPC_Historical[12:n,]
attach(S)

VolCoeffs <- data.frame(x1 = c(0,0))
#Here's where we'll store the desired data.

priors <- c("Delta_1DayPrior","Delta_2DayPrior","Delta_3DayPrior","Delta_4DayPrior","Delta_5DayPrior","Delta_7DayPrior","Delta_10DayPrior")
posts <- c("Delta","Delta_2DayPost","Delta_3DayPost","Delta_4DayPost","Delta_5DayPost","Delta_7DayPost","Delta_10DayPost")
#We'll use a single loop to compute each iDayPrior and iDayPost.
#Notice that unfortunately the spelling must be entirely correct or else.

for(i in 1:length(priors)){
	#We want to calculate a post and prior correlation each.
	#We can build our matrix by appending colummns of length 2,
	#1 for prior, 1 for post.
	curr <- 0
	curr[1] <- cor(S[priors[i]],Volume)
	curr[2] <- cor(S[posts[i]],Volume)
	VolCoeffs <- cbind(VolCoeffs,curr)
}

VolCoeffs$x1 <- NULL
colnames(VolCoeffs) <- c(1,2,3,4,5,7,10)
row.names(VolCoeffs) <- c("priors","posts")

print(VolCoeffs)

########################################

#The below code was used to QA the priors.
#Essentially, if these charts don't get flatter as t days gets higher, something's wrong.

# print(names(GSPC_Historical))
# print(S$Date[n-11])
# print(GSPC_Historical$Date[n])

# plot(density(GSPC_Historical$Delta))
# lines(density(GSPC_Historical$Delta_2DayPost), col = "darkred")
# lines(density(GSPC_Historical$Delta_3DayPost), col = "darkgreen")
# lines(density(GSPC_Historical$Delta_4DayPost), col = "darkblue")
# lines(density(GSPC_Historical$Delta_5DayPost), col = "pink")
# lines(density(GSPC_Historical$Delta_7DayPost), col = "green")
# lines(density(GSPC_Historical$Delta_10DayPost), col = "cyan")