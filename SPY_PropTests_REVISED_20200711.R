####This file, using data exclusively from the iShares S&P tracking product 'SPY,'
####calculates the pvalues for the the difference of means tests
####WHERE one data vector tested is (Delta_tDayPost | Delta_rDayPrior is positive)
####AND the other data vector tested is (Delta_tDayPost | Delta_rDayPrior is negative)
####WHERE r,t each range.

####This file additionally calculates the proportion of days that were negative r days prior
#### AND greater than .5% t days post, against the days where SPY was simply negative r days prior.

####We will use a double loop to generate both pieces of data for each r,t, and store
####the data we find in two matrtices, one for each query.

SPY_Historical <- read.csv("SPY_19930129_20190508.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

###################################
###################################
n <- length(SPY_Historical[[1]])
#n is easy.

SPY_Historical <- rename(SPY_Historical,"AdjClose" = "Adj.Close")
#Getting rid of the "." that Yahoo spits out.

SPY_Historical$AbsChange <- 0
SPY_Historical$Delta <- 0

###################################

#Some edited out code below :)

# SPY_Historical$upForDay <- ifelse(SPY_Historical$AbsChange >= 0,1,0)
# #Creating a new variable "upForDay"
# #IF: NFLX went up, this gets 1, otherwise it gets 0.
# #****This is a very important variable because it creates the boolean string that
# #****(pretty much) IS the logical representation of the stock market.


SPY_Historical$AbsChange_3DayPrior <- 0
SPY_Historical$AbsChange_4DayPrior <- 0
SPY_Historical$AbsChange_5DayPrior <- 0
SPY_Historical$AbsChange_6DayPrior <- 0
SPY_Historical$AbsChange_7DayPrior <- 0

#Creating/Initializing the very important "priors."
#These variables will contain information about what happened r days before the currentDay.
#The idea is to look at conditional probabilitites where we want to know about a "post" given a "prior" condition.
#The particular variables being created/initialized above will look at the absolute change
#of the stock price over a period. We'll also look at the percent gain over this "prior" r-day period.

SPY_Historical$Delta_3DayPrior <- 0
SPY_Historical$Delta_4DayPrior <- 0
SPY_Historical$Delta_5DayPrior <- 0
SPY_Historical$Delta_6DayPrior <- 0
SPY_Historical$Delta_7DayPrior <- 0

#Creating/initializing the priors containing percent gain over a prior r-day period.
#For this file, r = 3,4,5,6,7.

###################################################

SPY_Historical$AbsChange_5DayPost <- 0
SPY_Historical$AbsChange_6DayPost <- 0
SPY_Historical$AbsChange_7DayPost <- 0
SPY_Historical$AbsChange_8DayPost <- 0
SPY_Historical$AbsChange_9DayPost <- 0
SPY_Historical$AbsChange_10DayPost <- 0
SPY_Historical$AbsChange_11DayPost <- 0
SPY_Historical$AbsChange_12DayPost <- 0
SPY_Historical$AbsChange_13DayPost <- 0
SPY_Historical$AbsChange_14DayPost <- 0
SPY_Historical$AbsChange_15DayPost <- 0

#AND Creating/Initializing the very equally important "posts."
#These variables will contain information about what happened t days after the currentDay.

SPY_Historical$Delta_5DayPost <- 0
SPY_Historical$Delta_6DayPost <- 0
SPY_Historical$Delta_7DayPost <- 0
SPY_Historical$Delta_8DayPost <- 0
SPY_Historical$Delta_9DayPost <- 0
SPY_Historical$Delta_10DayPost <- 0
SPY_Historical$Delta_11DayPost <- 0
SPY_Historical$Delta_12DayPost <- 0
SPY_Historical$Delta_13DayPost <- 0
SPY_Historical$Delta_14DayPost <- 0
SPY_Historical$Delta_15DayPost <- 0

#Creating/initializing the posts containing percent gain over a period. These are the percentage change following a given day,
#and we therefore call them Delta_tDayPost, with "Delta" for change. In this file, t = 5,6,7,8,...,14,15.

###################################


for(i in 2:n){

	SPY_Historical$AbsChange[i] <- (SPY_Historical$AdjClose[i] - SPY_Historical$AdjClose[i-1])
	SPY_Historical$Delta[i] <- (SPY_Historical$AbsChange[i]/SPY_Historical$AdjClose[i-1])

#Calculating the absolute and percentage change for each trading day, beginning with previous day close and ending with current day close.
#'DTD_Change' is equivalent to 'Delta_1DayPost.'

#What follows is our calculating the Priors/Posts for the longer period of days.
#For priors, we'll look at r days before to currentDay for r = 3,4,5,6,7.
#For posts, we'll look at t day periods after currentDay for r = 5,6,7,8,...,14,15.


	if(i>=5){
		SPY_Historical$AbsChange_3DayPrior[i] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-4)]]))
		SPY_Historical$Delta_3DayPrior[i] <- (SPY_Historical$AbsChange_3DayPrior[[i]]/SPY_Historical$AdjClose[[(i-4)]])

	}
	
	
	if(i>=6){
		SPY_Historical$AbsChange_4DayPrior[i] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-5)]]))
		SPY_Historical$Delta_4DayPrior[i] <- (SPY_Historical$AbsChange_3DayPrior[[i]]/SPY_Historical$AdjClose[[(i-5)]])
		
			
	}	
	
	if(i>=7){
		SPY_Historical$AbsChange_5DayPrior[i] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-6)]]))
		SPY_Historical$Delta_5DayPrior[i] <- (SPY_Historical$AbsChange_5DayPrior[[i]]/SPY_Historical$AdjClose[[(i-6)]])
		
		SPY_Historical$AbsChange_5DayPost[i-5] <- SPY_Historical$AbsChange_5DayPrior[i]
		SPY_Historical$Delta_5DayPost[i-5] <- SPY_Historical$Delta_5DayPrior[i]	
		
	}
	
	if(i>=8){
		SPY_Historical$AbsChange_6DayPrior[i] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-7)]]))
		SPY_Historical$Delta_6DayPrior[i] <- (SPY_Historical$AbsChange_6DayPrior[[i]]/SPY_Historical$AdjClose[[(i-7)]])
		
		SPY_Historical$AbsChange_6DayPost[i-6] <- SPY_Historical$AbsChange_6DayPrior[i]
		SPY_Historical$Delta_6DayPost[i-6] <- SPY_Historical$Delta_6DayPrior[i]	
	
	}
	
	if(i>=9){
		SPY_Historical$AbsChange_7DayPrior[i] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-8)]]))
		SPY_Historical$Delta_7DayPrior[i] <- (SPY_Historical$AbsChange_7DayPrior[[i]]/SPY_Historical$AdjClose[[(i-8)]])
	
		SPY_Historical$AbsChange_7DayPost[i-7] <- SPY_Historical$AbsChange_7DayPrior[i]
		SPY_Historical$Delta_7DayPost[i-7] <- SPY_Historical$Delta_7DayPrior[i]	
	}
	
		
	if(i>=10){

		SPY_Historical$AbsChange_8DayPost[i-8] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-9)]]))
		SPY_Historical$Delta_8DayPost[i-8] <- SPY_Historical$AbsChange_8DayPost[[(i-8)]]/SPY_Historical$AdjClose[[(i-9)]]		
		
	}	
	
		
	if(i>=11){

		SPY_Historical$AbsChange_9DayPost[i-9] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-10)]]))
		SPY_Historical$Delta_9DayPost[i-9] <- SPY_Historical$AbsChange_9DayPost[[(i-9)]]/SPY_Historical$AdjClose[[(i-10)]]		
		
	}	
	
	if(i>=12){

		SPY_Historical$AbsChange_10DayPost[i-10] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-11)]]))
		SPY_Historical$Delta_10DayPost[i-10] <- SPY_Historical$AbsChange_10DayPost[[(i-10)]]/SPY_Historical$AdjClose[[(i-11)]]		
		
	}
	
	if(i>=13){

		SPY_Historical$AbsChange_11DayPost[i-11] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-12)]]))
		SPY_Historical$Delta_11DayPost[i-11] <- SPY_Historical$AbsChange_11DayPost[[(i-11)]]/SPY_Historical$AdjClose[[(i-12)]]		
		
	}	

	
	if(i>=14){

		SPY_Historical$AbsChange_12DayPost[i-12] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-13)]]))
		SPY_Historical$Delta_12DayPost[i-12] <- SPY_Historical$AbsChange_12DayPost[[(i-12)]]/SPY_Historical$AdjClose[[(i-13)]]		
		
	}	
	
		
	if(i>=15){

		SPY_Historical$AbsChange_13DayPost[i-13] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-14)]]))
		SPY_Historical$Delta_13DayPost[i-13] <- SPY_Historical$AbsChange_13DayPost[[(i-13)]]/SPY_Historical$AdjClose[[(i-14)]]		
		
	}	
	
		
	if(i>=16){

		SPY_Historical$AbsChange_14DayPost[i-14] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-15)]]))
		SPY_Historical$Delta_14DayPost[i-14] <- SPY_Historical$AbsChange_14DayPost[[(i-14)]]/SPY_Historical$AdjClose[[(i-15)]]		
		
	}	
	
		
	if(i>=17){

		SPY_Historical$AbsChange_15DayPost[i-15] <- ((SPY_Historical$AdjClose[[(i-1)]] - SPY_Historical$AdjClose[[(i-16)]]))
		SPY_Historical$Delta_15DayPost[i-15] <- SPY_Historical$AbsChange_15DayPost[[(i-15)]]/SPY_Historical$AdjClose[[(i-16)]]		
		
	}	
}

print(names(SPY_Historical))

priors <- c("Delta_3DayPrior","Delta_4DayPrior","Delta_5DayPrior","Delta_6DayPrior","Delta_7DayPrior")

posts <- c("Delta_5DayPost","Delta_6DayPost","Delta_7DayPost","Delta_8DayPost","Delta_9DayPost","Delta_10DayPost","Delta_11DayPost","Delta_12DayPost","Delta_13DayPost","Delta_14DayPost","Delta_15DayPost")

#I am creating these names so that I can create a double loop to test all combinations of priors and posts.
#This double loop is where and how we will populate our two matrices of data.

SPY_0change_pVals <- data.frame(row.names = priors)
SPY_.005delta0eta_pHats <- data.frame(row.names = priors)

#We now want to sample data from each half of (-1,-\eta) \union (\eta,1) and test the difference of the means of each sample.
#We'll also test how many times (Delta_tDayPost > delta) when Delta_tDayPost comes from the negative half.
#These statements take as arguments some numbers eta and delta, respectively.
#I'll leave delta and eta  hard coded here for now until I think of something more efficient.
#We'll start with something simple: eta is just 0 so that we test (-1,0) against (0,1), delta = .5%.

eta <- 0
delta <- .005


for(j in 1:length(posts)){
	#cols are posts, rows are priors.
	#j refers to col, i refers to row.
	curr_0 <- NULL
	#Initializing/creating the vector that stors all posts at priors[i].
	curr_pHat <- NULL
	for(i in 1:length(priors)){
		#We are testing the difference of means on the posts by asking
		#is there a sig dif between posts that come from the positive-prior and negative-prior slices of the data?
		a_0 <- SPY_Historical[posts[j]][SPY_Historical[priors[i]] > eta]
		b_0 <- SPY_Historical[posts[j]][SPY_Historical[priors[i]] < eta]
		#a_0 has positive posts, b_0 has negative ones	
		curr_0[i] <- (t.test(a_0,b_0))[[3]]
		#The [[3]] is what selects the actual p-value of the t.test.
		#We want to append this to curr, which we append to a data.frame.
		curr_pHat[i] <- length(b_0[b_0 > delta])/length(b_0)
		#pHat is length(conditional)/length(all), essentially.
		#For us, the "all" is b_0, and the conditional is components of b_0 that are bigger than delta = .005.
		#NOTE: I am using the name b_0 (and a_0 and so on) only because we have eta = 0.
			}
	SPY_0change_pVals <- cbind(SPY_0change_pVals,curr_0)
	SPY_.005delta0eta_pHats <- cbind(SPY_.005delta0eta_pHats,curr_pHat)
	
	#We just built the column (vector) curr, now we append it/them to our data.frame(s).

	
	curr_0 <- NULL
	curr_pHat <- NULL

	#Before starting our news j loop, or column loop, we will want to 
	#reset our curr vectors so that they can amass the same number of data points
	#for the next t and same r's.
	
		}
		
names(SPY_0change_pVals) <- posts
names(SPY_.005delta0eta_pHats) <- posts

print("*************************")
print("*************************")
print("pVals for difference in average posts given a positive versus a negative prior")
print("*************************")
print("*************************")
print(SPY_0change_pVals)

####The file has ended. Below are 'prior' attempts of mine to understand the problem at hand.

########################Now we are ready to test proportions :)########################

#Let \delta,\eta \in [0,1)
#We want to know P(\Delta_t > \delta \mid \Delta_{-r} < -\eta)
#for different values of t,r,\delta and \eta

#We'll start with \delta = .005, \eta = 0 i.e. -\eta = 0
#And let's just use t = 7, r = 5 since it seems familiar for some reason

#t = 7 is posts[3]
#r = 5 is priors[3]

#\{\Delta_7 \mid \Delta_{-5} < -0\} is SPY_Historical$Delta_7DayPost[SPY_Historical$Delta_5DayPrior < 0]
#\{\Delta_7 > .005 \mid \Delta_{-5} < -0\} is SPY_Historical$Delta_7DayPost[SPY_Historical$Delta_5DayPrior < 0][Delta_7DayPost > 0]

# a_75 <- SPY_Historical$Delta_7DayPost[SPY_Historical$Delta_5DayPrior < 0]
# pHat_75 <- length(a_75[a_75 > .005])/length(a_75)

