#####This document calculates numeric changes in the value of the S&P 500
#####Changes over 1,2,3,5,10,20,50,100,200 days each are calculated
#####For every trading day since 01/03/1950
#####The document also works through an example
#####Meant to help understand how to build a matrix of matrices

###################################

detach()
GSPC_Historical <- read.csv("GSPC_MaxData_20190723.csv")
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(prob)

###################################
GSPC_N <- length(GSPC_Historical[[1]])
###Our statistical N

GSPC_Historical <- rename(GSPC_Historical,"AdjClose" = "Adj.Close")
####Reformatting the date into something that R can make sense of
GSPC_Historical$Date <- (as.Date(GSPC_Historical$Date, format = "%m/%d/%Y"))
#GSPC_Historical$Date <- (as.Date(GSPC_Historical$Date))


####We just don't need these variables from YahooFinance at this time
GSPC_Historical$Open <- NULL
GSPC_Historical$High <- NULL
GSPC_Historical$Low <- NULL
GSPC_Historical$Close <- NULL
#We don't need the close because we are using the AdjClose... always

#I am now adding in the 1-day-prior variable, which we'll just call "Delta_Prior"
#Remember that the 1-day-post is simply Delta, and thus it's already been calculated
GSPC_Historical$AbsChange <- 0
GSPC_Historical$Delta <- 0
GSPC_Historical$AbsChange_Prior <- 0
GSPC_Historical$Delta_Prior <- 0	

#Creating/Initializing the very important "priors" for t = 2,3,5,10,20,50,100,200
#These variables will contain information about what happened t days before the currentDay
#The idea is to look at conditional probabilitites, which I believe is a Bayesian approach
#The particular variables being created/initialized above will look at the absolute change
#of the stock price over a period

#First, we create/initialize the absolute change in value of the S&P over t days

GSPC_Historical$AbsChange_2DayPrior <- 0
GSPC_Historical$AbsChange_3DayPrior <- 0
GSPC_Historical$AbsChange_5DayPrior <- 0
GSPC_Historical$AbsChange_10DayPrior <- 0
GSPC_Historical$AbsChange_20DayPrior <- 0
GSPC_Historical$AbsChange_50DayPrior <- 0
GSPC_Historical$AbsChange_100DayPrior <- 0
GSPC_Historical$AbsChange_200DayPrior <- 0

#Now we create/initialize the priors containing percent gain over t days
#The order is chosen this way because in calculating the Delta's
#The AbsChange for the same day is used as the numerator of the fraction
#that is Delta

GSPC_Historical$Delta_2DayPrior <- 0
GSPC_Historical$Delta_3DayPrior <- 0
GSPC_Historical$Delta_5DayPrior <- 0
GSPC_Historical$Delta_10DayPrior <- 0
GSPC_Historical$Delta_20DayPrior <- 0
GSPC_Historical$Delta_50DayPrior <- 0
GSPC_Historical$Delta_100DayPrior <- 0
GSPC_Historical$Delta_200DayPrior <- 0

###################################################

GSPC_Historical$AbsChange_2DayPost <- 0
GSPC_Historical$AbsChange_3DayPost <- 0
GSPC_Historical$AbsChange_5DayPost <- 0
GSPC_Historical$AbsChange_10DayPost <- 0
GSPC_Historical$AbsChange_20DayPost <- 0
GSPC_Historical$AbsChange_50DayPost <- 0
GSPC_Historical$AbsChange_100DayPost <- 0
GSPC_Historical$AbsChange_200DayPost <- 0

#AND Creating/Initializing the very equally important "posts"
#These variables will contain information about what happened t days after the currentDay

GSPC_Historical$Delta_2DayPost <- 0
GSPC_Historical$Delta_3DayPost <- 0
GSPC_Historical$Delta_5DayPost <- 0
GSPC_Historical$Delta_10DayPost <- 0
GSPC_Historical$Delta_20DayPost <- 0
GSPC_Historical$Delta_50DayPost <- 0
GSPC_Historical$Delta_100DayPost <- 0
GSPC_Historical$Delta_200DayPost <- 0

###################################

#This is where we get to the meat of this file; computing Delta_t's
#The probabilities to be calculated will be called (WLOG) "Delta_[t]DayPost"
#Delta refers to the fact that the number computed is a percent change over time

for(i in 2:GSPC_N){
	#Can't calculate a prior for day1, so skip to day2 for cleanliness
	
	#Taking the opportunity to populate these variables
	#By my estimation, variables that look at DayOverDay need an interative process to populate
	GSPC_Historical$AbsChange[i] <- (GSPC_Historical$AdjClose[i] - GSPC_Historical$AdjClose[i-1])
	GSPC_Historical$Delta[i] <- (GSPC_Historical$AbsChange[i]/GSPC_Historical$AdjClose[i-1])
	######New Delta_Prior code here ]
	
	#The rest of this code will all go in the i-loop
	#We are populating the period-over-period change for
	#t = 2,3,5,10,20,50,100,200 days
	
	if(i>=3){
		GSPC_Historical$AbsChange_Prior[i] <- GSPC_Historical$AbsChange[i-1]
		GSPC_Historical$Delta_Prior[i] <- GSPC_Historical$Delta[i-1]
		#Delta happened on day 1
		#So since the Delta_Prior for day 2 would be what happened on day 1
		#we assign Delta[1] to Delta_Prior[2] 
	}
	
	if(i>=4){
		#we want the change that occurred in the 2 days prior to day i
		#that means we want to see how close(day i-1) relates to close(day i-3)
		#note that this isn't defined unless i>=4, since i can't be 0 or less
		GSPC_Historical$AbsChange_2DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-3)]]))
		GSPC_Historical$Delta_2DayPrior[i] <- (GSPC_Historical$AbsChange_2DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-3)]])
		
		GSPC_Historical$AbsChange_2DayPost[i-2] <- GSPC_Historical$AbsChange_2DayPrior[i]
		GSPC_Historical$Delta_2DayPost[i-2] <- GSPC_Historical$Delta_2DayPrior[i]	
	}
	
	if(i>=5){
		GSPC_Historical$AbsChange_3DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-4)]]))
		GSPC_Historical$Delta_3DayPrior[i] <- (GSPC_Historical$AbsChange_3DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-4)]])
		
		GSPC_Historical$AbsChange_3DayPost[i-3] <- GSPC_Historical$AbsChange_3DayPrior[i]
		GSPC_Historical$Delta_3DayPost[i-3] <- GSPC_Historical$Delta_3DayPrior[i]	
	}
	
	if(i>=7){
		GSPC_Historical$AbsChange_5DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-6)]]))
		GSPC_Historical$Delta_5DayPrior[i] <- (GSPC_Historical$AbsChange_5DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-6)]])
		
		GSPC_Historical$AbsChange_5DayPost[i-5] <- GSPC_Historical$AbsChange_5DayPrior[i]
		GSPC_Historical$Delta_5DayPost[i-5] <- GSPC_Historical$Delta_5DayPrior[i]	
	}
	
	if(i>=12){
		GSPC_Historical$AbsChange_10DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-11)]]))
		GSPC_Historical$Delta_10DayPrior[i] <- (GSPC_Historical$AbsChange_10DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-11)]])
		
		GSPC_Historical$AbsChange_10DayPost[i-10] <- GSPC_Historical$AbsChange_10DayPrior[i]
		GSPC_Historical$Delta_10DayPost[i-10] <- GSPC_Historical$Delta_10DayPrior[i]	
	}
	
	if(i>=22){
		GSPC_Historical$AbsChange_20DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-21)]]))
		GSPC_Historical$Delta_20DayPrior[i] <- (GSPC_Historical$AbsChange_20DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-21)]])
		
		GSPC_Historical$AbsChange_20DayPost[i-20] <- GSPC_Historical$AbsChange_20DayPrior[i]
		GSPC_Historical$Delta_20DayPost[i-20] <- GSPC_Historical$Delta_20DayPrior[i]	
	}
	
	if(i>=52){
		GSPC_Historical$AbsChange_50DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-51)]]))
		GSPC_Historical$Delta_50DayPrior[i] <- (GSPC_Historical$AbsChange_50DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-51)]])
		
		GSPC_Historical$AbsChange_50DayPost[i-50] <- GSPC_Historical$AbsChange_50DayPrior[i]
		GSPC_Historical$Delta_50DayPost[i-50] <- GSPC_Historical$Delta_50DayPrior[i]	
	}
	
	if(i>=102){
		
		GSPC_Historical$AbsChange_100DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-101)]]))
		GSPC_Historical$Delta_100DayPrior[i] <- (GSPC_Historical$AbsChange_100DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-101)]])
		
		GSPC_Historical$AbsChange_100DayPost[i-100] <- GSPC_Historical$AbsChange_100DayPrior[i]
		GSPC_Historical$Delta_100DayPost[i-100] <- GSPC_Historical$Delta_100DayPrior[i]	
	}
	
	if(i>=202){
		
		GSPC_Historical$AbsChange_200DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-201)]]))
		GSPC_Historical$Delta_200DayPrior[i] <- (GSPC_Historical$AbsChange_200DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-201)]])
		
		GSPC_Historical$AbsChange_200DayPost[i-200] <- GSPC_Historical$AbsChange_200DayPrior[i]
		GSPC_Historical$Delta_200DayPost[i-200] <- GSPC_Historical$Delta_200DayPrior[i]	
	}

}

####What comes below is the beginnnings of the work on the tessaract
####We will work towards the tessaract, or matrix of matrices, with a "warm-up example"
####The example we are working is 
####The finished product can be found in ProbMatrixCode_REVISED_20200626.R

S <- GSPC_Historical
####Changing name here to reflect that our data set
####is ultimately just one big sample space
S$probs <- 1/length(S$Date)
attach(S)

###More on sample spaces below from before this revision:
###8/3/2019
###We're going to think of the data set GSPC_Historical (S for ease of notation, essentially) 
###which contains data on each and every day of trading on the ^GSPC 
###as "every possible outcome" of an experiment, namely
###The experiment of observing the day-to-day change (in the adjusted closing price of the ^GSPC)
###of a random day of trading on the ^GSPC

###We are ultimately interested in quantifying our expectations of where stock prices change/will change over periods of time

########################################################################################################

#First, we work through a single table
#To warm up, we will calculate the uncoditional probability
#Prob(\Delta > k*.001) for k \in {0,1,2,...,100}
#For example, if k=52, the calculation being made iteratively in the code below
#would be Prob(dayOverDayChange[currDay] > .052)
#or in other words what is the likelihood that any given trading day
#saw a change from open to close of 5.2% in the value of the S&P 

myTable <- data.frame(k = 0:100)
#We'll initiate the count vector k so that we can iterate through a formula that uses a constant multiplier
myTable$distanceFromZero <- myTable$k*.001
#Adding the column that contains the desired percentage changes
#Which can be thought of as distances from zero


#In the following for loop, we'll be calculating probabilities
#that the change in S&P over t days is greater than each value of k
#We'll do everything in one big loop


for(i in 1:length(myTable$k)){
	
	#For each i
	#we'll initialize and populate for each particular range of days,
	#the Probability that the rDayPost is a percentage change larger
	#in magnitude that distanceFromZero[i]
	
	myTable$probs[i] <- Prob(subset(S,S$Delta > myTable$distanceFromZero[i]))
	myTable$totalNum[i] <- length(subset(S,S$Delta > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t2[i] <- Prob(subset(S,S$Delta_2DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t2[i] <- length(subset(S,S$Delta_2DayPost > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t3[i] <- Prob(subset(S,S$Delta_3DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t3[i] <- length(subset(S,S$Delta_3DayPost > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t5[i] <- Prob(subset(S,S$Delta_5DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t5[i] <- length(subset(S,S$Delta_5DayPost > myTable$distanceFromZero[i])$Date) 
	
	myTable$probs_t10[i] <- Prob(subset(S,S$Delta_10DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t5[i] <- length(subset(S,S$Delta_10DayPost > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t20[i] <- Prob(subset(S,S$Delta_20DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t20[i] <- length(subset(S,S$Delta_20DayPost > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t50[i] <- Prob(subset(S,S$Delta_50DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t50[i] <- length(subset(S,S$Delta_50DayPost > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t100[i] <- Prob(subset(S,S$Delta_100DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t100[i] <- length(subset(S,S$Delta_100DayPost > myTable$distanceFromZero[i])$Date)
	
	myTable$probs_t200[i] <- Prob(subset(S,S$Delta_200DayPost > myTable$distanceFromZero[i]))
	myTable$totalNum_t200[i] <- length(subset(S,S$Delta_200DayPost > myTable$distanceFromZero[i])$Date)
	
	###probs_tX contains the probabililty at each .001 step, that
	###any given XDayPost would be bigger than distanceFromZero[i]
}

########################################################################################################

plot(x = myTable$distanceFromZero ,y = myTable$probs, ylim = c(0,1),type = "l")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t2, type = "l",col = "darkred")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t3, type = "l",col = "darkgreen")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t5, type = "l",col = "darkblue")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t10, type = "l",col = "red")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t20, type = "l",col = "forestgreen")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t50, type = "l",col = "blue")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t100, type = "l",col = "pink")
lines(x = myTable$distanceFromZero ,y = myTable$probs_t200, type = "l",col = "chartreuse")


########################################################################################################

#####There is no more code--the file is done
#####The rest are past efforts at thinking through
#####The tessaract that was built in ProbMatrixCode_REVISED_20200626.R


#for each post t
	#for each distance d
		#calculate P(Delta_t > d)
		
# for(t in posts){
	# for(i in S$k){
		# print(Prob(subset(S,S[t] > i*.001)))
	# }
# }


#Given a prior r
	#for each prior-distance d_r
		#for each post t
			#for each post-distance d_t
				#calculate P(Delta_t > d_t|Delta_r < d_r)
				#this is the same thing as
				#P(Delta_t > d_t AND Delta_r < d_r)/P(Delta_r < d_r)


# r <- priors[1]		
# for(i in myTable$k){
	# for(t in posts)
		# for(j in myTable$k){
			# Prob(subset(S,((S[t] > j*.001) & (S[r] < i*.001 - .05))))/Prob(subset(S,S[r] < i*.001 - .05))
		# }
# }

#each matrix myTable_r_t
#has prior distance as rows
#and post distance as columns

#for each prior r
	#for each post t
		#for each prior distance d_r
			#for each post distance d_t
				#calculate p <- P(Delta_t > d_t | Delta_r < d_r)
				#store p in the current post column
				#add the post column to myTable_r_t

#ex
#start with 5DayPrior and 10DayPost fixed
#and create the matrix of probabilities for just this (dual) selection
#of prior and post time periods

# t <- posts[4]
# r <- priors[3]
# myTable_r5_t10 <- data.frame(row.names = 0:100)

# for(j in 0:100){
	# #rows are priors, cols are posts
	# #j refers to col, i refers to row
	# currCol_r5_t10 <- NULL
	# #creating the vector that stors all posts at priors[i]
	# for(i in 0:100){
		# #we are first slicing the data based on whether priors were in the set \left((-1,1) \setminus (-\alpha,\alpha)\right)
		# #for \alpha \in \{k/2 \mid k = 0,1,2,3,4}
		# #then, we test the difference of means on the posts, i.e. we ask
		# #is there a sig dif between posts that come from the positive and negative slices of the data?
		# # a_0 <- GSPC_Historical[posts[j]][GSPC_Historical[priors[i]] > 0]
		# # b_0 <- GSPC_Historical[posts[j]][GSPC_Historical[priors[i]] < 0]	
		# currCol_r5_t10[i+1] <- Prob(subset(S,((S[t] > j*.001) & (S[r] < i*.001 - .05))))/Prob(subset(S,(S[r] < i*.001 - .05)))
		# #the [[3]] is what selects the actual p-value of the t.test
		# #we want to append this to curr, which we append to a dataframe
			# }
	# myTable_r5_t10 <- cbind(myTable_r5_t10,currCol_r5_t10)
# }
# names(myTable_r5_t10) <- 0:100

#Great work
#Next time, we'll figure out how to make a matrix of matrices such as the one we just made
#This may involve playing with strings in R



############################
###The following code has been copied over to the "ProbMatrixCode" series

# priors <- c("Delta","Delta_2DayPrior","Delta_3DayPrior","Delta_5DayPrior","Delta_10DayPrior","Delta_20DayPrior","Delta_50DayPrior","Delta_100DayPrior","Delta_200DayPrior")

# posts <- c("Delta","Delta_2DayPost","Delta_3DayPost","Delta_5DayPost","Delta_10DayPost","Delta_20DayPost","Delta_50DayPost","Delta_100DayPost","Delta_200DayPost")

# intervals <- c(1,2,3,5,10,20,50,100,200)


# seedString <- paste("myTable",intervals,sep="_r")
# myTessaract <- data.frame
# for(word in seedString){
	# currWord <- (paste(word,intervals,sep="_t"))
	# myTessaract <- cbind(myTessaract,currWord)
# }
# myTessaract <- myTessaract[,2:(length(intervals)+1)]
#####myTessaract will be a data.frame with entries that are data.frames themselves
#####We are going to use a quadruple loop to populate myTessarct

#####
############################
#for r in priors
	#for t in posts
		#populate current data.frame:
		#for j in 0:100
			#for i in 0:100
				#calculate p = P(Delta_t > j*.001 | Delta_r < i*.001 - .05)
				#add p to the current probs column
			#add the current probs column to the current data.frame
		#add the current data.frame to myTessaract[r,t]
		#OR
		#add the current data.frame to the current data.frame column
		#add the current data.frame column to myTessaract
		
# for(r in 1:(length(intervals))){
	# for(t in 1:(length(intervals))){
		# #select current data frame
		# currTable <- data.frame
		# for(j in 0:100){
		# #rows are priors, cols are posts
		# #j refers to col, i refers to row
			# currCol <- NULL
			# for(i in 0:100){
				# #We are essentially calculating this (conditional) probability (100**2)**2 times
				# currCol[i+1] <- Prob(subset(S,((S[posts[t]] > j*.001) & (S[priors[r]] < i*.001 - .05))))/Prob(subset(S,(S[priors[r]] < i*.001 - .05)))
			# }
			# currTable <- cbind(currTable,currCol)
		# myTessaract[[r,t]] <- currTable
		# }
	# }
# }


#for r in priors
	#for t in posts
		#current data.frame gets a new blank data.frame
		#populate current data.frame:
		#for j in 0:100
			#for i in 0:100
				#calculate p = P(Delta_t > j*.001 | Delta_r < i*.001 - .05)
				#add p to the current probs column
			#add the current probs column to the current data.frame
		#assign myTessaract[[r,t]] <- current data.frame
		#OR
		#add the current data.frame to the current data.frame column
		#add the current data.frame column to myTessaract

