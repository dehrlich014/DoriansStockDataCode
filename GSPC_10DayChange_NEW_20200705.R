####This document looks at the performance data of the S&P since 2000
####and compoutes in one table, the probability that the next 10 days, i.e. Delta_10DayPost
####is greater than .005 given various performances fromm the previous r days.
####These "various performances" will come as a range, for example, between -1% and -.5%. 
#####Put another we, we will endeavor to know if the change in 10 days is greater than .5%
####given a number of r-day-prior conditions WHERE r = 10,20,50,75,100,150,200,250,400.

####Probabilities will be stored in a table that is indexed by
####prior day interval and top on the interval of change i.e. 0 in -.5% - 0.

####This table is being constructed a la carte (as oppoosed to a new tessaract).
####We think it's worth highlighting that the user can intuit these probabilities with a day-after-timeframe of that 
####the user ordinarily would think in. This is to say, we think people understand 10-day-timeframes and it's worth focusing on that.

#The first line in these files is traditionally a read into a file.
#Here, we're reading into a data file. Elsewhere, we've read into other R files.\\

#########********Current status:Need to put in a binning system for the distances of Delta_rDayPrior.

GSPC_Historical <- read.csv("GSPC_20000103_20200703.csv")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(prob)
#These are the standard packages I'll call to have what I need for coding.
#I particularly use dplyr and prob a lot.

GSPC_N <- length(GSPC_Historical[[1]])
GSPC_Historical <- rename(GSPC_Historical,"AdjClose" = "Adj.Close")

#Reformatting the date to something R-friendly.
GSPC_Historical$Date <- (as.Date(GSPC_Historical$Date, format = "%m/%d/%Y"))

#Dropping unneeded variables from YF and initializing the ones we'll need.
GSPC_Historical$Open <- NULL
GSPC_Historical$High <- NULL
GSPC_Historical$Low <- NULL
GSPC_Historical$Close <- NULL
#We always use the AdjClose over the normal Close.

#We usually work with 1,2,3,5,10,20,50,100,200 day intervals but for here
#we will just focus in on one time period so that we can focus in on the actual
#odds that certain past performances suggest a positive future performance.

#####posts#####

GSPC_Historical$AbsChange_10DayPost <- 0
GSPC_Historical$Delta_10DayPost <- 0

#####priors#####

GSPC_Historical$AbsChange_10DayPrior <- 0
GSPC_Historical$Delta_10DayPrior <- 0

GSPC_Historical$AbsChange_20DayPrior <- 0
GSPC_Historical$Delta_20DayPrior <- 0

GSPC_Historical$AbsChange_50DayPrior <- 0
GSPC_Historical$Delta_50DayPrior <- 0

GSPC_Historical$AbsChange_75DayPrior <- 0
GSPC_Historical$Delta_75DayPrior <- 0

GSPC_Historical$AbsChange_100DayPrior <- 0
GSPC_Historical$Delta_100DayPrior <- 0

GSPC_Historical$AbsChange_150DayPrior <- 0
GSPC_Historical$Delta_150DayPrior <- 0

GSPC_Historical$AbsChange_200DayPrior <- 0
GSPC_Historical$Delta_200DayPrior <- 0

GSPC_Historical$AbsChange_250DayPrior <- 0
GSPC_Historical$Delta_250DayPrior <- 0

GSPC_Historical$AbsChange_400DayPrior <- 0
GSPC_Historical$Delta_400DayPrior <- 0

#Now that we have determined the names of our variables, we'll store them as strings in a vector
#so that we can name the rows and columns of our future table of probabilities accordingly.

priors <- c("Delta_10DayPrior","Delta_20DayPrior","Delta_50DayPrior","Delta_75DayPrior","Delta_100DayPrior","Delta_150DayPrior","Delta_200DayPrior","Delta_250DayPrior","Delta_400DayPrior")
#distances <- as.character(c(-.1,-.04,-.025,-.020,-.015,-.01,-.005,0))
#distances <- c(0,-.001,-.002,-.003,-.004,-.005,-.01)
distances <- c(0,-.0025,-.005,-.0075,-.01,-.015,-.02,-.03,-.05,-.1)

#Initializing the one post and many prior conditions i.e. the total and percent changes over our timesframes.

for(i in 12:GSPC_N){
	GSPC_Historical$AbsChange_10DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-11)]]))
	GSPC_Historical$Delta_10DayPrior[i] <- (GSPC_Historical$AbsChange_10DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-11)]])
		
	GSPC_Historical$AbsChange_10DayPost[i-10] <- GSPC_Historical$AbsChange_10DayPrior[i]
	GSPC_Historical$Delta_10DayPost[i-10] <- GSPC_Historical$Delta_10DayPrior[i]
	
	if(i>=22){
		GSPC_Historical$AbsChange_20DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-21)]]))
		GSPC_Historical$Delta_20DayPrior[i] <- (GSPC_Historical$AbsChange_20DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-21)]])
	}
	
	if(i>=52){
		GSPC_Historical$AbsChange_50DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-51)]]))
		GSPC_Historical$Delta_50DayPrior[i] <- (GSPC_Historical$AbsChange_50DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-51)]])
	}
	
	if(i>=77){
		GSPC_Historical$AbsChange_75DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-76)]]))
		GSPC_Historical$Delta_75DayPrior[i] <- (GSPC_Historical$AbsChange_75DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-76)]])
	}
	
	if(i>=102){		
		GSPC_Historical$AbsChange_100DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-101)]]))
		GSPC_Historical$Delta_100DayPrior[i] <- (GSPC_Historical$AbsChange_100DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-101)]])
	}
	
	if(i>=152){		
		GSPC_Historical$AbsChange_150DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-151)]]))
		GSPC_Historical$Delta_150DayPrior[i] <- (GSPC_Historical$AbsChange_150DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-151)]])
	}
	
	if(i>=202){
		GSPC_Historical$AbsChange_200DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-201)]]))
		GSPC_Historical$Delta_200DayPrior[i] <- (GSPC_Historical$AbsChange_200DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-201)]])
	}
	
	if(i>=252){		
		GSPC_Historical$AbsChange_250DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-251)]]))
		GSPC_Historical$Delta_250DayPrior[i] <- (GSPC_Historical$AbsChange_250DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-251)]])
	}
	
	if(i>=402){		
		GSPC_Historical$AbsChange_400DayPrior[i] <- ((GSPC_Historical$AdjClose[[(i-1)]] - GSPC_Historical$AdjClose[[(i-401)]]))
		GSPC_Historical$Delta_400DayPrior[i] <- (GSPC_Historical$AbsChange_400DayPrior[[i]]/GSPC_Historical$AdjClose[[(i-401)]])
	}
	
}

print(names(GSPC_Historical))



S <- GSPC_Historical[12:GSPC_N,]
attach(S)
#I'm switching to S for ease and to get more of a sense that the data we uploaded is a sample space of outcomes.
#Also removing the first 11 rows of GSPC_Historical, which are empty.
S$probs <- 1/length(S$Date)
#We assume the Equally Likely Model of probability whereby we assume each of the length(S) outcomes has the same
#probability of occuring, namely 1/length(S).

#Now, let's construct our table. We'll start by naming it.

Day10Changes <- data.frame
t <- "Delta_10DayPost"
A <- subset(S,(S[t] > .005))
#It's easier to hard code these unchanging conditions for our loop before we start the loop
#since otherwise, we'd be calculating the same A (9*9) times.
#This "unchanging" condition is the desired probability, namely, the probability
#that we see a positive change of at least .5% in the market 10 days after the day in question.
#The 10DayPost and .5% percent change are being adopted as benchmarks of desirability for this project.

#Now that the table is named, it's time to populate it with probabiliites.

d <- length(distances)

for(r in priors){
	i <- 1
	currCol <- 0
	#currCol is where we'll store the p's we calculate for each distance r.
	#i is just an index for currCol; had to create it since our looping variables are not the numbers 1:K for some K.
	for(j in 1:(d-1)){
#In this double for loop in r and d, we are calculating Prob(S[t] > .05 | (S[r] < d[i]) &  (S[r] > d[i-1])) = Prob(A|S[r] < d).
		#print(d)
		#print(as.character(d))
		#Brd <- subset(S,(S[r] < d))
		#Brd <- subset(S,((S[r] < distances[j]) &  (S[r] > distances[j-1])))
		#print(c(r,j))
		
		#Using setdiff() here so that we can clearly type out the conditions within the for loop code.
		#With set diff, we will get the ranges we desire by taking the rows in S that for example,
		#have a delta prior less than 0 (in B2) and not in the set of rows of S that has a given delta prior less than -.005 (not in B1).
		#B1 holds fewer things than B2, and we want the things in B2 that are not in B1 to create the range -.5% - 0%
		B1 <- subset(S,S[r] < distances[j+1])
		B2 <- subset(S,S[r] < distances[j])
		Brd <- setdiff(B2,B1)
		#Naming the given condition 'Brd' rather than 'B' to signify that there is one for each r and d.
		Ard <- intersect(A,Brd)
		currp <- Prob(Ard)/Prob(Brd)
		#The above is equal to Prob(A | Brd) i.e. probablity of "A given Brd".
		
		#x <- c(length(B1$Date),length(B2$Date),length(Ard$Date),length(Brd$Date))
		#print(x)
		#Correcting NA's from 0 sets--if currp is NA because Ard was empty, switch from NA to 0.
		if((is.na(currp) == TRUE) & (nrow(Ard) == 0)){
			currp <- 0
			}
		#Storing the data in currCol, then incrementing the index so that the next point is stored above the current one.
		currCol[i] <- currp
		i = i+1
	}
	#Now we may add on a populated column of Day10 probabiliites to our table.
	Day10Changes <- cbind(Day10Changes,currCol)
	
}
#Removing the funny column with ????'s that gets initiated when one calls 'data.frame'.
#I believe we can wait until now rather than plucking it inside the loop.
#The above reason is that the code that constructs our table only builds a column and add it on a frame.
#This process will run length(priors) time, regardless of how many columns are already present.
Day10Changes <- Day10Changes[,2:(length(priors)+1)]
#length(priors) = number of columns in Day10Changes.

#Finally, with a populated table, we may name the rows and columns.
row.names(Day10Changes) <- as.character(distances[1:(d-1)])
colnames(Day10Changes) <- priors

#Please note: The row that shows 0% is the range -.25% - 0%, the row that shows
#-.25% is the range -.5% - -.25% and so on. This is to say the name of the row is the top of the range,
#and the name of the row below is bottom of the range of the same row in question.
#The last row, -5%, is the values -10% - -5% in the range of the prior changes,

print(Day10Changes)

#####Below is a chart that shows how the data each from the variables Delta_rDayPrior
#####is clustered around their means. Observe a "lying down" pattern as r goes up.

colors <- c("darkred","darkblue","darkgreen","orange","green","blue","pink","cyan")
priorsShort <- c("10Day","20Day","50Day","75Day","100Day","150Day","200Day","250Day","400Day")
legend <- NULL

for(i in 1:length(priors)){
	currString <- paste(priorsShort[i],"=",colors[i],sep = "")
	legend <- paste(legend,currString,sep = "||")
}


plot(density(S$Delta_10DayPrior), main = "Distributions of all Delta_rDayPrior", ylab = legend, cex.lab = .5)
lines(density(S$Delta_20DayPrior), col=colors[1])
lines(density(S$Delta_50DayPrior), col=colors[2])
lines(density(S$Delta_75DayPrior), col=colors[3])
lines(density(S$Delta_100DayPrior), col=colors[4])
lines(density(S$Delta_150DayPrior), col=colors[5])
lines(density(S$Delta_200DayPrior), col=colors[6])
lines(density(S$Delta_250DayPrior), col=colors[7])
lines(density(S$Delta_400DayPrior), col=colors[8])
abline(v = 0)
abline(v = .05)
