####This file is OBSOLETE and has been REVISED on 6/27/2020
####This file is OBSOLETE and has been REVISED on 6/27/2020
####This file is OBSOLETE and has been REVISED on 6/27/2020


#Making vectors with names of every prior/post we are using
priors <- c("Delta_Prior","Delta_2DayPrior","Delta_3DayPrior","Delta_5DayPrior","Delta_10DayPrior","Delta_20DayPrior","Delta_50DayPrior","Delta_100DayPrior","Delta_200DayPrior")
posts <- c("Delta","Delta_2DayPost","Delta_3DayPost","Delta_5DayPost","Delta_10DayPost","Delta_20DayPost","Delta_50DayPost","Delta_100DayPost","Delta_200DayPost")
intervals <- c(1,2,3,5,10,20,50,100,200)

#Initiating the "nameMatrix", which for now
#we are trying to make the same thing as "myTessaract"
seedString <- paste("myTable",intervals,sep="_t")
myTessaract <- data.frame
#we assign myTessaract by initiating row.names of our tessaract
#but only so that we can index the vector row.names(myTessaract) and properly name each row
#the present names are just dummy names

#myTessaract will end up being a matrix of matrices
#we can construct the matrix shell by looping through
#each concatenated "myTable_r(i)",
#appending to each one, every "_t(j)",
#storing each new "myTable_r(i)_t(j)" in
#a column vector of characetrs.
for(word in seedString){
	currWord <- (paste(word,intervals,sep="_r"))
	myTessaract <- cbind(myTessaract,currWord)
}

#Each column vector is added on to myTessaract
#As of now, myTessaract is a matrix of strings
#The strings will be the names of each table of probabilities

myTessaract <- myTessaract[,2:length(myTessaract[1,])]
###For reasons I don't understand, the first column of a data.frame gives
# $ :function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors())  
#This line of code is a workaround


print(myTessaract)
VarneyVector <- 0
VarneyIndex <- 0
subsetTupleVector <- 0
subsetTupleIndex <- 0

row.names(myTessaract) = 1:length(priors)
print(myTessaract)

for(r in 1:length(priors)){
	row.names(myTessaract)[r] <- priors[r]
	#name the rth row of our tesssaract with the rDayPrior that is used
	for(t in 1:length(posts)){
		if(r==length(priors)){
			colnames(myTessaract)[t] <- posts[t]
			#we also name the t'th column of myTessaract with each tDayPost that is used for this rDayPrior
			#BUT
			#we only do this if we are working on the last column
			#since otherwise, we will name columns (length(intervals)) times (i.e. 9 times as of 9/7/2019)
			}
		#currTable <- data.frame(row.names = 0:10)
		currTable <- data.frame
		#initiate/reset currTable by assigning it a blank data.frame
		#and then initiate the rownames of that new blank data.frame
		Varney <- 0
		#Stu is here to help us count the number of times we caclulate p
		#It should be (10+1)**2 = 121 for the mini version
		#And (100+1)**2 = 10201 for the real thing
		for(j in 0:10){
			currCol <- NULL
			for(i in 0:10){
				dt <- j*.01
				dr <- i*.01 - .05
				A <- subset(S,((S[posts[t]] > dt) & (S[priors[r]] < dr)))
				B <- subset(S,(S[priors[r]] < dr))
				p <- Prob(A)/Prob(B)
				#p = P(\Delta_t > j*.01 | \Delta_r < i*.01 - .05)
				#for r,t \in \{1,2,3,5,10,20,50,100,200}
				#and for j,i \in [10]*
				#print(paste(i,j))
				if((p > -1 & p < 1) & is.na(p) == FALSE){
					Varney <- Varney+1
					}
					#if p is a valid probability estimate, give Stu another
					#I am hoping that we spot some oddly numbered Varney's that show us
					#The probability function is not working all of the time
					#We want Varney = 121, if we are taking i,j \in [10]*
				#Now that we have counted the validity of p, we will want to swap out NA's for 0's
				#This, however, should only occur if the reason that we are getting Prob(A)/Prob(B) returning NA
				#Is that A is an empty set
				if((is.na(p) == TRUE) & nrow(A) == 0){
					p <- 0
				}
				#Now we append p
				currCol[i+1] <- p
				#populates currCol with 11/121 entries
				###***subsetTupleIndex <- subsetTupleIndex + 1
				####***subsetTupleVector[subsetTupleIndex] <- c(A,B,p)
				#increment subsetTupleIndex by 1
				#to store the current subsetTuple c(A,B) in the next spot
				#This will be very useful to see what kind of sets we are attempting to compute probabilities with
				#It may also be useful data in itself
				#We are calculating the A's and B's, why not keep em?
				#speaking of, we'll properly name the current row of our currentTable
				#(but only if we have just added the last column, since otherwise we will be unnecessarily naming each row 11 times)
				if(j==10){
					row.names(currTable)[i+1] <- paste("dr=",as.character(dr),sep="")
				}
			}
			#print(Varney)
			currTable <- cbind(currTable,currCol)
			if(j==1){
				row.names(currTable) <- 0:10
				#dummy names to initiate currTable's row names
				#the true row.names will be assigned only as we are iterating through the final column of each currTable
			}
			#add each newly populated currCol to currTable
			colnames(currTable)[j+2] <- paste("dt=",as.character(dt),sep="")
			#and then name that column
		}
		currTable <- currTable[,2:length(currTable[1,])]
		#Chopping off bugged first column
		assign(myTessaract[[r,t]],currTable)
		#"naming" currTable by assigning the string 'myTable_t(j)_r(i)' to currTable
		#The string 'myTable_t(j)_r(i)' is "temporarily" being stored in myTessaract[[r,t]]
		#We can swap out the string for the actual table now that we've populated and "named" it
		myTessaract[[r,t]] <- currTable
		VarneyIndex <- VarneyIndex + 1
		#After we store currTable in the r,t position in the tessaract
		#We increment our VarneyIndex by 1
		#This just allows us to store our calculated Varney
		#In the next position down in our VarneyVector
		VarneyVector[VarneyIndex] <- Varney
		#Ideally, each Varney would be 121
		#Anything less means either NA, or our prob function has an error
		#But again, we are still (perhaps naively) figuring what NA means when returned by Prob()
		print(Varney)
	}
}
