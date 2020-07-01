####This document is a companion doucment to GSPC_ProbMatrixTesting_REVISED_20200626.R.
####In this document, a data.frame of data.frames of probabilities is created, i.e. a matrix of matrices.
####Each individual data.frame of probabilities show 121 different probabilities that (Delta_tDayPost > dt | Delta_rDayPrior > dr)
####WHERE dr ranges over [-.05,.05] BY .01 AND dt ranges over [0,.1] BY .01
####AND WHERE t,r each range over the set {1,2,3,5,10,20,50,100,200}.
####***What's externally needed for this file to run, however, is a sample space, S, that has computed each of the rDayPriors and tDayPosts.


#Making vectors with names of every prior/post we are using.
priors <- c("Delta_Prior","Delta_2DayPrior","Delta_3DayPrior","Delta_5DayPrior","Delta_10DayPrior","Delta_20DayPrior","Delta_50DayPrior","Delta_100DayPrior","Delta_200DayPrior")
posts <- c("Delta","Delta_2DayPost","Delta_3DayPost","Delta_5DayPost","Delta_10DayPost","Delta_20DayPost","Delta_50DayPost","Delta_100DayPost","Delta_200DayPost")
intervals <- c(1,2,3,5,10,20,50,100,200)
#These 3 vectors will be used to name our tessaract.

seedString <- paste("myTable",intervals,sep="_t")
myTessaract <- data.frame
#We assign myTessaract by initiating row.names of our tessaract
#but only so that we can index the vector row.names(myTessaract) and properly name each row.
#The present names are just dummy names.

#myTessaract will end up being a matrix of matrices.
#We can construct the matrix shell by looping through
#each concatenated "myTable_t(i)",
#appending to each one, every "_r(j)",
#storing each new "myTable_t(i)_r(j)" in a column vector of characetrs.
for(word in seedString){
	currWord <- (paste(word,intervals,sep="_r"))
	myTessaract <- cbind(myTessaract,currWord)
}

#Paste creates a column vector of strings since the argument 'intervals' is a vector.
#That vector is then appended to the data.frame myTessaract.

#Each column vector is added on to myTessaract, where as of now, myTessaract is a matrix of strings.
#The strings will be the names of each individual table of probabilities.

myTessaract <- myTessaract[,2:length(myTessaract[1,])]
###For reasons I don't understand, the first column of a data.frame gives
# $ :function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors())  
#--this line of code is a workaround.


print(myTessaract)
pCountVector <- 0
pCountIndex <- 0

row.names(myTessaract) <- priors
colnames(myTessaract) <- posts

print(myTessaract)

#This is a quadruple for loop.
#We are ranging, in order, over
#Prior period;
#Post period;
#Distance away from 0% change in value following day of (post);
#Distance away from 0% change in value before day of (prior).
for(r in 1:length(priors)){
	for(t in 1:length(posts)){
		#currTable <- data.frame(row.names = 0:10)
		currTable <- data.frame
		#initiate/reset currTable by assigning it a blank data.frame
		#and then initiate the rownames of that new blank data.frame.
		pCount <- 0
		#pCount is here to help us count the number of times we caclulate p
		#It should be (10+1)**2 = 121 for each i,j iteration i.e. each prob table we create
		for(j in 0:10){
			currCol <- NULL
			for(i in 0:10){
				dt <- j*.01
				#dt calculates the distance that the tDayPost is measured against.
				dr <- i*.01 - .05
				#And dr calculates the distance that rDayPrior is measured against.
				#We are creating columns that calculate given priors from [-5%,5%] by 1%,
				#holding each post constant from [0%,10%], also up by 1%.
				#By calculating 11 priors for each of 11 posts, we have a table of 121 different probabilities.
				A <- subset(S,((S[posts[t]] > dt) & (S[priors[r]] < dr)))
				#A is the intersection of the rows where tDayPost > dt.
				#AND the rows where rDayPrior < dr
				#The intersect() function has been tested and observed to be *dramatically* slower than simply using &.
				B <- subset(S,(S[priors[r]] < dr))
				#B is the given condition here, i.e. P(A|B).
				p <- Prob(A)/Prob(B)
				#This is a conditional probability, namely the Prob(tDayPost > dt|rDayPrior < dr),
				#or as we compute here, Prob(A)/Prob(B) WHERE A = intersect([tDayPost > dt],[rDayPrior < dr]).
				
				#To be more specific, what we calculate in variable p is
				#p = P(\Delta_t > j*.01 | \Delta_r < i*.01 - .05)
				#for r,t \in \{1,2,3,5,10,20,50,100,200}
				#and for j,i \in {0,1,2,...,10}.
				
				#We will want to swap out NA's for 0's
				#This, however, should only occur if the reason that we are getting Prob(A)/Prob(B) returning NA
				#is that A is an empty set.
				#This swapping of NA for 0 occurs since R returns NA when the size of an event in a sample space is 0.
				#R "should" return 0 when the size of the set being measured is 0, but it decides to return NA instead.
				if((is.na(p) == TRUE) & nrow(A) == 0){
					p <- 0
				}
				if((p > -1 & p < 1) & is.na(p) == FALSE){
					pCount <- pCount+1
				}
				#We want pCount = 121, if we are taking i,j \in {0,1,2,...,10} each
				#since the only "false errors" we've observed are the NA's when P(A) should return 0 and we corrected those.
				#In theory, we now should only have NA's if there is a true error.
				
				currCol[i+1] <- p
				#The i-loop is where we calculate 11 different p's for 11 different priors while holding the post constant.
				#So in each of the 11 i-loops,we populate currCol with 11/121 entries.
				
				if(j==10){
					print("Trying to name rows")
					print(paste("i,j are",i,j,sep = " "))
					row.names(currTable)[i+1] <- paste("dr=",as.character(dr),sep="")
				}
				#i+1 because i starts from 0 and there is no row 0 in R.
			###End of i loop.
			}
			currTable <- cbind(currTable,currCol)
			if(j==1){
				row.names(currTable) <- 0:10
				#Dummy names to initiate currTable's row names--we get an error when trying to call row.names(currTable) otherwise
				#the true row.names will be assigned only as we are iterating through the final column of each currTable.
			}
			#Add each newly populated currCol to currTable and name the column while in our currTable.
			#j+2 because j starts from 0 AND we apparently will get a bugged first column that we have to ignore and then remove.
			print("Trying to name columns")
			print(paste("i,j are",i,j,sep = " "))
			colnames(currTable)[j+2] <- paste("dt=",as.character(dt),sep="")
		###End of j loop
		}
		currTable <- currTable[,2:length(currTable[1,])]
		#Chopping off bugged first column.
		assign(myTessaract[[r,t]],currTable)
		#"Naming" currTable by assigning the string 'myTable_t(j)_r(i)' to currTable.
		#The string 'myTable_t(j)_r(i)' is "temporarily" being stored in myTessaract[[r,t]]
		#We can swap out the string for the actual table, which is a data.frame and not a string, now that we've populated and "named" it.
		myTessaract[[r,t]] <- currTable
		pCountIndex <- pCountIndex + 1
		#After we store currTable in the r,t position of the tessaract, we increment our pCountIndex by 1.
		#This just allows us to store our calculated pCount in the next position down in our pCountVector.
		pCountVector[pCountIndex] <- pCount
		#Ideally, each pCount would be 121.
		#Anything less means either an NA appeared, or our prob function has an error.
		#But again, we are still (perhaps naively) figuring what NA means when returned by Prob()
		#print(pCount)
	###End of r,t loops
	}
}
