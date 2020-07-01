####This doucment calculates a made-up yet unclear if original metric,
####the "NormClose," which is defined by
####NormClose[currentDay] = NormClose[previousDay]*(1 + percentChange[currentDay]).

####The hope in writing code for the NormClose was that
####the distraction of the particular "price" of a stock could be removed
####i.e. the particular value of securities would mean the same thing
####whether it were the NormClose[NFLX], NormClose[PIMIX] or NormClose[IAU].

#First, we'll load in the data for the 8 securities chosesn to be analyzed
#side-by-side with the NormClose statistic.

source("GSPC_DeltaOnly_20190820.R")
source("DJI_DeltaOnly_20190820.R")
source("NSRGY_DeltaOnly_20190820.R")
source("PFF_DeltaOnly_20190820.R")
source("IAU_DeltaOnly_20190820.R")
source("SLV_DeltaOnly_20190820.R")
source("SHY_DeltaOnly_20190820.R")
source("HYG_DeltaOnly_20190820.R")
detach()

symbols <- c("GSPC","DJI","NSRGY","PFF","IAU","SLV","SHY","HYG")
colors <- c("black","red","blue","forestgreen","darkgoldenrod","gray","turquoise","limegreen")
securities <- data.frame(symbols,colors)

#######I am going to try to use hard-coded symbols as little as possible.
#######For this reason, I want to create a mini-data.frame of
#######the names of the securities and the color I'll assign each one.
#######Then, this essentially assigns each security a number
######I'll want to just think of securities as being numbers 1 - 2^k for some k,

####6/27/2019: My k is 3 for now, I think higher than that may get unwieldy.
####6/28/2019: I am not going to mess with the hard-coded symbols, now that is for another day :)

print("files are open and running")

#The goal of the next 7 lines (WHERE 7 = 2^3 - 1)
#is to combine all 8 of the data.frames we uploaded above into one large data.frame.
#We will merge 4 pairs, and then from those 4 new merged sets,
#merge 2 pairs, and the 2 merged sets that still stand will be merged to complete the sequence.


myData1 <- merge(GSPC_Historical,DJI_Historical,by.x = "Date", by.y = "Date",suffixes = c(".GSPC",".DJI"))
myData2 <- merge(NSRGY_Historical,PFF_Historical,by.x = "Date", by.y = "Date",suffixes = c(".NSRGY",".PFF"))
myData3 <- merge(IAU_Historical,SLV_Historical,by.x = "Date", by.y = "Date",suffixes = c(".IAU",".SLV"))
myData4 <- merge(SHY_Historical,HYG_Historical,by.x = "Date", by.y = "Date",suffixes = c(".SHY",".HYG"))

myData5 <- merge(myData1,myData2,by.x = "Date",by.y = "Date")
myData6 <- merge(myData3,myData4,by.x = "Date",by.y = "Date")

myData <- merge(myData5,myData6,by.x = "Date",by.y = "Date")

#Please note that the merge will drop the dates that only one data.frame has
#i.e. in the case where a security wasn't introduced until the mid-2000s.

print("over here")

####IMPT: With respect to the range of days over which NormClose will be computed
####'s' is for start, 'N' is for eNd.

#We'll default to ranging from the earliest day available to the latest.
#One may readily switch out the true start, s=2, and true end, N=length(myData$Date)
#to see how the NormClose's of the variety of securities behaves over a custom interval instead
#i.e setting Merged_s <- which(myData$Date %in%  as.Date("2007-1-10")) will mean Merged_s is set
#to the day with date "2008-1-10".

Merged_s <- 2

#"s" for start needs to be >= 2 so that the program doesn't crash when
#attempting to multiply Delta*NormClose[0].

Merged_N <- length(myData[[1]])
#myData[1] is myData$Date; this line simply takes the length of that column vector as the length of the data.frame.
# #Merged_N <- which(myData$Date %in%  as.Date("2010-6-15"))
# #Merged_N <- 700

# #This is just the day where we start computing the NormClose.
# #The which() line returns the index of the desired date.
# #With this which() line, we can just input a date rather than arbitrarily find a good cutoff index.

#The NormClose of any security is by definition, initialized to 1.

myData$NormClose.GSPC <- 1
myData$NormClose.DJI <- 1
myData$NormClose.NSRGY <- 1
myData$NormClose.PFF <- 1
myData$NormClose.IAU <- 1
myData$NormClose.SLV <- 1
myData$NormClose.SHY <- 1
myData$NormClose.HYG <- 1

print("all the way here")

for(i in Merged_s:Merged_N){
	
	#For each day i in the interval of days [Merged_s,Merged_N], compute the ith NormClose
	#by taking the Delta (or change in value from open to close) of day i and multiply (Delta[i]+1) by the previous NormClose.
	
	myData$NormClose.GSPC[i] <- myData$NormClose.GSPC[i-1]*(1+myData$Delta.GSPC[i])
	myData$NormClose.DJI[i] <- myData$NormClose.DJI[i-1]*(1+myData$Delta.DJI[i])
	myData$NormClose.NSRGY[i] <- myData$NormClose.NSRGY[i-1]*(1+myData$Delta.NSRGY[i])
	myData$NormClose.PFF[i] <- myData$NormClose.PFF[i-1]*(1+myData$Delta.PFF[i])
	myData$NormClose.IAU[i] <- myData$NormClose.IAU[i-1]*(1+myData$Delta.IAU[i])
	myData$NormClose.SLV[i] <- myData$NormClose.SLV[i-1]*(1+myData$Delta.SLV[i])
	myData$NormClose.SHY[i] <- myData$NormClose.SHY[i-1]*(1+myData$Delta.SHY[i])
	myData$NormClose.HYG[i] <- myData$NormClose.HYG[i-1]*(1+myData$Delta.HYG[i])


}

#^^^Wondering if there is a more efficient way to do this....
#It also may not matter, I have not had any issues with find/replace or anything else.

attach(myData)
print(names(myData))

#These next few lines are just to spiff up whatever graph we choose to generate:

groupName <- "NSRGY/Commodities/Indicies"
#In these two lines, we will create a semi-modular chart header with admittedly some hard coding.
#The securities were hard coded so it seems reasonable for a part of a chart header to be as well.
chartName <- paste(groupName,"from",as.character(Date[Merged_s]),"through",as.character(Date[Merged_N]))
#The actual chart header is more robust that 'groupName'.
#The header will change if the user changes the starting day, Merged_s, or the stopping day, Merged_N.
legend <- NULL
#As the name 'legend' might portend, the following for loop is for:
#securities[,1] refers to the column that houses all 8 of our securites;
#securities[,2] refers to the column that shows which color was assigned to which security.
for(i in 1:length(securities[,1])){
	currString <- paste(as.character(securities[,1][i]),"=",as.character(securities[,2][i]),sep = "")
	legend <- paste(legend,currString,sep = "||")
	#legend=+1
}

#Taking the max of the NormClose's so that I know how high to draw the y-axis.
#We'll use these metrics as arguments when we graph our 8 NormCloses.
#In particular we'll assign something along the lines of ylim = c(m,L).
L <- max(NormClose.GSPC,NormClose.NSRGY,NormClose.PFF,NormClose.DJI,NormClose.IAU,NormClose.SLV,NormClose.SHY,NormClose.HYG)
m <- min(NormClose.GSPC,NormClose.NSRGY,NormClose.PFF,NormClose.DJI,NormClose.IAU,NormClose.SLV,NormClose.SHY,NormClose.HYG)

#We're ready for the plot. We'll assign the top/bottom of the graph, label the title and yaxis.
plot(x = Date[(Merged_s-1):Merged_N], y = NormClose.GSPC[(Merged_s-1):Merged_N], ylim = c((m-.07),(L+.5)),type = "l", main = chartName, xlab = "", ylab = legend, cex.lab = .5,col = as.character(securities[,2][1]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.DJI[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][2]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.NSRGY[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][3]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.PFF[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][4]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.IAU[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][5]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.SLV[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][6]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.SHY[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][7]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.HYG[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][8]))

#The horizontal lines will give the viewer a frame of reference.
#One can see with ease whether a NormClose moves below its initial value.
#And whether a NormClose eclipses the min/max of the S&P's NormClose.

abline(h = 1)
abline(h = min(NormClose.GSPC), col = "brown")
abline(h = max(NormClose.GSPC), col = "brown")
#abline(v = Date[700])
###which(NormClose.NSRGY %in% min(NormClose.NSRGY)) == 700
###700 is a bit of a recession naidir.
