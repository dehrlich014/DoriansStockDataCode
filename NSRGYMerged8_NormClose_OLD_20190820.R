####This file is OLD and has been REVISED on 6/27/2020
####This file is OLD and has been REVISED on 6/27/2020
####This file is OLD and has been REVISED on 6/27/2020

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

#######I am going to try to use hard-coded symbols as little as possible
#######For this reason, I want to create a mini-data.frame of
#######The names of the securities and the color I'll assign each one
#######Then, this essentially assigns each security a number
######I'll want to just think of securities as being numbers 1 - 2^k for some k

####6/27/2019: My k is 3 for now, I think higher than that may get unwieldy
####6/28/2019: I am not going to mess with the hard-coded symbols, now that is for another day :)

print("files are open and running")

myData1 <- merge(GSPC_Historical,DJI_Historical,by.x = "Date", by.y = "Date",suffixes = c(".GSPC",".DJI"))
myData2 <- merge(NSRGY_Historical,PFF_Historical,by.x = "Date", by.y = "Date",suffixes = c(".NSRGY",".PFF"))
myData3 <- merge(IAU_Historical,SLV_Historical,by.x = "Date", by.y = "Date",suffixes = c(".IAU",".SLV"))
myData4 <- merge(SHY_Historical,HYG_Historical,by.x = "Date", by.y = "Date",suffixes = c(".SHY",".HYG"))

myData5 <- merge(myData1,myData2,by.x = "Date",by.y = "Date")
myData6 <- merge(myData3,myData4,by.x = "Date",by.y = "Date")

myData <- merge(myData5,myData6,by.x = "Date",by.y = "Date")

print("over here")

####IMPT: s is for start, N is for eNd

#Merged_s <- which(myData$Date %in%  as.Date("2007-1-10"))
Merged_s <- 2

###"s" for start
###needs to be >= 2

#Merged_N <- length(myData[[1]])
Merged_N <- which(myData$Date %in%  as.Date("2010-6-15"))
#Merged_N <- 700

###This is just the day where we start computing the NormClose
###The which() line returns the index of the desired date
###With this which() line, we can just input a date rather than arbitrarily find a good cutoff index

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
	
	myData$NormClose.GSPC[i] <- myData$NormClose.GSPC[i-1]*(1+myData$Delta.GSPC[i])
	myData$NormClose.DJI[i] <- myData$NormClose.DJI[i-1]*(1+myData$Delta.DJI[i])
	myData$NormClose.NSRGY[i] <- myData$NormClose.NSRGY[i-1]*(1+myData$Delta.NSRGY[i])
	myData$NormClose.PFF[i] <- myData$NormClose.PFF[i-1]*(1+myData$Delta.PFF[i])
	myData$NormClose.IAU[i] <- myData$NormClose.IAU[i-1]*(1+myData$Delta.IAU[i])
	myData$NormClose.SLV[i] <- myData$NormClose.SLV[i-1]*(1+myData$Delta.SLV[i])
	myData$NormClose.SHY[i] <- myData$NormClose.SHY[i-1]*(1+myData$Delta.SHY[i])
	myData$NormClose.HYG[i] <- myData$NormClose.HYG[i-1]*(1+myData$Delta.HYG[i])


}

####^^^Wondering if there is a more efficient way to do this....
####It also may not matter, I have not had any issues with find/replace or anything else

attach(myData)
print(names(myData))

####These next few lines are just to spiff up whatever graph I choose to generate
groupName <- "NSRGY/Commodities/Indicies"
chartName <- paste(groupName,"from",as.character(Date[Merged_s]),"through",as.character(Date[Merged_N]))
legend <- NULL
for(i in 1:length(securities[,1])){
	currString <- paste(as.character(securities[,1][i]),"=",as.character(securities[,2][i]),sep = "")
	legend <- paste(legend,currString,sep = "||")
}

# ####Taking the max of the NormClose's so that I know how high to draw the y-axis
L <- max(NormClose.GSPC,NormClose.NSRGY,NormClose.PFF,NormClose.DJI,NormClose.IAU,NormClose.SLV,NormClose.SHY,NormClose.HYG)
m <- min(NormClose.GSPC,NormClose.NSRGY,NormClose.PFF,NormClose.DJI,NormClose.IAU,NormClose.SLV,NormClose.SHY,NormClose.HYG)
#####USE THE ABOVE L

#L <- max(NormClose.GSPC,NormClose.NSRGY,NormClose.PFF,NormClose.DJI,NormClose.SLV,NormClose.SHY,NormClose.HYG)


plot(x = Date[(Merged_s-1):Merged_N], y = NormClose.GSPC[(Merged_s-1):Merged_N], ylim = c((m-.07),(L+.5)),type = "l", main = chartName, xlab = "", ylab = legend, cex.lab = .5,col = as.character(securities[,2][1]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.DJI[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][2]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.NSRGY[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][3]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.PFF[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][4]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.IAU[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][5]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.SLV[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][6]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.SHY[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][7]))
lines(x = Date[(Merged_s-1):Merged_N], y = NormClose.HYG[(Merged_s-1):Merged_N], type = "l", col = as.character(securities[,2][8]))

# plot(density(Delta.GSPC[Merged_s:Merged_N]), xlim = c(-.075,.075), ylim = c(0,75), main = chartName, xlab = "", ylab = legend, cex.lab = .5,col = as.character(securities[,2][1]))
# lines(density(Delta.DJI[Merged_s:Merged_N]),col = as.character(securities[,2][2]))
# lines(density(Delta.NSRGY[Merged_s:Merged_N]),col = as.character(securities[,2][3]))
# lines(density(Delta.PFF[Merged_s:Merged_N]),col = as.character(securities[,2][4]))
# lines(density(Delta.IAU[Merged_s:Merged_N]),col = as.character(securities[,2][5]))
# lines(density(Delta.SLV[Merged_s:Merged_N]),col = as.character(securities[,2][6]))
# lines(density(Delta.SHY[Merged_s:Merged_N]),col = as.character(securities[,2][7]))
# lines(density(Delta.HYG[Merged_s:Merged_N]),col = as.character(securities[,2][8]))
abline(h = 1)
abline(h = min(NormClose.GSPC), col = "brown")
abline(h = max(NormClose.GSPC), col = "brown")
#abline(v = Date[700])
###which(NormClose.NSRGY %in% min(NormClose.NSRGY)) == 700
###700 is a bit of a recession naidir
