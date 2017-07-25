#https://github.com/pimikeymike/CreditCardEstimate.git

######### US Census Data Set ###############################
# Monthly Population Estimates by Universe                 #
#      , Age, Sex, Race, and Hispanic Origin               #
# for the United States: April 1, 2010 to December 1, 2016 #
# Source: U.S. Census Bureau, Population Division          #
############################################################

library(RCurl)
library(data.table)
APIKEY <- fread(file.path(getwd(),paste("CensusAPIKEY",".csv",sep = "")), header = TRUE)
censusGEOURL <- paste0("https://api.census.gov/data/2016/pep/population?get=POP,DENSITY,GEONAME&for=state:*&key=",APIKEY)
censusAGEURL <- paste0("https://api.census.gov/data/2016/pep/natmonthly?get=POP,MONTHLY,AGE,GEONAME&for=us:*&key=",APIKEY)
rm(APIKEY)

library(jsonlite)
myjsoncensus <- jsonlite::fromJSON(txt = censusURL)
uspop <- as.data.frame(myjsoncensus)
names(uspop) <- as.character(unlist(uspop[1,]))
uspop <- uspop[-1,]
uspop$POP <- as.numeric(as.character(uspop$POP))
uspop$DENSITY <- as.numeric(as.character(uspop$DENSITY))


myjsoncensusAGE <- jsonlite::fromJSON(txt = censusAGEURL)
uspopAGE <- as.data.frame(myjsoncensusAGE)
names(uspopAGE) <- as.character(unlist(uspopAGE[1,]))
uspopAGE <- uspopAGE[-1,]
uspopAGE$POP <- as.numeric(as.character(uspopAGE$POP))
uspopAGE$AGE <- as.numeric(as.character(uspopAGE$AGE))
uspopAGE$MONTHLY <- as.numeric(as.character(uspopAGE$MONTHLY))


######################## Credit Card Figures ######################
# http://www.statisticbrain.com/credit-card-ownership-statistics/ #
# Total number of credit cards in use in the US 	1,895,834,000   #
# Total number of US credit card holders 	199,800,000             #
###################################################################


Total_Number_CreditCards <- as.numeric(1895834000)                
Total_Number_CardHolders <- as.numeric(199800000)                 

#plot filtered to dec 2016
uspopAGEDist <- uspopAGE[which((uspopAGE$AGE <= 80) & (uspopAGE$AGE >= 1) & (uspopAGE$MONTHLY == 94)),]
plot(x=uspopDist$AGE, y=(uspopAGEDist$POP/1000), type = "o")
max(uspopAGE$POP)

# If we use normal distribution . using a distribution table
onedev <- .68*max(uspop$POP)
twodev <- .95*max(uspop$POP)
threedev <- .9967*max(uspop$POP)

#age range between 1 and 80 then
agemedian <- 80/2
AgeonedevLow <- (agemedian*(1-(.68/2)))
AgeonedevHigh <- (agemedian*(1+(.68/2)))

# normal distribution on 2 deviations. 95 percent
AgetwodevLow <- (agemedian*(1-(.95/2)))
AgetwodevHigh <- (agemedian*(1+(.95/2)))

#normal at 3 deviations, 99.7
AgethreedevLow <- (agemedian*(1-(.997/2)))
AgethreedevHigh <- (agemedian*(1+(.997/2)))

# normal distribution outside 95 percent that are cannot have credit cards

qty_Popul_earlyAge <- (((1-.95)/2)*sum(uspop$POP))

CensusEarlyAge <- sum(uspopAGEDist[which(uspopAGEDist$AGE >= 1 & uspopAGEDist$AGE <= 21),"POP"])

### definitely not normal distribution . maybe if uniform would suggest a period of low population followed by high population
###  then followed by low rate of births. we can check the census 2016 to get an idea of rate of "transfer" between ages. Not sure
### if there is a terminology for this.

head(uspopAGEDist,10)





