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

### this is to parse the census for us population by state, no age.
myjsoncensus <- jsonlite::fromJSON(txt = censusGEOURL)
uspop <- as.data.frame(myjsoncensus)
names(uspop) <- as.character(unlist(uspop[1,]))
uspop <- uspop[-1,]
uspop$POP <- as.numeric(as.character(uspop$POP))
uspop$DENSITY <- as.numeric(as.character(uspop$DENSITY))

### this is to parse the census for us population by age and month 
myjsoncensusAGE <- jsonlite::fromJSON(txt = censusAGEURL)
uspopAGE <- as.data.frame(myjsoncensusAGE)
names(uspopAGE) <- as.character(unlist(uspopAGE[1,]))
uspopAGE <- uspopAGE[-1,]
uspopAGE$POP <- as.numeric(as.character(uspopAGE$POP))
uspopAGE$AGE <- as.numeric(as.character(uspopAGE$AGE))
uspopAGE$MONTHLY <- as.numeric(as.character(uspopAGE$MONTHLY))


######################## Credit Card Figures ######################
# http://www.statisticbrain.com/credit-card-ownership-statistics/ #
# Unused source:                                                  #
#     https://wallethub.com/edu/number-of-credit-cards/25532/     #
# Total number of credit cards in use in the US 	1,895,834,000   #
# Total number of US credit card holders 	199,800,000             #
# 
###################################################################


Total_Number_CreditCards <- as.numeric(1895834000)                
Total_Number_CardHolders <- as.numeric(199800000)                 

#plot filtered to dec 2016
uspopAGEDist <- as.data.frame(uspopAGE[which(((uspopAGE$AGE <= 80) & (uspopAGE$AGE >= 1)) & (uspopAGE$MONTHLY == 94)),])
plot(x=uspopAGEDist$AGE, y=(uspopAGEDist$POP/1000), type = "o", xlab = "AGE", ylab = "Population ('000s)")
#max(uspopAGE$POP)





av <- zoo::zoo(uspopAGEDist$POP)
agelag <- as.data.frame(lag(av,k = -1, na.pad = TRUE))
names(agelag) <- c("agelagyr")

agelagcombined <-  as.data.frame(cbind(uspopAGEDist, agelag[1]))
agelagcombined_pct <-  as.data.frame(((agelagcombined$POP - agelagcombined$agelagyr)
                                      /agelagcombined$agelagyr)*100)
names(agelagcombined_pct) <- c("pct_chg")
agelagcombined <-  as.data.frame(cbind(agelagcombined, agelagcombined_pct[1]))
plot(agelagcombined$pct_chg, type = "o")
## The changes in population fluctate between gains and losses plus or minus 5 percent, so the number that went from one age to another, is followed by a close figure
##   to replace them. Keeping the number of populants in the early age category
##   giving more of a uniform distribution to the entire us population and age




# If we use normal distribution . using a distribution table
onedev <- .68*max(uspop$POP)
twodev <- .95*max(uspop$POP)
threedev <- .9967*max(uspop$POP)

QtyPopulationByDev <- function(no_deviations = 1){
  if(no_deviations == 1){devValue <- .68*max(uspop$POP)}
      else if(no_deviations == 2){devValue <- .95*max(uspop$POP)}
          else if(no_deviations == 3){devValue <- .9967*max(uspop$POP)}
  return(devValue)
  }
  

#QtyPopulationByDev(no_deviations = 3)
  
  

#age range between 1 and 80 at 1 deviation
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










