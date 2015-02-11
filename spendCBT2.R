#Buy Till You Die - A walkthrough 
#This code is build for SpendCBT.

rm(list=ls()) #Clears Workspace 
library(BTYD)
setwd("") #Set working directory

elog <- read.csv("", header = TRUE) #Load the file

elog$date <- as.character(elog$date)
elog$date <- as.Date(elog$date, "%Y%m%d") #Convert to date format
summary(elog) #Check

elog <- dc.MergeTransactionsOnSameDate(elog); #Merging transactions on the same date

cohort_end_date = '2013-11-30' #Need to make further research. http://www.inside-r.org/packages/cran/BTYD/docs/dc.ElogToCbsCbt

(end.of.cal.period <-
   min(elog$date)+as.numeric((max(elog$date)-min(elog$date))/2)) #Determing cut off date (middle)

elog.cal <- elog[which(elog$date <= end.of.cal.period), ] 

split.data <- dc.SplitUpElogForRepeatTrans(elog.cal); #Spliting the data
clean.elog <- split.data$repeat.trans.elog; #Clean

###Creating CBS - Matrix
spend.cbt <- dc.CreateSpendCBT(clean.elog); #Customer-by-time matrix, is.avg.spend = FALSE
spend.cbt[1:3,1:5] #Check. You should sales values here.

tot.cbt <- dc.CreateSpendCBT(elog.cal) #Changed elog to elog.cal
cal.cbt <- dc.MergeCustomers(tot.cbt, spend.cbt) #All customers should be here.

birth.periods <- split.data$cust.data$birth.per #birth
last.dates <- split.data$cust.data$last.date #last dates
cal.cbs.dates <- data.frame(birth.periods, last.dates, 
                            end.of.cal.period)
cal.cbs <- dc.BuildCBSFromCBTAndDates(cal.cbt, cal.cbs.dates,
                                      per="week") #calibration cbs
##############################################

#Setting up to Estimate Parameter

##############################################
calculateAvgSpend <- function(cbt.row) {
  purchaseCols = which(cbt.row != 0)
  sum(cbt.row[purchaseCols]) / length(purchaseCols)
} #Average Spend calculation. We will need this to estimate parameters

m.x <- apply(cal.cbt, 1, calculateAvgSpend) # Contains average for each customer
m.x[which(is.na(m.x))] <- 0  #Removes NaNs
# write.csv(m.x, file = "") #Check
ave.spend <- m.x #Input m.x with removed NaNs.
tot.trans <- cal.cbs[,"x"]
# write.csv(tot.trans, file = "")#Check

ave.spend <- ave.spend[which(tot.trans > 0)]  #remove customers with 0 transactions
tot.trans <- tot.trans[which(tot.trans > 0)]  #remove customers with 0 transactions

(params <- spend.EstimateParameters(ave.spend, tot.trans)) #Estimate spend parameters

# spendParams <- spend.EstimateParameters(m.x, cal.cbs[,1])
# print(spendParams) ##there are warnings here, but not above? Why?

spend.plot.average.transaction.value(params, ave.spend, tot.trans) #Plot
spend.LL(params, ave.spend, tot.trans) #Log likelyhood of spend

