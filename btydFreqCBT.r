#Buy 'Til you Die Package 
# PREDICTING LONG TERM CUSTOMER VALUE WITH BTYD PACKAGE
# Pareto/NBD (negative binomial distribution) modeling of 
# repeat-buying behavior in a noncontractual setting, 
#Reference: https://gist.github.com/mattbaggott/5113177 
#Data divided into calibration (training) and holdout (testing). 

################################################################ 

#INSTALLING and SETTING WORKING DIRECTORY

################################################################ 

rm(list=ls())

setwd("/Users/ShrutikaHalaharvi/Desktop/btyd_sales/") # Set your directory

toInstallCandidates <- c("ggplot2", "BTYD", "reshape2", "plyr", "lubridate")
# check if pkgs are already present
toInstall <- toInstallCandidates[!toInstallCandidates%in%library()$results[,1]] 
if(length(toInstall)!=0)
{install.packages(toInstall, repos = "http://cran.r-project.org")}
# load pkgs
lapply(toInstallCandidates, library, character.only = TRUE)

################################################################ 

#LOAD DATA

################################################################  


elog <- read.csv("btyd_0Sales_20140731Date.csv", header = TRUE) #load data

#Date format from character to date. 
elog$date <- as.character(elog$date)
elog$date <- as.Date(elog$date, "%Y%m%d") 

summary(elog)
# Transaction-flow models, such as the Pareto/NBD, are concerned
# with interpurchase intervals. 
# Since we only have dates and there may be multiple purchases on a day
# we merge all transactions that occurred on the same day 
# using dc.MergeTransactionsOnSameDate() 

elog <- dc.MergeTransactionsOnSameDate(elog)
head(elog)
summary(elog)  # no NAs

################################################################ 

#EXAMINE DATA

################################################################  


# make log plot and plot sales

ggplot(elog, aes(x=date,y=sales,group=cust))+
  geom_line(alpha=0.2, colour="#000099")+
  scale_x_date()+
  scale_y_log10()+
  ggtitle("Sales for individual customers")+
  ylab("Sales ($, US)")+xlab("")+
  theme_minimal() 

# look at days between orders
# model describes rates via a gamma distribution across customers

purchaseFreq <- ddply(elog, .(cust), summarize, 
                      daysBetween = as.numeric(diff(date)))
###View the file
# write.csv(purchaseFreq, file = "daysBetweenPurchases.csv")

time.btw.purchases.days <- windows();ggplot(purchaseFreq,aes(x=daysBetween))+
  geom_histogram(fill="orange")+
  xlab("Time between purchases (days)")+
  theme_minimal()
summary(purchaseFreq)
sum(elog$sales)
################################################################ 

#DIVIDE DATA 

################################################################  

# into a calibration phase
# and a holdout phase

# determine middle point for splitting.
# below gives the midpoint date 
(end.of.cal.period <-
   min(elog$date)+as.numeric((max(elog$date)-min(elog$date))/2))

# split data into train(calibration) and test (holdout) and make matrices
data <- dc.ElogToCbsCbt(elog, per="week", 
                        T.cal=end.of.cal.period,
                        merge.same.date=TRUE, # not needed, we already did it
                        statistic = "freq")   # which CBT to return 

#  write.csv(data, file = "data.csv")

# take a look
str(data) 

# cbs is short for "customer-by-sufficient-statistic" matrix
#               with the sufficient stats being: 
#                       frequency --> "x"
#                       recency (time of last transaction) and --> "t.x"
#                       total time observed --> "T.cal"

# extract calibration matrix
cal2.cbs <- as.matrix(data[[1]][[1]])
str(cal2.cbs)
#write.csv(cal2.cbs, file = "cal2_cbs.csv")

################################################################ 

# ESTIMATE PARAMETERS FOR THE MODEL

################################################################ 


# initial estimate
(params2 <- pnbd.EstimateParameters(cal2.cbs))

# look at log likelihood
(LL <- pnbd.cbs.LL(params2, cal2.cbs))

# make a series of estimates, see if they converge
p.matrix <- c(params2, LL)
for (i in 1:20) {
  params2 <- pnbd.EstimateParameters(cal2.cbs, params2)
  LL <- pnbd.cbs.LL(params2, cal2.cbs)
  p.matrix.row <- c(params2, LL)
  p.matrix <- rbind(p.matrix, p.matrix.row)
}

# examine. Does this converge?
p.matrix # for data set. Yes, it converges 
# write.csv(p.matrix, file = "p_matrix.csv")

# use final set of values
(params2 <- p.matrix[dim(p.matrix)[1],1:4])

#for note above.
# Main model params are :

# r          gamma parameter for NBD transaction 
# alpha      gamma parameter for NBD transaction 
# s          gamma parameter for Pareto (exponential gamma) dropout process
# beta       gammma parameter for Pareto (exponential gamma) dropout process 

################################################################ 

# PLOT LOG-LIKELIHOOD ISO-CONTOURS FOR MAIN PARAMS

################################################################

# set up parameter names for a more descriptive result
param.names <- c("r", "alpha", "s", "beta")

LL <- pnbd.cbs.LL(params2, cal2.cbs)

dc.PlotLogLikelihoodContours(pnbd.cbs.LL, params2, cal.cbs = cal2.cbs , n.divs = 5,
                             num.contour.lines = 7, zoom.percent = 0.3,
                             allow.neg.params = FALSE, param.names = param.names) 
################################################################ 

# PLOT GROUP DISTRIBUTION OF PROPENSITY TO PURCHASE, DROPOUT

################################################################ 

# par to make two plots side by side
par(mfrow=c(1,2))

# Plot the estimated distribution of lambda 
# (customers' propensities to purchase)
pnbd.PlotTransactionRateHeterogeneity(params2, lim = NULL)
# lim is upper xlim
# Plot estimated distribution of gamma 
# (customers' propensities to drop out).

pnbd.PlotDropoutRateHeterogeneity(params2)

# set par to normal
par(mfrow=c(1,1))

################################################################ 

# EXAMINE INDIVIDUAL PREDICTIONS 

################################################################  

# estimate number transactions a new customer 
# will make in 52 weeks
pnbd.Expectation(params2, t = 52)

# expected characteristics for a specific individual, 
# conditional on their purchasing behavior during calibration

# calibration data for customer 1516
# frequency("x"), recency("t.x") and total time observed("T.cal")

cal2.cbs["1",]
x <- cal2.cbs["1", "x"]         # x is frequency
t.x <- cal2.cbs["1", "t.x"]     # t.x is recency, ie time of last transactions
T.cal <- cal2.cbs["1", "T.cal"] # T.cal is total time observed

# estimate transactions in a T.star-long duration for that cust
pnbd.ConditionalExpectedTransactions(params2, T.star = 52, # weeks
                                     x, t.x, T.cal)
#Answer:[1] 84.88063


################################################################ 

# PROBABILITY A CUSTOMER IS ALIVE AT END OF CALIBRATION / TRAINING 

################################################################ 


x           # freq of purchase
t.x         # week of last purchase
T.cal <- 36.5 # week of end of cal, i.e. present
pnbd.PAlive(params2, x, t.x, T.cal)

# To visualize the distribution of P(Alive) across customers:
params3 <- pnbd.EstimateParameters(cal2.cbs)
p.alives <- pnbd.PAlive(params3, cal2.cbs[,"x"], cal2.cbs[,"t.x"], cal2.cbs[,"T.cal"])

ggplot(as.data.frame(p.alives),aes(x=p.alives))+
  geom_histogram(colour="grey",fill="orange")+
  ylab("Number of Customers")+
  xlab("Probability Customer is 'Live'")+
  theme_minimal()

# plot actual & expected customers binned by num of repeat transactions
pnbd.PlotFrequencyInCalibration(params2, cal2.cbs, 
                                censor=21, title="Model vs. Reality during Calibration")

################################################################ 

# HOW WELL DOES MODEL DO IN HOLDOUT PERIOD?

################################################################ 


# get holdout transactions from dataframe data, add in as x.star

x.star   <- data[[2]][[2]][,1]
cal2.cbs <- cbind(cal2.cbs, x.star)
str(cal2.cbs)

holdoutdates <- attributes(data[[2]][[1]])[[2]][[2]]
holdoutlength <- round(as.numeric(max(as.Date(holdoutdates))-
                                    min(as.Date(holdoutdates)))/7)

# plot predicted vs seen conditional freqs and get matrix 'comp' w values

T.star <- holdoutlength
censor <- 1 # Bin all order numbers here and above
comp <- pnbd.PlotFreqVsConditionalExpectedFrequency(params2, T.star,
                                                    cal2.cbs, x.star, censor)
rownames(comp) <- c("act", "exp", "bin")
comp

# plot predicted vs actual by week

# get data without first transaction, this removes those who buy 1x
removedFirst.elog <- dc.SplitUpElogForRepeatTrans(elog)$repeat.trans.elog
removedFirst.cbt <- dc.CreateFreqCBT(removedFirst.elog)

# get all data, so we have customers who buy 1x
allCust.cbt <- dc.CreateFreqCBT(elog)

# write.csv(allCust.cbt, file ="allCust_cbt.csv") #All customers are in here. Total customers = 310. 

# add 1x customers into matrix
tot.cbt <- dc.MergeCustomers(data.correct=allCust.cbt, 
                             data.to.correct=removedFirst.cbt)  #All customers are in here. Total customers = 310. 
#write.csv(tot.cbt, file = "tot_cbt.csv")

lengthInDays <- as.numeric(max(as.Date(colnames(tot.cbt)))-
                             min(as.Date(colnames(tot.cbt))))
origin <- min(as.Date(colnames(tot.cbt)))

tot.cbt.df <- melt(tot.cbt,varnames=c("cust","date"),value.name="Freq")
tot.cbt.df$date<-as.Date(tot.cbt.df$date)
tot.cbt.df$week<-as.numeric(1+floor((tot.cbt.df$date-origin+1)/7))

transactByDay  <- ddply(tot.cbt.df,.(date),summarize,sum(Freq))
transactByWeek <- ddply(tot.cbt.df,.(week),summarize,sum(Freq))
names(transactByWeek) <- c("week","Transactions")
names(transactByDay)  <- c("date","Transactions")

T.cal <- cal2.cbs[,"T.cal"]
T.tot <- 73 # end of holdout
comparisonByWeek <- pnbd.PlotTrackingInc(params2, T.cal,
                                         T.tot, actual.inc.tracking.data=transactByWeek$Transactions)

################################################################ 

#FORMAL MEASURE OF ACCURARY 

################################################################ 


# root mean squared error
rmse <- function(est, act) { return(sqrt(mean((est-act)^2))) }

# mean squared logarithmic error
mlse <- function(est, act) { return(mean((log1p(est)-log1p(act))^2)) }

str(cal2.cbs)

cal2.cbs[,"x"]

predict<-pnbd.ConditionalExpectedTransactions(params2, T.star = 37, # weeks
                                              x     = cal2.cbs[,"x"], 
                                              t.x   = cal2.cbs[,"t.x"], 
                                              T.cal = cal2.cbs[,"T.cal"])

cal2.cbs[,"x.star"]  # actual transactions for each person

FinalPredictionTransactions <- cal2.cbs[,"x.star"]
write.csv(FinalPredictionTransactions, file = "FinalPredictionTransactionDemo.csv") 
head(FinalPredictionTransactions)
#1) 1st Column contains "Customer IDs"
#2) 2nd Column contains "Predicted Number of Transactions"

head(cal2.cbs[,"x.star"])

# rmse(act=cal2.cbs[,"x.star"],est=predict) # Room Mean Squared Error
# mlse(act=cal2.cbs[,"x.star"],est=predict) # Maximum Likelihood Sequence Estimator

