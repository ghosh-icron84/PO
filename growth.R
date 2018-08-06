#-------------------------------------------------------------------------------#
#    Objective:	To create outputs as requested by Marketing						#
#																				#
#    Start:		Sep 22, 2014							     					#
#    R Version:	3.1.0 (2014-04-10) -- "Spring Dance"					     	#
#    Dependencies & Input files: 												#
#-------------------------------------------------------------------------------#
#------------------- loading data, libs etc ------------------------------------#
#	Part A: load R-data, R-functions from local files
require(xts) # load xts package 

# load return data for MFs ...
####load(paste("..\\..\\02ProcData\\ret0", ".RData", sep="")) 
# load various legend and auxiliary information of the MFs ...
####load(paste("..\\..\\02ProcData\\lgnd", ".RData", sep="")) 

library(lubridate)
library(Rcpp)
library(PortfolioAnalytics)

load("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/Fundsret.Rdata")
# load various legend and auxiliary information of the MFs ...
####load(paste("..\\..\\02ProcData\\lgnd", ".RData", sep="")) 

load("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/Fundslgnd.Rdata")
ret1<-ret0 # renaming the data for internal consistency 
head(ret1) # checking the return data
head(lgnd)

# load appropriate benchmark data ...
####load(paste("..\\..\\02ProcData\\bmret", ".RData", sep="")) 
#	load(paste("..\\08.01 Data\\bmDebtret", '.Rdata', sep='') )  

load("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMret.Rdata")
load("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMlgnd.Rdata")
head(bmret) # checking the benchmark data
attach(bmret)
bmret = bmret[, c("S.P.BSE.100","S.P.BSE.MID.CAP")]
bmeqt = xts(bmret, order.by = as.Date(rownames(bmret)))
#-------------------------------------------------------------------------------#
#	All core R-code developed so far were cleaned, improved, and saved as functions for 
#	better readability & revision tracking 				
#	They are saved in the R script file 'UsefulFunctionsV3.R' 
#---------------------------------------------------------------------------#
setwd("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Code/Growth_18/Codes")
source(file="UsefulFunctionsQ1_18.R")
#------------------- Part A ends   ---------------------------------------------#

#------------------- Part B starts ---------------------------------------------#	
#	Part B: obtain required  Opt-PF (saves results as data and plots)			#
#-------------------------------------------------------------------------------#
#	Using 1-year, and 6-month windows to build 									#
#		Aggressive PF (65-80% Equity Funds) Frontier & Basic Opt PF				#
#		with individual fund wt no more than 40% (group = 92)					#
#-------------------------------------------------------------------------------#
SMart_Wrapper_Q2OptPF (hor = 6, group = 92, method = "Basic", token = "optGPF3_1h6mo")
SMart_Wrapper_Q2OptPF (hor = 12, group = 92, method = "Basic", token = "optGPF3_1h1yr")

#------------------- Part B ends   ---------------------------------------------#

#------------------- Part C starts ---------------------------------------------#
#	Part C: Obtain Cumulative Return plots for various optimal PF strategies 	#
#------------------- Part C starts ---------------------------------------------#
myPF_CumReturns(myPFstrList = c("optGPF3_1h6mo", "optGPF3_1h1yr"), bmRet = bmeqt, prefix="Growth")
#------------------- Part C ends   ---------------------------------------------#

#------------------- Part D starts ---------------------------------------------#
#	Part D: Obtain Quarterly Return plots for various optimal PF strategies 	#
#------------------- Part D starts ---------------------------------------------#

myPF_QrtReturns(myPFstrList = c("optGPF3_1h6mo", "optGPF3_1h1yr"),
                ttl = "Comparing Quarterly Returns for GPF3 (@H= 1yr, 6mo)",bmRet = bmeqt, prefix="Growth")

#------------------- Part D ends   ---------------------------------------------#
#------------------- Part E starts ---------------------------------------------#
#	Part E: Obtain Overall Perf. Summary plots for various Opt PF strategies 	#
#			by changing risk parameter we get plot for all 3 risk measures		#
#------------------- Part E starts ---------------------------------------------#
myPF_Overall_Performance(myPFstrList = c("optGPF3_1h6mo", "optGPF3_1h1yr"),
                         ttl = "Overall Performance of GPF3 (@H= 1yr, 6mo)",
                         risk = "SD",	bmRet = bmeqt , prefix="Growth")

myPF_Overall_Performance(myPFstrList = c("optGPF3_1h6mo", "optGPF3_1h1yr"),
                         ttl = "Overall Performance of GPF3 (@H= 1yr, 6mo)",
                         risk = "CVaR",	bmRet = bmeqt , prefix="Growth")

myPF_Overall_Performance(myPFstrList = c("optGPF3_1h6mo", "optGPF3_1h1yr"),
                         ttl = "Overall Performance of GPF3 (@H= 1yr, 6mo)",
                         risk = "MaxDrawDown",	bmRet = bmeqt , prefix="Growth")

#------------------- Part E ends   ---------------------------------------------#

