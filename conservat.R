
#-------------------------------------------------------------------------------#
#    Objective:	Program to get Optimum 'Conservative' PF  				   		#
#				for various cases ...											#
#			a) horizon = 12, 6, 24 (in months)									#
#			b) group = 4 & 4.1 (max individual fund wt 80% & 40%, resp) 		#
#			c) method = "Basic", "Shrinkage", "MVE", etc. 						#
#																				#
#    Start:		Aug 25, 2014							     					#
#    End:		Sep 15, 2014													#
#    R Version:	3.1.0 (2014-04-10) -- "Spring Dance"					     	#
#    Dependencies & Input files: 												#
#		RData: ret0.RData, lgnd.Rdata, bmDebtret.RData							#
#		Rcode: UsefulFunctionsV3_18.R"												#
#		Top-level Packages: xts, timeSeries, PerformanceAnalytics, fPortfolio	#
#	 Author: 	Debapriya Ghosh						     				#
#-------------------------------------------------------------------------------#
#	Part A: Load R-data, R-functions from local files							#
#	Part B: Obtain various types of Opt-PF (saves results as data and plots)	#
#	Part C: Obtain Cumulative Return plots for various optimal PF strategies 	#
#	Part D: Obtain Quarterly Return plots for various optimal PF strategies 	#
#	Part E: Obtain Overall Perf. Summary plots for various Opt PF strategies 	#
#-------------------------------------------------------------------------------#
#------------------- Part A starts ---------------------------------------------#
#	Part A: load R-data, R-functions from local files
require(xts) # load xts package 

# load return data for MFs ...
####load(paste("..\\..\\02ProcData\\ret0", ".RData", sep="")) 

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
####load(paste("..\\08.01 Data\\bmDebtret", '.Rdata', sep='') )  

load("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMret.Rdata")
head(bmret) # checking the benchmark data
attach(bmret)
bmdebtret = bmret[,c("CRISIL.Composite.Bond.Fund.Index","CRISIL.Liquid.Fund.Index","CRISIL.Short.Term.Bond.Fund.Index")]
bmdebt <- xts(bmdebtret,order.by=as.Date(rownames(bmdebtret)))

#-------------------------------------------------------------------------------#
#	All core R-code developed so far were cleaned, improved, and saved as functions for 
#	better readability & revision tracking 				
#	They are saved in the R script file 'UsefulFunctionsV3.R' 
#-------------------------------------------------------------------------------#
setwd("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Code/Conservative_18/Codes")
source(file="UsefulFunctionsV2_18.R")

#------------------- Part A ends ---------------------------------------------#

#------------------- Part B starts ---------------------------------------------#	
#	Part B: obtain various types of Opt-PF (saves results as data and plots)	#
#-------------------------------------------------------------------------------#
#	Using 1-year, 2-year, and 6-month windows to build 							#
#		Conservative PF Frontier & Basic Opt PF									#
#		with individual fund wt no more than 80% (group = 4)					#
#-------------------------------------------------------------------------------#
# SMart_Wrapper_ConservativeOptPF (hor = 12, group = 4, method = "Basic", token = "optCPF3h1yr")
# SMart_Wrapper_ConservativeOptPF (hor = 15, group = 4, method = "Basic", token = "optCPF3h2yr")   ##error
# SMart_Wrapper_ConservativeOptPF (hor =  6, group = 4, method = "Basic", token = "optCPF3h6mo")

tme = BackTestingTimeFrames(horizon = 12)
tme = BackTestingTimeFrames(horizon = 6)
#tme = BackTestingTimeFrames(horizon = 21)

# let us also do it for Robust with 6-mo case 
####SMart_Wrapper_ConservativeOptPF (hor =  6, group = 4, method = "Shrinkage", token = "optCPF5h6mo")

#-------------------------------------------------------------------------------#
#	Using 1-year and 6-month windows to build 									#
#		Conservative PF Frontier & Basic Opt PF									#
#		with individual fund wt no more than 40% (group = 4.1)					#
#-------------------------------------------------------------------------------#
SMart_Wrapper_ConservativeOptPF(hor = 12, group = 4.3, method = "Basic", token = "optCPF3_1h1yr")
SMart_Wrapper_ConservativeOptPF(hor =  6, group = 4.3, method = "Basic", token = "optCPF3_1h6mo")

#-------------------------------------------------------------------------------#
#	Using 1-year and 6-month windows to build 									#
#		Conservative PF Frontier & Basic Opt PF									#
#		with individual fund wt no more than 35% (group = 4.2)					#
#		Note: making the cap below 35% yielding situation when Opt-PF cannot be #
#		computed in some situations. 											#
#-------------------------------------------------------------------------------#
####SMart_Wrapper_ConservativeOptPF (hor = 12, group = 4.2, method = "Basic", token = "optCPF3_2h1yr")
####SMart_Wrapper_ConservativeOptPF (hor =  6, group = 4.2, method = "Basic", token = "optCPF3_2h6mo")

# let us also do it for Robust with 6-mo case 
####SMart_Wrapper_ConservativeOptPF (hor =  6, group = 4.2, method = "Shrinkage", token = "optCPF5_2h6mo")

#-------------------------------------------------------------------------------#
#	Using 6-month windows to build 												#
#		Conservative PF Frontier & Robust (Shrinkage) Opt PF					#
#		with individual fund wt no more than 40% (group = 4.1)					#
#-------------------------------------------------------------------------------#
####SMart_Wrapper_ConservativeOptPF (hor =  6, group = 4.1, method = "Shrinkage", token = "optCPF5_1h6mo")
####SMart_Wrapper_ConservativeOptPF (hor =  6, group = 4.1, method = "MVE", token = "optCPF4_1h6mo")

#------------------- Part B ends   ---------------------------------------------#

#------------------- Part C starts ---------------------------------------------#
#	Part C: Obtain Cumulative Return plots for various optimal PF strategies 	#
#------------------- Part C starts ---------------------------------------------#
#myPF_CumReturns(myPFstrList = c("optCPF3h1yr", "optCPF3h6mo"), bmRet = bmdebt, prefix="Conservative")

myPF_CumReturns(myPFstrList = c("optCPF3_1h1yr", "optCPF3_1h6mo"), bmRet = bmdebt, prefix="Conservative")

# myPF_CumReturns(myPFstrList = c( "optCPF3_1h6mo","optCPF5_1h6mo"), bmRet = bmdebt, prefix="Conservative_2")
# 
# myPF_CumReturns(myPFstrList = c( "optCPF3_1h6mo","optCPF5_1h6mo","optCPF4_1h6mo"), bmRet = bmdebt, prefix="Conservative_3")
# 
# myPF_CumReturns(myPFstrList = c( "optCPF3_1h6mo","optCPF5_1h6mo","optCPF3_2h6mo"), bmRet = bmdebt, prefix="Conservative_4")

#------------------- Part C ends   ---------------------------------------------#

#------------------- Part D starts ---------------------------------------------#
#	Part D: Obtain Quarterly Return plots for various optimal PF strategies 	#
#------------------- Part D starts ---------------------------------------------#

myPF_QrtReturns(myPFstrList = c("optCPF3_1h1yr", "optCPF3_1h6mo"),ttl = "Comparing Quarterly Returns for CPF3 (@H= 1yr, 6mo)",bmRet = bmdebt, prefix="Conservative")

#------------------- Part D ends   ---------------------------------------------#
#------------------- Part E starts ---------------------------------------------#
#	Part E: Obtain Overall Perf. Summary plots for various Opt PF strategies 	#
#			by changing risk parameter we get plot for all 3 risk measures		#
#------------------- Part E starts ---------------------------------------------#

myPF_Overall_Performance(myPFstrList = c("optCPF3_1h1yr", "optCPF3_1h6mo"),ttl = "Overall Performance of CPF3 (@H= 1yr, 6mo)",risk = "SD",	bmRet = bmdebt, prefix="Conservative")

myPF_Overall_Performance(myPFstrList = c("optCPF3_1h1yr", "optCPF3_1h6mo"),ttl = "Overall Performance of CPF3 (@H= 1yr, 6mo)",
                         risk = "CVaR",	bmRet = bmdebt, prefix="Conservative")

myPF_Overall_Performance(myPFstrList = c("optCPF3_1h1yr", "optCPF3_1h6mo"),ttl = "Overall Performance of CPF3 (@H= 1yr, 6mo)",
                         risk = "MaxDrawDown",	bmRet = bmdebt, prefix="Conservative")






