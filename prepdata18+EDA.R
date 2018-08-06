#-------------- Reading Funds NAV data ---------------------------------#

mf <- read.csv("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Data/CSV/FundsNav.csv")
lgnd <- read.csv("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Data/CSV/Fundslgnd.csv")
head(mf)

head(lgnd)
chk<-table(mf[,1])

sort(chk)
dim(mf)
head(mf[,1:5])
rownames(mf) <- as.Date(as.character(mf[,1]),"%d-%b-%y")  
mf<-mf[,-1]
head(mf[,1:5])
#------Need to now handle weekend & holidays in the NAV ---------------#
  
HandleRowNA <- function(dta,cut=30)
{
  # determine rows to be deleted due to massive NAs
  chk <-apply(dta,1,function(x) round(sum(is.na(x))/length(x)*100,1))
  dtaGood<-dta[chk<cut,]      # any row having xx% or missing considered missing ##
  print(dim(dta))	
  print(dim(dtaGood))
  dtaGood
}

mf <- HandleRowNA(mf,30)  # drop dates when 60% or more funds did not report NAVs

write.csv(mf,"C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Data/CSV/Newdata.csv")

#------- Computing daily log-returns of MFs  -----------#  

logRetMF<-function(mfdta)
{
  if (is.null(mfdta))return(cat("No NAV data set is provided"))
  require(PerformanceAnalytics)
  retMF<-Return.calculate(mfdta, method="log")
  retMF<-retMF[-1,] # 1st row is by definition NA's
  return(retMF)
}

ret0 <- logRetMF(mfdta=mf)

save(ret0, file=paste("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/Fundsret", '.Rdata', sep = '') )
save(lgnd, file=paste("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/Fundslgnd", '.Rdata', sep='') )

sum(is.na(ret0))

head(ret0[,1:5])
dim(ret0)

#-------------- Reading raw Benchmark NAV data ---------------------------------#

bmnav <- read.csv("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Data/CSV/BMnavs.csv")
bmlgnd <- read.csv("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Data/CSV/Bnchmrk_lgnd.csv")
head(bmnav)

rownames(bmnav)<-as.Date(as.character(bmnav[,1]),"%d-%b-%y") # making the data an xts object
bmnav<-bmnav[,-1]
head(bmnav)

#---  Need to now handle weekend & holidays in the NAV --------------------------#
HandleRowNA<-function(dta,cut=30)
{
  # determine rows to be deleted due to massive NAs
  chk<-apply(dta,1,function(x) round(sum(is.na(x))/length(x)*100,1))
  dtaGood<-dta[chk<cut,] # any row having xx% or missing considered missing
  print(dim(dta))	
  print(dim(dtaGood))
  dtaGood
}
bm<-HandleRowNA(bmnav, 30) # drop dates when 30% or more funds did not report NAVs

#---  Need to now compute log-Returns of the Benchmark indices --------------------#

logRetMF<-function(mfdta=NULL)
{
  if (is.null(mfdta)) return(cat("No NAV data set is provided"))
  
  require(PerformanceAnalytics)
  
  retMF<-Return.calculate(mfdta, method="log")
  retMF<-retMF[-1,] # 1st row is by definition NA's
  return(retMF)
}

bmret<-logRetMF(mfdta=bm)
head(bmret)

sum(is.na(bmnav))

bmret <-logRetMF(mfdta=bmnav)
head(bmret)

save(bmret, file=paste("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMret", '.Rdata', sep='') )
save(bmlgnd, file=paste("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMlgnd", '.Rdata', sep='') )


ret1 <- ret0
ret1[!complete.cases(ret1),]
ret1 <- na.omit(ret1)
sum(is.na(ret1))
dim(ret1)

library(fPortfolio)

RETS <-as.timeSeries(ret1)
class(RETS)

assetsBasicStatsPlot(RETS[, 1:8], title = "", description = "")
assetsBoxStatsPlot(RETS[, 1:8], title = "", description = "")

Assets <- assetsArrange(RETS[, 1:10], method = "hclust")
chk001HC <- 100 * RETS[, Assets]
assetsPairsPlot(chk001HC , pch = 19, cex = 0.5, col = "royalblue4")

assetsCorgramPlot(chk001HC, method = "shade", pch = 19, cex = 0.5)

library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(stringr)
library(memoise)
library(xts)
library(fPortfolio)

dta <- xts(ret1,order.by=as.Date(rownames(ret1)))

dim(dta)

library(fPortfolio) 
anAsset <- as.timeSeries(100*dta)

#Case 1: Minimizing the risk for a target return
#### Viewing specifications of a portfolio from scratch
spec.pf1 <- portfolioSpec() 
str(spec.pf1)

#### Checking whether specifications of the portfolio are according to our requirement
setOptimize(spec.pf1) <- "minRisk"
getType(spec.pf1)
getOptimize(spec.pf1) # 'minRisk' - Default specification - minimizes risk for a target return

assets <- as.data.frame(anAsset)

#### Setting equal weights to each fund in the portfolio 
nn <- ncol(assets) 
setWeights(spec.pf1) <- rep(1/nn, times = nn) 
wts <- getWeights(spec.pf1)

write.csv(wts,"C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Data/CSV/Initialwts.csv")


#### Finding a feasiblePortfolio given the default specifications (MV and minRisk) and equal weights
eqlwt.pf1 <- feasiblePortfolio(data = anAsset, spec = spec.pf1,	constraints = "LongOnly")
print(eqlwt.pf1)

#### Finding the Mean Variance efficientPortfolio with target return equal to that of equal-wt Portfolio
setTargetReturn(spec.pf1) <- getTargetReturn(eqlwt.pf1@portfolio)["mean"]
mnvar.pf1 <- efficientPortfolio(data = anAsset, spec = spec.pf1, constraints = "LongOnly")
print(mnvar.pf1)

#### Identifying the fund/asset names in the above portfolio having positive weights given the target return
sel.mnvar.pf1 <- colnames(anAsset)[round(getWeights(mnvar.pf1),3)>0]

#### Converting the weights of the identified fund/asset names into percentage
wts.mnvar.pf1 <- (round(getWeights(mnvar.pf1),3)[round(getWeights(mnvar.pf1),3)>0])*100 

# #### Corresponding to the identified fund/asset names merging all the columns of 'lgnd' dataset with the weights in the efficient portfolio
# MVOpt<-merge(lgnd[lgnd$Abbreviations %in% sel.mnvar.pf1, ],data.frame(nme=sel.mnvar.pf1,wts.mnvar.pf1), by.x="Abbreviations", by.y="nme")
# 
# #### Trimming the Fund name from the merged dataset
# MVOpt$Fund.Name <- substr(MVOpt[,2], 1,20)
# 
# #### Merged dataset constructed excluding the 'Comment' column
# MVOpt<-MVOpt[,c(1,2,3,4,6)]
# print(MVOpt)

##alternate code: to be followed##
RESULT <- cbind(lgnd[match(colnames(anAsset)[round(getWeights(mnvar.pf1),3)>0],as.character(lgnd$Legends)),],weight=(round(getWeights(mnvar.pf1),3)[round(getWeights(mnvar.pf1),3)>0])*100)

#------------ Trying to install required packages -----------------
#install.packages("fPortfolioSolver", repos="http://R-Forge.R-project.org")#, type="source")

#Case 2: Maximizing the return within a target risk

pf2 <- portfolioSpec()
str(pf2)

#### Setting equal weights to all funds/assets in the portfolio
nn <- ncol(anAsset)
setWeights(pf2) <- rep(1/nn, times = nn)
getWeights(pf2)

#setType(pf2)<-"MVO" #default
setOptimize(pf2) <-"maxReturn" #### Changing the Optimization specification from minRisk to maxReturn
#setTargetReturn(pf2) <- 0.025
#getTargetReturn(pf2)
#setTargetRisk(pf2)<-.7
getType(pf2)
getOptimize(pf2) #maxReturn ... default changed to maximize return

#### Finding a feasiblePortfolio with equal weights
eqlwt.pf2 <- feasiblePortfolio(data = anAsset, spec = pf2,	constraints = "LongOnly")
plot(eqlwt.pf2)
print(eqlwt.pf2)

# fPortfolioSolver must have been installed at this stage

require(fPortfolio)
# setSolver(pf2) <- "solveRsocp" 
# getSolver(pf2)
setTargetRisk(pf2) <- getTargetRisk(eqlwt.pf2@portfolio)["Sigma"] 
getTargetRisk(pf2)
mnvar.pf1 <- efficientPortfolio(data = anAsset, spec = pf2, constraints = "LongOnly")
mnvar.pf2 <- maxreturnPortfolio(data = anAsset, spec = pf2, constraints = "LongOnly")
print(mnvar.pf1)
print(mnvar.pf2)

sel.mnvar.pf1 <- colnames(anAsset)[round(getWeights(mnvar.pf1),4)>0]
sel.mnvar.pf2 <- colnames(anAsset)[round(getWeights(mnvar.pf2),4)>0]

wts.mnvar.pf1 <- (round(getWeights(mnvar.pf1),4)[round(getWeights(mnvar.pf1),4)>0])*100
wts.mnvar.pf2 <- (round(getWeights(mnvar.pf2),4)[round(getWeights(mnvar.pf2),4)>0])*100 


RESULT1 <- cbind(lgnd[match(colnames(anAsset)[round(getWeights(mnvar.pf1),4)>0],as.character(lgnd$Legends)),],weight=(round(getWeights(mnvar.pf1),4)[round(getWeights(mnvar.pf1),4)>0])*100)
RESULT2 <- cbind(lgnd[match(colnames(anAsset)[round(getWeights(mnvar.pf2),4)>0],as.character(lgnd$Legends)),],weight=(round(getWeights(mnvar.pf2),4)[round(getWeights(mnvar.pf2),4)>0])*100)












