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

#------Need to now handle weekend & holidays in the NAV --------------------------#
  
HandleRowNA <- function(dta,cut=50)
{
  # determine rows to be deleted due to massive NAs
  chk <-apply(dta,1,function(x) round(sum(is.na(x))/length(x)*100,1))
  dtaGood<-dta[chk<cut,]      # any row having xx% or missing considered missing ##
  print(dim(dta))	
  print(dim(dtaGood))
  dtaGood
}

mf <- HandleRowNA(mf, 50)  # drop dates when 60% or more funds did not report NAVs

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

bmnav <-HandleRowNA(bmnav,50)
sum(is.na(bmnav))

bmret <-logRetMF(mfdta=bmnav)
head(bmret)

save(bmret, file=paste("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMret", '.Rdata', sep = '') )
save(bmlgnd, file=paste("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Output/BMlgnd", '.Rdata', sep='') )






