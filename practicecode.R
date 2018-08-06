
mf1<-read.csv("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Shared info/20140618 Asset Allocation_SM/01RawData/NAVs.csv")
lgnd1<-read.csv("C:/Users/debapriyag/Desktop/Portfolio Opt_2018/Shared info/20140618 Asset Allocation_SM/01RawData/FundsLgnd.csv")

head(lgnd1)
#chk<-table(mf[,1])
#sort(chk)
dim(mf1)
head(mf1[,1:5])
rownames(mf1)<-as.Date(as.character(mf1[,1]),"%d-%b-%y") # making the data an xts object
#### rownames(mf)<-as.Date(mf[,1], origin="1899-12-30") # making the data an xts object
mf1<-mf1[,-1]
head(mf1[,1:5])

#---  Need to now handle weekend & holidays in the NAV --------------------------#

HandleRowNA <- function(dta,cut=30)
{
  # determine rows to be deleted due to massive NAs
  chk<-apply(dta,1,function(x) round(sum(is.na(x))/length(x)*100,1))
  dtaGood<-dta[chk<cut,] # any row having xx% or missing considered missing
  print(dim(dta))	
  print(dim(dtaGood))
  dtaGood
}
mf1 <- HandleRowNA(mf1, 30)            # drop dates when 30% or more funds did not report NAVs
-----------------------------------------------------------------------------------------------------
 
  
   #------ STEP 2: Computing daily log-returns of MFs  -----------  

logRetMF<-function(mfdta=NULL)
{
  if (is.null(mfdta))return(cat("No NAV data set is provided"))
  require(PerformanceAnalytics)
  retMF <- Return.calculate(mfdta, method="log")
  retMF <- retMF[-1,] # 1st row is by definition NA's
  return(retMF)
}
ret0 <-logRetMF(mfdta=mf1)

sum(is.na(ret0))

head(ret0[,1:5])
dim(ret0)








