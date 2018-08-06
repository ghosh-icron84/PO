#---------------------- Backtesting Portfolios ---------------------------------------------#
#    Objective:	To check 'Backtesting' performance of a PF strategy over time 			   	#
#    Start:		July 10, 2014							     								#
#    End:										  											#
#    R Version:	3.1.0 (2014-04-10) -- "Spring Dance"								     	#
#    R-Packages:fPortfolioBacktest												     		#
#    Author: 	Dr. S. Mukherjee									     					#
#-------------------------------------------------------------------------------------------#
#																							#
#	note:	need to write custom codes as the scenario does not exactly match with those 	#
#			implemented in off-the-shelf packages!											#
#-------------------------------------------------------------------------------------------#
SMart_PF = function(RET=NULL, start=NULL, end=NULL, dropcol=NULL, n_Frontier=5)
#	inputs:	time-horizon, global data, specifications on optimization						#
#	outputs:PF-composition: fund-name, wts (>0), PF-performance: return and risk			# 
{
#	1st: subset the return data to the time-horizon specified 
	if (is.null(RET)) stop("History of daily/monthly returns not specified")
	require(xts); require(timeSeries); require(fPortfolio)
	dta = xts(RET, order.by=as.Date(rownames(RET)))
	if (!is.null(start) & !is.null(end)) {
		dta = dta[paste(start,"/", end, sep="")]
		if(!is.null(dropcol)) {
			dropcol.no = match(dropcol,colnames(dta))
			dta = dta[, -dropcol.no]
		}
		dta1 = dta[!is.na(rowSums(dta)),] # making sure no NAs in the data
		if(NApc <- dim(dta1)[1]/dim(dta)[1]< 0.8) 
			{
				print(head(dta))
				stop (paste("too many NAs (",round(NApc*100,1),"%) in the data",sep=""))
			}
	}
	else stop("start & end must be specified in correct format")
#	print(head(dta1))
#	print(tail(dta1))
#	2nd: apply the data to obtain the Optimum PF 
#	TODO: currently only implementing simple MVO with target return same as equal-wt PF 
	my_asset = as.timeSeries(dta1*100)
	spec.pf = portfolioSpec() 
	setOptimize(spec.pf) = "minRisk"
	setType(spec.pf) = "MV"
	setTargetReturn(spec.pf) = mean (my_asset)
	print(mean(my_asset))
	opt.pf = efficientPortfolio(data = my_asset, spec = spec.pf, constraints = "LongOnly") 
	print(opt.pf)
	
	setNFrontierPoints(spec.pf)<-n_Frontier
	frontier.pf<-portfolioFrontier(my_asset, spec.pf)
	print(frontier.pf)
	#frontierPlot(object=frontier.pf, pch=19, cex=0.5)
	#plot(frontier.pf)
	opt.pf.wts = (round(getWeights(opt.pf),3)[round(getWeights(opt.pf),3)>0])*100
	names(opt.pf.wts) = colnames(my_asset)[round(getWeights(opt.pf),3)>0] 
	print(opt.pf.wts)
	frontier.pf
 }

load(paste("..\\02ProcData\\ret0", ".RData", sep=""))
load(paste("..\\02ProcData\\lgnd", ".RData", sep=""))
ret1<-ret0
  
chk = SMart_PF(RET=ret1, start="2011-01-01", end="2011-12-31", dropcol="FTCBO", n_Frontier=35)  # this data has some issues
chk = SMart_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", n_Frontier=35) 
fullFrontier=frontierPoints(chk, frontier = "lower",  return = "mean", risk = "Cov", auto = F)
chk.stat = getStatistics(chk)
plot(diag(chk.stat$Cov), chk.stat$mean,pch = 19, cex = 1.5, col = topo.colors(6), xlim=c(-0.1,1.2), ylim=c(-0.04, 0.2), xlab="Risk[Cov]", ylab="Return[mean]")
  abline(h = 0, col = "grey")
   abline(v = 0, col = "grey")
points(getTargetRisk(chk@portfolio)[,"Cov"], getTargetReturn(chk@portfolio)[,"mean"], type="l", lty=2, col="grey")
plot(fullFrontier)
plot(chk)

chk = SMart_PF(RET=ret1, start="2011-04-01", end="2012-03-31", dropcol="FTCBO", n_Frontier=10) 
SMart_PF(RET=ret1, start="2011-07-01", end="2012-06-30", dropcol="FTCBO") 
SMart_PF(RET=ret1, start="2011-10-01", end="2012-09-30", dropcol="FTCBO")  
SMart_PF(RET=ret1, start="2012-01-01", end="2012-12-31")  

round(getTargetReturn(chk@portfolio)[,"mean"],3) # gives all the returns for the frontier
round(getTargetRisk(chk@portfolio)[,"Cov"],3) # gives all the risks for the frontier
round(getWeights(chk@portfolio)[16:19,],2)  # gives the weights for the top few returns of the frontier
#-------------------
#------------------- 16.07.2014 -- plots needed to create the presentation -------------------------------------------------
chk = SMart_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", n_Frontier=35) 
fullFrontier=frontierPoints(chk, frontier = "both",  return = "mean", risk = "Cov", auto = F)
chk.stat = getStatistics(chk)
rsk = sqrt(diag(chk.stat$Cov))
tpe = as.character(lgnd[match(names(rsk),lgnd$Abbreviations), "Asset.Type"])
pchl = rep(19, length(rsk)); pchl[tpe=="Debt+Equity"]<-17; pchl[tpe=="Debt"]<-15;
coll = rep(topo.colors(6)[2], length(rsk)); coll[tpe=="Debt+Equity"]<-topo.colors(6)[4]; coll[tpe=="Debt"]<-topo.colors(6)[6]
rwd = chk.stat$mean
#plot(rsk, rwd ,pch = 19, cex = 1.2, col = topo.colors(6), xlim=c(-0.1,1.2), ylim=c(-0.04, 0.2), xlab="Risk=SD", ylab="Reward =E[Return]")
plot(rsk, rwd ,pch = pchl, cex = 1.2, col = coll, xlim=c(-0.1,1.2), ylim=c(-0.04, 0.2), xlab="Risk=SD", ylab="Reward =E[Return]")
  abline(h = 0, col = "grey")
   abline(v = 0, col = "grey")
   legend(0.05, .2, pch= c(19, 15, 17), col = topo.colors(6)[c(2,6,4)],text.col=  topo.colors(6)[c(2,6,4)], legend=c( "Equity Funds", "Debt Funds", "Mixed Funds"))
#points(getTargetRisk(chk@portfolio)[,"Cov"], getTargetReturn(chk@portfolio)[,"mean"], type="l", lty=2, lwd=2,col="darkblue")
   points(0.3464, 0.0795, pch= 15, cex=2, col="grey")
   points(0.2012, 0.0795, pch= 15, cex=2, col="orange")
      points(0.4476, 0.12, pch= 17, cex=2, col="orange")
points(fullFrontier,type="l", lty=2, lwd=2,col="grey" )
#---------------------------------------------------------------------------------------------------------------
