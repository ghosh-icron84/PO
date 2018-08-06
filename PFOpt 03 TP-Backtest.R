#---------------------------------------------------------------------------#
#---------------------- Backtesting of Tangency Portfolios -----------------#
#    Objective:	Program to obtain Backtesting of Tangency PF				#
#    Start:		July 24, 2014										     	#
#    End:															  		#
#    R Version:	3.1.0 (2014-04-10) -- "Spring Dance"						#
#    R-Packages:fPortfolioBacktest											#
#    Author: 	Dr. S. Mukherjee									     	#
#---------------------------------------------------------------------------#


#--- first: construction of Tangency PF		--------------------------------#
Tangency_PF = function(RET=NULL, start=NULL, end=NULL, dropcol=NULL, RF =0,
					n_Frontier=5, col.lgnd=NULL, grp=0)
#	inputs:	global data, time-horizon, specifications on optimization		#
#	outputs:PF-frontier	plot and Tangency PF								# 
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
	
	setNFrontierPoints(spec.pf)<-n_Frontier
	
	if (grp==1)  { # only group constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Asset.Type[ match(colnames(my_asset), col.lgnd$Abbreviations)]
		pfConstraints = c("LongOnly", 
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.6")
		)
	}
	else if (grp==2)  { # adding fund constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Asset.Type[ match(colnames(my_asset), col.lgnd$Abbreviations)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.3",sep=""),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.6")
		)
	}
	else  pfConstraints = "LongOnly"

	setRiskFreeRate(spec.pf) <- RF
	tang.pf<-tangencyPortfolio(my_asset, spec.pf, constraints = pfConstraints)
	
	#print(str(spec.pf)) 
	frontier.pf<-portfolioFrontier(my_asset, spec.pf, constraints = pfConstraints)
#	print(frontier.pf)
#	print(pfConstraints)
	frontierPlot(frontier.pf,  xlim=c(-0.05,1.2), ylim=c(-0.01, 0.2))
	tangencyLines(frontier.pf, col="blue")	
	abline(h = 0, col = "grey")
	abline(v = 0, col = "grey")
	text(-0.01,RF, RF, cex=0.7, col="blue", adj=1)
	tangencyPoints(frontier.pf, col="blue", pch=18, cex=1.2)
#	opt.pf.wts = (round(getWeights(opt.pf),3)[round(getWeights(opt.pf),3)>0])*100
#	names(opt.pf.wts) = colnames(my_asset)[round(getWeights(opt.pf),3)>0] 
#	print(opt.pf.wts)
	tang.pf
 }

 
	load(paste("..\\02ProcData\\ret0", ".RData", sep=""))
	load(paste("..\\02ProcData\\lgnd", ".RData", sep=""))
	ret1<-ret0

#-- tangency portfolios unconstrained/grp.-constrained/grp&fund-constrained -------------------
#--		with various value of risk free rate --------------------------------------------------	
	tpf0.0 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", 
						n_Frontier=35)  # returns the TP with RF=0 from unconstrained frontier
	tpf0.1 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.04,
						n_Frontier=35)  # returns the TP with RF=xx% from unconstrained frontier
	tpf0.2 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.07,
						n_Frontier=35)  # returns the TP with RF=xx% from unconstrained frontier

						
	tpf1.1 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.04,
						n_Frontier=35, grp=1, col.lgnd=lgnd)  # returns the TP with RF=xx% from grp constrained frontier
	tpf1.2 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.07,
						n_Frontier=35, grp=1, col.lgnd=lgnd)  # returns the TP with RF=xx% from grp constrained frontier						
	tpf2.1 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.04,
						n_Frontier=35, grp=2, col.lgnd=lgnd)  # returns the TP with RF=xx% from grp constrained frontier
					
	tpf2.2 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.07,
						n_Frontier=35, grp=2, col.lgnd=lgnd)  # returns the TP with RF=xx% from grp constrained frontier
						
	summary.Tangency_PF = function(tn.pf, lbl1=NULL, lbl2 = NULL)
	{
		wts0 = getWeights(tn.pf@portfolio); wts = wts0[wts0>0]; wts= round(wts*100,1)
		perf = c(getTargetReturn(tn.pf@portfolio)["mean"], getTargetRisk(tn.pf@portfolio)[c("Cov", "CVaR")])
		perf = round(perf*100,2)
		RF = round(getRiskFreeRate(tn.pf)*100,2) #sprintf("%.2f%s", round(getRiskFreeRate(tn.pf)*100,2),"%")
		smry = t(data.frame(c(RF, perf, wts)))
		colnames(smry) = c("Rf", names(perf), names(wts))
		rownames(smry) = paste(lbl1,lbl2)
		smry
	}
		
	summary.Tangency_PF	(tpf0.1, "Unconstrained", "RF=4%")
	summary.Tangency_PF	(tpf1.1, "Group-constr.", "RF=4%" )
	summary.Tangency_PF	(tpf2.1, "Gr/Fnd-constr.", "RF=4%")

##	Below are the performance summary and weights of Tangency PF during the period it was computed	
##                     Rf mean  Cov  CVaR BSLMT BSLSO DBRMC IPCCP IDFPE LTGIL MIAEB REEOP REMIP RESCF
## Unconstrained RF=4%  4 5.87  9.03 12.91  7.1  55.4   0.1   3.8   0.2  24.5   4.8     2         2.2
## Group-constr. RF=4%  4 7.91 20.56 34.76  4.2         2.9    29        45.8   5.9   3.4   1.5   7.4
## Gr/Fnd-constr.RF=4%  4 7.16 16.71 27.37   20         1.9  26.4          30   4.5   1.6    10   5.7

#----------------------------------------------------------------------------------------------------- 

## Now to find performance of these tangency PF on a 'future' time period

	Select_RET = function(RET=NULL, start=NULL, end=NULL, dropcol=NULL)
	{
	#	1st: subset the return data to the time-horizon specified 
		if (is.null(RET)) stop("History of daily/monthly returns not specified")
		require(xts); require(timeSeries)
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
		as.timeSeries(dta1*100)
	}


	projectedPerformance = function(a.pf, RET)
	{
		wts = getWeights(a.pf@portfolio); wts = wts[wts>0]
		dta = RET[,match(names(wts), names(RET))]
		require(PerformanceAnalytics)
		dta = xts(dta/100, order.by=as.Date(rownames(dta)))
		fnd.ret = t(table.AnnualizedReturns(dta, scale=252))[,1]
		print(wts)
		print(fnd.ret)
		sum(fnd.ret*wts)
	}
	
#	anAsset = Select_RET(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO")
	testAsset = Select_RET(RET=ret1, start="2013-01-01", end="2013-01-31", dropcol="FTCBO")
	testAsset = Select_RET(RET=ret1, start="2014-01-01", end="2014-01-31", dropcol="FTCBO")
	tpf2.1 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.04,
						n_Frontier=35, grp=2, col.lgnd=lgnd)  # recommended PF to use in Jan-2012	
	tpf2.1 = Tangency_PF(RET=ret1, start="2010-01-01", end="2010-12-31", dropcol="FTCBO", RF = 0.0,
						n_Frontier=35, grp=2, col.lgnd=lgnd)  # recommended PF to use in Jan-2012	
	projectedPerformance(tpf2.1, testAsset) # 11.6% return
	tpf2.1a = Tangency_PF(RET=ret1, start="2013-01-01", end="2013-12-31", dropcol="FTCBO", RF = 0.0,
						n_Frontier=35, grp=2, col.lgnd=lgnd)  # perfect foresight case 
	projectedPerformance(tpf2.1a, testAsset) # 15.6% return 

	range(rownames(ret1))

####---- 28-07-2014 ---------------------------------------------------------------------------------
	
	a.pf = tpf2.1
	wts = getWeights(a.pf@portfolio); wts = wts[wts>0]
	dta = testAsset[,match(names(wts), names(testAsset))]
#	dta = xts(dta/100, order.by=as.Date(rownames(dta)))
	dim(dta)
	length(wts)
	dta1 = dta %*% wts
	dta1 = xts(dta1/100, order.by=as.Date(rownames(dta)))
	table.AnnualizedReturns(dta1, scale=252)*100
#----------------------------------------------------------------------------------------------

	pfBacktest.atom = function(a.pf, RET)
	{
		wts = getWeights(a.pf@portfolio); wts = wts[wts>0]
		dta = RET[,match(names(wts), names(RET))]
		dta1 = dta %*% wts
		dta1 = xts(dta1/100, order.by=as.Date(rownames(dta)))
		print(252*mean(dta1))
		print(exp(252*mean(log(1+dta1)))-1)
		require(PerformanceAnalytics)
		perf = table.AnnualizedReturns(dta1, scale=252)
		print(table.DownsideRisk(dta1, scale=252))
		chart.CumReturns(dta1)
		chart.Drawdown(dta1)
		print(ES(dta1)) # this is nothing but approx. = (quantile(dta1, 0.05))
		#print(wts)
		perf
	}


	tpf2.1 = Tangency_PF(RET=ret1, start="2012-01-01", end="2012-12-31", dropcol="FTCBO", RF = 0.04,
			n_Frontier=35, grp=2, col.lgnd=lgnd)  # recommended PF to use in Jan-2012	

	testAsset = Select_RET(RET=ret1, start="2013-01-01", end="2013-01-31", dropcol="FTCBO")

	pfBacktest.atom(tpf2.1, testAsset) # 15.6% return 

#----------------------------------------------------------------------------------------------------
	# creating time frames for Backtesting 
	# using 1 yr as length of time horizon (h) and  
	# using 1 month for testing period (Delta-h) as well as increment/shift of h 
	require(lubridate)
	# ymd function parses dates in year-month-day format
	startDate = ymd('2009-07-01')
	# The %m+% adds months to dates without exceeding the last day
	hstarts = as.Date( startDate %m+% months(c(0:47)))
	hends = as.Date((startDate  %m+% months(12)) %m+% months(c(0:47))) -1
	Delhstr = hends +1
	Delhend = hends %m+% months(1)
	
	
#-----------------------------------------------------------------------------------------------------
	# sub-setting the return data for the specified data range and handles NAs: 28-07-2014
	Select_RET_V2 = function(RET=NULL, start=NULL, end=NULL, cut=0.05, dbg=0)
	{
	#	1st: subset the return data to the time-horizon specified 
		if (cut > 0.2) stop ("cut value must be < 0.2")
		if (cut < 0.0001) stop ("cut value must be > 0.0001")
		if (is.null(RET)) stop("History of daily/monthly returns not specified")
		require(xts); # require(timeSeries)
		dta = xts(RET, order.by=as.Date(rownames(RET)))
		if (!is.null(start) & !is.null(end)) {
			dta = dta[paste(as.character(start),"/", as.character(end), sep="")]
			chk = apply(dta, 2, mychk <-function(x) sum(is.na(x)))
			if (sum(chk >  dim(dta)[1]*cut)>0) dta1 = dta[,-which (chk >  dim(dta)[1]*cut)]
			else dta1 = dta
			dta2 = dta1[!is.na(rowSums(dta1)),] # making sure no more NAs in the data
		}
		else stop("start & end must be specified in correct format")
		if(dbg) { print(dim(dta2))}; #print(range(rownames(dta2)))}
		dta2
	}

#-----------------------------------------------------------------------------------------------------
	Tangency_PF_V2 = function(RET=NULL, start=NULL, end=NULL, RF =0,
					n_Frontier=5, col.lgnd=NULL, grp=0, dbg=0)
#	inputs:	global data, time-horizon, specifications on optimization		#
#	outputs:PF-frontier	plot and Tangency PF								# 
	{
#	1st: subset the return data to the time-horizon specified 
	ret0 = Select_RET_V2(RET=RET,start=start, end=end,dbg=dbg)
	my_asset = as.timeSeries(ret0*100)
	
#	next: set-up pff spectifications	
	spec.pf = portfolioSpec() 
	setOptimize(spec.pf) = "minRisk"
	setType(spec.pf) = "MV"
	
	setNFrontierPoints(spec.pf)<-n_Frontier
	
	if (grp==1)  { # only group constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Asset.Type[ match(colnames(my_asset), col.lgnd$Abbreviations)]
		pfConstraints = c("LongOnly", 
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.6")
		)
	}
	else if (grp==2)  { # adding fund constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Asset.Type[ match(colnames(my_asset), col.lgnd$Abbreviations)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.3",sep=""),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.6")
		)
	}
	else  pfConstraints = "LongOnly"
	
	minv.pf = minvariancePortfolio(my_asset, spec.pf, constraints = pfConstraints)
	RF = round(getTargetReturn(minv.pf@portfolio)["mean"],3)
	setRiskFreeRate(spec.pf) <- RF
	minv.pf = minvariancePortfolio(my_asset, spec.pf, constraints = pfConstraints) 
	#running (the above line) again to set the RF
	
#	finally: finding the required Tangency PF	
	tang.pf<-tangencyPortfolio(my_asset, spec.pf, constraints = pfConstraints)
	if (dbg) print("I am here")

	special = 0
	if (getTargetReturn(tang.pf@portfolio)["mean"] < RF) {special =1; opt.pf = minv.pf}
	else opt.pf = tang.pf

#	plotting the efficient frontier with Tangency PF marked on it	
	frontier.pf<-portfolioFrontier(my_asset, spec.pf, constraints = pfConstraints)
	if (dbg) print("I am still here")
	if (dbg)print(frontier.pf)
	#frontierPlot(frontier.pf,  xlim=c(-0.05,1.2), ylim=c(-0.01, 0.2))
	xy = frontierPoints(frontier.pf)
	plot(xy, xlim=c(-0.05,1.2), ylim=c(-0.03, 0.2))
	if (dbg) print("I am still here 2")
	if(!special) tangencyLines(frontier.pf, col="blue")	
	abline(h = 0, col = "grey")
	abline(v = 0, col = "grey")
	text(-0.01,RF, RF, cex=0.7, col="blue", adj=1)
	if (dbg) print("I am still here 3")
	##tangencyPoints(frontier.pf, col="blue", pch=18, cex=1.2)
	points(getTargetRisk(opt.pf@portfolio)["Cov"], getTargetReturn(opt.pf@portfolio)["mean"], 
		col="blue", pch=18, cex=1.2)
	opt.pf		
	}

#------------ 30-07-2014 -----------------------------------------------------------------------------
 	pfBacktest_V2 = function(a.pf, RET=NULL, start=NULL, end=NULL, dbg=0)
	{
		retNOW = Select_RET_V2(RET=RET,start=start, end=end,dbg=dbg)

		wts = getWeights(a.pf@portfolio); wts = wts[wts>0]
		dta = retNOW[,match(names(wts), names(retNOW))]

		dta1 = dta %*% wts
		dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
		# print(252*mean(dta1)) # annualized return Arithmetic Scale 
		# print(exp(252*mean(log(1+dta1)))-1) # annualized return Geometric Scale 
		# print(mean(dta1)*100) # mean daily return (percent) plotted in efficientFrontier
		# print(sd(dta1)*100) #  SD of daily return (risk) (pct) plotted in efficientFrontier
		AnnRet = t(table.AnnualizedReturns(dta1, scale=252))[1]
		AnnRsk = t(table.AnnualizedReturns(dta1, scale=252))[2]
		maxDrawDown = t(table.DownsideRisk(dta1))[7]
		CVaR = t(table.DownsideRisk(dta1))[9]
		data.frame(AnnRet,AnnRsk,maxDrawDown,CVaR)
	}
	
	pfBenchmark_V2 = function(BMRET=NULL, start=NULL, end=NULL, dbg=0)
	{
		bmret1 = Select_RET_V2(RET=BMRET,start=start, end=end,dbg=dbg)

		require(PerformanceAnalytics)
		AnnRet = t(table.AnnualizedReturns(bmret1, scale=252))[,1]
		AnnRsk = t(table.AnnualizedReturns(bmret1, scale=252))[,2]
		maxDrawDown = t(table.DownsideRisk(bmret1))[,7]
		CVaR = t(table.DownsideRisk(bmret1))[,9]
		data.frame(AnnRet,AnnRsk,maxDrawDown,CVaR)
	}

	require(fPortfolio)
	require(timeSeries)
	require(PerformanceAnalytics)
	op = par(mfrow=c(10,5), mar=rep(0,4)+0.3)
	for (ih in 1:length(hstarts)) {
	## TODO - find the optimum PF for the current horizon h
	
			#ih =17			
			retSel = Select_RET_V2(ret1, start=hstarts[ih], end=hends[ih], dbg=1)
			names(retSel)
			range(rownames(timeSeries(retSel)))
			tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = 0.02, 
				n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to use in Jan-2012	
	}
	

	.fooDebuggin = function # the code below was used while developing the program 
						# and commented out using this wrapper as it may not run properly anymore'
						# due to updated definition of functions
	{
		ih=17
		

		tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = 0.02, 
			n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to use in Jan-2012	
		pfBacktest_V2 (tpf, RET=ret1, start=hstarts[ih], end=hends[ih]) # Ooops ... why not matching ????
		pfBacktest_V2 (tpf, RET=ret1, start=Delhstr[ih], end=Delhend[ih])
	
		retSel = Select_RET_V2(ret1, start=hstarts[ih], end=hends[ih], dbg=1)
		wts = getWeights(tpf@portfolio); wts = wts[wts>0]
		retSel1 = retSel[, match(names(wts), names(retSel))]
		sum(apply(retSel1, 2, mean)*100*wts) # ok this is the mean as plotted in efficientFrontier
		sum(apply(retSel1 %*% wts*100, 2, sd))# ok this is the SD (risk) as plotted in efficientFrontier
		
		dim(retSel1)[1] # = 240

		ret.PF = retSel1 %*% wts*100
		ret.PF = xts(ret.PF, order.by=as.Date(rownames(timeSeries(retSel1))))
		mean(ret.PF) # ok this is the mean as plotted in efficientFrontier
		sd(ret.PF) # ok this is the SD (risk) as plotted in efficientFrontier
		dim(ret.PF)
		exp(sum(log(1+ret.PF/100))*252/240)-1 # same as Ann Ret as below
		table.AnnualizedReturns(ret.PF/100, scale=252, geometric=T)
		sum(ret.PF/100)*252/240 # same as Ann Ret as below
		table.AnnualizedReturns(ret.PF/100, scale=240, geometric=F)
		
		t(table.DownsideRisk(ret.PF/100, scale=252))[9] #max draw-down
	}
		

	load(paste("..\\02ProcData\\bmret", ".RData", sep=""))
	load(paste("..\\02ProcData\\bmlgnd", ".RData", sep=""))

	ih = 47
	bmSel =  Select_RET_V2(bmret, start=hstarts[ih], end=hends[ih], dbg=1)

	tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = 0.02, 
			n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to use in Jan-2012	
	pfBacktest_V2 (tpf, RET=ret1, start=hstarts[ih], end=hends[ih]) # Ooops ... why not matching ????
	pfBacktest_V2 (tpf, RET=ret1, start=Delhstr[ih], end=Delhend[ih])
	pfBenchmark_V2(BMRET=bmret, start=Delhstr[ih], end=Delhend[ih])

	
	optPFlist = list()
	for (ih in 1:length(hstarts)) {
	## finds the optimum PF for the current horizon h
	
			#ih =17			
			tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = 0.02, 
				n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to use in Jan-2012	
			optPFlist=append(optPFlist, tpf)
	}
	
	save(optPFlist, file=paste("optPFlist", '.Rdata', sep='') )
	load(paste("optPFlist", ".RData", sep=""))
	
	length(optPFlist)
	
	PerfList = list()
	for (ih in 1:length(hstarts)) {
	## for each opt PF, it now computes the performance of the PF & BM for the next month (DeltaH)
		if (ih == 29) next
		optPF = optPFlist[[ih]]
		tmp = pfBacktest_V2 (optPF, RET=ret1, start=Delhstr[ih], end=Delhend[ih])
		tmp = rbind(tmp, pfBenchmark_V2(BMRET=bmret, start=Delhstr[ih], end=Delhend[ih])[c("BSE100", "BSE500"),])
		rownames(tmp)[1] = "OptPF"
		PerfList = append(PerfList, tmp)
		print(tmp)
		print(ih)
	}
	
	
		retSel = Select_RET_V2(ret1, start=hstarts[ih], end=hends[ih], dbg=1)
	
		wts = getWeights(optPF@portfolio); wts = wts[wts>0]
		dta = retSel[,match(names(wts), names(retSel))]
		dim(dta)
		dta1 = dta %*% wts
		dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
		# print(252*mean(dta1)) # annualized return Arithmetic Scale 
		# print(exp(252*mean(log(1+dta1)))-1) # annualized return Geometric Scale 
		# print(mean(dta1)*100) # mean daily return (percent) plotted in efficientFrontier
		# print(sd(dta1)*100) #  SD of daily return (risk) (pct) plotted in efficientFrontier
		require(PerformanceAnalytics)
		AnnRet = t(table.AnnualizedReturns(dta1, scale=252))[1]
		AnnRsk = t(table.AnnualizedReturns(dta1, scale=252))[2]
		maxDrawDown = t(table.DownsideRisk(dta1))[7]
		CVaR = t(table.DownsideRisk(dta1))[9]
		data.frame(AnnRet,AnnRsk,maxDrawDown,CVaR)
		
		pfBacktest_V2 (optPF, RET=ret1, start=Delhstr[ih], end=Delhend[ih])