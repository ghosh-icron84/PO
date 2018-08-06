--------------------------------------------------------------------------
Functions used:

Function 1: Select_RET_V2
Function 2: Tangency_PF_V2
Function 3: pfBacktest_V2
Function 4: pfBenchmark_V2
Function 5: myplot2.Wts



	load(paste("C:\\Users\\souravd\\Desktop\\Shreya\\Portfolio Allocation Model\\ret0", ".RData", sep=""))
	load(paste("C:\\Users\\souravd\\Desktop\\Shreya\\Portfolio Allocation Model\\lgnd", ".RData", sep=""))
	ret1<-ret0
	
	####load(paste("..\\02ProcData\\bmret", ".RData", sep=""))
	####load(paste("..\\02ProcData\\bmlgnd", ".RData", sep=""))

	####Loading the index data

	load(paste("C:\\Users\\souravd\\Desktop\\Shreya\\Portfolio Allocation Model\\bmret", ".RData", sep=""))
	load(paste("C:\\Users\\souravd\\Desktop\\Shreya\\Portfolio Allocation Model\\bmlgnd", ".RData", sep=""))

	head(ret1)	

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

	#### Libraries needed
	
	require(fPortfolio)
	require(timeSeries)
	require(PerformanceAnalytics)
	
----------------------------------------------------------------------------------------
# sub-setting the return data for the specified data range and handles NAs: 28-07-2014
#### 'cut' gives you the subset size, 'dbg' is used for debugging - dbg=0 signifies
#### non-debugging mode 
----------------------------------------------------------------------------------------
	Select_RET_V2 = function(RET=NULL, start=NULL, end=NULL, cut=0.05, dbg=0)
	{
	#	1st: subset the return data to the time-horizon specified 
		if (cut > 0.2) stop ("cut value must be < 0.2")
		if (cut < 0.0001) stop ("cut value must be > 0.0001")
		if (is.null(RET)) stop("History of daily/monthly returns not specified")
		require(xts); # require(timeSeries)
		dta = xts(RET, order.by=as.Date(rownames(RET)))
		####creating a subset of data excluding those columns where too many NAs are present
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

-----------------------------------------------------------------------------------------------------
#### Creating Tangency Portfolio on NA treated subset data
#### inputs:	global data, time-horizon, specifications on optimization
####	outputs:PF-frontier	plot and Tangency PF
-----------------------------------------------------------------------------------------------------
	Tangency_PF_V2 = function(RET=NULL, start=NULL, end=NULL, RF =0,
					n_Frontier=5, col.lgnd=NULL, grp=0, dbg=0)

	{
	#1st: subset the return data to the time-horizon specified using Function 1
	ret0 = Select_RET_V2(RET=RET,start=start, end=end,dbg=dbg)
	my_asset = as.timeSeries(ret0*100)
	
	#next: set-up of portfolio specifications	
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
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.9")
		)
	}
	else if (grp==3)  { # adding fund constraints - adjusting the constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Asset.Type[ match(colnames(my_asset), col.lgnd$Abbreviations)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.8",sep=""),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.9")
		)
	}	else  pfConstraints = "LongOnly"
	
	minv.pf = minvariancePortfolio(my_asset, spec.pf, constraints = pfConstraints)
	RF = round(getTargetReturn(minv.pf@portfolio)["mean"],3)
	setRiskFreeRate(spec.pf) <- RF
	minv.pf = minvariancePortfolio(my_asset, spec.pf, constraints = pfConstraints) 
	#running (the above line) again to set the RF
	
	#finally: finding the required Tangency PF	
	tang.pf<-tangencyPortfolio(my_asset, spec.pf, constraints = pfConstraints)
	if (dbg) print("I am here")

	special = 0
	if (getTargetReturn(tang.pf@portfolio)["mean"] < RF) {special =1; opt.pf = minv.pf}
	else opt.pf = tang.pf

	#plotting the efficient frontier with Tangency PF marked on it	
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
------------------------------------------------------------------------------------------------------
#### Creating a table containing Annualized Return, Annualized Std Dev, Maximum Drawdown and
#### Conditional Value-at-Risk for a selected portfolio on NA treated data subset

#### In the code, 252 implies no of working days the stock market is open - since we are using daily
#### returns, we use this scaling - mainly US standard
------------------------------------------------------------------------------------------------------
 	pfBacktest_V2 = function(a.pf, RET=NULL, start=NULL, end=NULL, dbg=0)
	{
		retNOW = Select_RET_V2(RET=RET,start=start, end=end,dbg=dbg, cut=0.2)

		wts = getWeights(a.pf@portfolio); wts = wts[wts>0]
		dta = retNOW[,match(names(wts), names(retNOW))]

		dta1 = dta %*% wts  #### Calculates the weighted return of the portfolio at a particular point of time
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
	
----------------------------------------------------------------------------------------------
#### Creating a table containing Annualized Return, Annualized Std Dev, Maximum Drawdown and
#### Conditional Value-at-Risk for the NA treated Benchmark Index return data subset
----------------------------------------------------------------------------------------------
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
----------------------------------------------------------------------------------------------
#### This code recalls the set of tangency portfolios if already created - If not then
#### creates a dataset of all the tangency portfolios for different time horizons
----------------------------------------------------------------------------------------------

	if (!file.exists("optPFlist.RData")) {
		optPFlist = list()
		for (ih in 1:length(hstarts)) {
		## finds the optimum PF for the current horizon h
		
				#ih =17			
				tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = 0.02, 
					n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to use in Jan-2012	
				optPFlist=append(optPFlist, tpf)
		}
		save(optPFlist, file=paste("optPFlist", '.Rdata', sep='') )
	} else load(paste("optPFlist", ".RData", sep=""))

	
	length(optPFlist)

----------------------------------------------------------------------------------------------------
#### This code recalls the list of performances of the set of tangency portfolios (generated above)
#### if already created - If not then creates such a list using Function 3
----------------------------------------------------------------------------------------------------
	
	if (!file.exists("PerfList.RData")) {
		PerfList = list()
		for (ih in 1:length(hstarts)) {
		## for each opt PF, compute the performance of the PF & BM for the next month (DeltaH)
			#if (ih %in% c(29:35,44:48)) next
			optPF = optPFlist[[ih]]
			tmp = pfBacktest_V2 (optPF, RET=ret1, start=Delhstr[ih], end=Delhend[ih])
			tmp = rbind(tmp, pfBenchmark_V2(BMRET=bmret, start=Delhstr[ih], end=Delhend[ih])[c("BSE100", "BSESns"),])
			rownames(tmp)[1] = "OptPF"
			PerfList = append(PerfList, list(tmp))
			#print(tmp)
			print(ih)
		}
		save(PerfList, file=paste("PerfList", '.Rdata', sep='') )
	} 	else load(paste("PerfList", ".RData", sep=""))
	
#	AnnRet = sapply(PerfList, myfunc <- function(x) x[,1])
#	PerfList[[3]]
#	barplot(AnnRet, beside=T, legend.text	=c("OptPF","BSE100", "BSESns") )
#	?barplot	

 #------------------------------------ 04-08-2014 -------------------------------------------------------#

 #---- when PFs are constructed on past one year data and being tuned up on monthly basis 
	 for (ih in 1:length(hstarts)) {
		#tst = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih])[,c("BSE100", "BSESns")]
		#chart.CumReturns(tst, legend.loc="topleft")
	      #if (ih %in% c(29:35,44:48)) next
		optPF = optPFlist[[ih]]
		wts = getWeights(optPF@portfolio); wts = wts[wts>0]
		# there were more than 10% missing values of LTGIL - so had to increase cut to 0.2 
		retNOW = Select_RET_V2(RET=ret1, start=Delhstr[ih], end=Delhend[ih], cut=0.2)  
		dta = retNOW[,match(names(wts), names(retNOW))]
		dta1 = dta %*% wts
		dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
		bma1 = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih])[,c("BSE100", "BSESns")]
		if (ih==1) AllRets = cbind( bma1,dta1)
		else AllRets = rbind(AllRets, cbind( bma1,dta1))
	}
	colnames(AllRets)[3] = "OptPF-M"
	dim(AllRets)
#	View(AllRets)
	chart.CumReturns(AllRets[,3:1], legend.loc="topleft", col= topo.colors(6)[3:1],
		main = "Cumulative Return of Optimal PF (monthly tune-up)",ylim=c(-.3, .6) )



#---- when PFs are constructed on past one year data and being tuned up on quarterly basis 
	par(mfrow=c(4,4), mar = c(1,1,.1,.1))
	for (ih in 1:length(hstarts)) {
	## finds the optimum PF for the current horizon h
			if (ih %% 3 != 1) next # doing it quarterly 
			tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = 0.02, 
				n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to use in Jan-2012	
	}
	par(mfrow=c(1,1), mar=c(2,4,1,1))
	

	if (!file.exists("PerfListQ.RData")) {
		PerfListQ = list()
		for (ih in 1:length(hstarts)) {
		## for each opt PF, compute the performance of the PF & BM for the next month (DeltaH)
			#if (ih %in% c(29:35,44:48)) next
			if (ih %% 3 != 1) next
			optPF = optPFlist[[ih]]
			tmp = pfBacktest_V2 (optPF, RET=ret1, start=Delhstr[ih], end=Delhend[ih+2])
			tmp = rbind(tmp, pfBenchmark_V2(BMRET=bmret, start=Delhstr[ih], end=Delhend[ih+2])[c("BSE100", "BSESns"),])
			rownames(tmp)[1] = "OptPF"
			PerfListQ = append(PerfListQ, list(tmp))
			#print(tmp)
			print(ih)
		}
	save(PerfListQ, file=paste("PerfListQ", '.Rdata', sep='') )
	} else load(paste("PerfListQ", ".RData", sep=""))
	
	AnnRetQ = t(sapply(PerfListQ, myfunc <- function(x) x[,1]))
	colnames(AnnRetQ) = c("OptPF","BSE100", "BSESns")
	rownames(AnnRetQ) = as.character(Delhend[seq(1,48,3)])
	
	PerfList[[3]]
	barplot(t(AnnRetQ), beside=T, legend.text	=colnames(AnnRetQ),las=2,cex.axis=.7, cex.names=0.7,
		col = topo.colors(6)[c(6,2:1)],ylim= c(min(AnnRetQ), max(AnnRetQ)*1.1),
		args.legend = list(x = "topleft") )
		
 for (ih in 1:length(hstarts)) {
	#tst = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih])[,c("BSE100", "BSESns")]
	#chart.CumReturns(tst, legend.loc="topleft")
#	if (ih %in% c(29:35,44:48)) next
	if (ih %% 3 != 1) next
	print(ih)
	optPF = optPFlist[[ih]]
	wts = getWeights(optPF@portfolio); wts = wts[wts>0]
	retNOW = Select_RET_V2(RET=ret1, start=Delhstr[ih], end=Delhend[ih+2], cut=0.1)  
	dta = retNOW[,match(names(wts), names(retNOW))]
	dta1 = dta %*% wts
	dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
	bma1 = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih+2])[,c("BSE100", "BSESns")]
	if (ih==1) AllRetQ = cbind( dta1,bma1)
	else AllRetQ = rbind(AllRetQ, cbind( dta1,bma1))
}

	colnames(AllRetQ)[1] = "OptPFQ"
	dim(AllRetQ)
#	View(AllRetQ)
	chart.CumReturns(AllRetQ, legend.loc="topleft", main = "Cumulative Return of Optimal PF (quarterly tune-up)",
			col=   topo.colors(6)[c(6,2:1)],ylim=c(-.3, .6)  )

	AllRetCombind = cbind(AllRets[,3],AllRetQ)
	colnames(AllRetCombind)[1:2] =c( "OptPF_M", "OptPF_Q")
	chart.CumReturns(AllRetCombind, legend.loc="topleft", col=   topo.colors(6)[c(3,6,2:1)],
	main = "Cumulative Return of Optimal PF (monthly & quarterly tune-up)",ylim=c(-.3, .6) )

	chart.CumReturns(AllRetCombind[,-1], legend.loc="topleft", col= topo.colors(6)[c(6,2,1)],
	main = "Cumulative Return of Optimal PF (Quarterly tune-up)",ylim=c(-.3, .6) )

	chart.CumReturns(AllRetCombind[,-2], legend.loc="topleft", col= topo.colors(6)[c(3,2,1)],
	main = "Cumulative Return of Optimal PF (Monthly tune-up)",ylim=c(-.3, .6) )
	
#------ now plot the weight distributions across the Opt PFs -----------------------------------------#
	wtsMat <- c()

	fnds = colnames(ret1)
	for (ih in 1:length(hstarts)) {
		if (ih %% 3 != 1) next
		optPF = optPFlist[[ih]]
		wts = getWeights(optPF@portfolio) 
		ab = match(fnds, names(wts))
		nwts = wts[ab]
		names(nwts) = fnds
#		print(length(wts))
		if (ih==1) wtsMat = nwts
		else wtsMat = rbind(wtsMat,nwts)
	}

	colPos=c()
	for ( j in 1:dim(wtsMat)[2]) {
		ab = wtsMat[,j]
		if (sum(ab, na.rm = T) > 0 ) colPos = c(colPos, j)
	}
	wtsMatPos = round(wtsMat[, colPos],3)
	
	dim(wtsMatPos)
	rowSums(wtsMatPos, na.rm=T)
	
	fndPos.tpe = as.character(lgnd[match(colnames(wtsMatPos),lgnd$Abbreviations), "Asset.Type"])
	coll = rep(topo.colors(6)[2], length(fndPos.tpe))
	coll[fndPos.tpe=="Debt+Equity"] = topo.colors(6)[4]
	coll[fndPos.tpe=="Debt"] = topo.colors(6)[6]

	
	myplot2.Wts<-function(weightmat, pos.only=F, subttl="")
	{
		nr = dim(weightmat)[1]
		op = par(mar=c(0,0,0,0), oma=c(6,1,4,2))
		nf = layout(cbind(matrix(rep(1:nr, each=10), nr, 10, byrow = TRUE), (nr+1):(2*nr)))
		layout.show(nf)
		
		for (i in 1:nr){
			if (i==nr) bp = barplot(weightmat[i,], ylim=c(0,1), axes=F, col=coll,
						names.arg =colnames( weightmat),las=2, cex.names=0.7)
			else bp = barplot(weightmat[i,], ylim=c(0,1), names.arg=rep("",length(weightmat[i,])), axes=F,col=coll)
			text(bp[weightmat[i,]>0], .45, round(weightmat[i,]*100,0)[weightmat[i,]>0], cex=0.8, col="navy")
		}
		for (i in 1:nr) {
			plot(c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="")
			text(.5,.5, paste ("PF-", i,sep=""),cex=.9)
		}
		title(outer=T,line=1, main=paste("Weights of Funds of Opt PFs across consecutive time windows", subttl), col.main="maroon")
		par(op)	
	}
	myplot2.Wts(wtsMatPos)
	
	
	chk = Select_RET_V2(RET=bmret, start=Delhstr[1], end=Delhend[48])
	chart.CumReturns(chk, legend.loc="topleft", 
		main = "Cumulative Return of Optimal PF (monthly & quarterly tune-up)" )

#------------------------------------ 06-08-2014 -------------------------------------------------------#
# some points to consider
# a) exclude balanced funds
# b) allow more wt on a single fund 
# c) allow more equity funds (done! as currently it is capped at 90% with grp=2 option in Tangency_PF_V2)
# d) CVaR not exceed a cut-off, say 0.7
# e) implement Performance with "perfect foresight"
# f)  

#---------------- starting with (a) excluding balanced funds ------------------------------------------#
	ret2 = ret0[, - which(substr(lgnd$Asset.Type[match(colnames(ret0), lgnd$Abbreviations)],1, 11) == "Debt+Equity")]
	
	optPFlist2 = list()
	par(mfrow=c(4,4), mar = c(1,1,.1,.1))
	for (ih in 1:length(hstarts)) {
	## finds the optimum PF for the current horizon h
			if (ih %% 3 != 1) next # doing it quarterly 
			tpf = Tangency_PF_V2(RET=ret2, start=hstarts[ih], end=hends[ih], RF = NULL, 
				n_Frontier=35, grp=2, col.lgnd=lgnd, dbg=1)  # recommended PF to 	
			optPFlist2[[ih]] = tpf
	}
	par(mfrow=c(1,1), mar=c(4,4,1,1))
	

	if (!file.exists("PerfListQ2.RData")) {
		PerfListQ2 = list()
		for (ih in 1:length(hstarts)) {
		## for each opt PF, compute the performance of the PF & BM for the next month (DeltaH)
			#if (ih %in% c(29:35,44:48)) next
			if (ih %% 3 != 1) next
			optPF = optPFlist2[[ih]]
			tmp = pfBacktest_V2 (optPF, RET=ret2, start=Delhstr[ih], end=Delhend[ih+2])
			tmp = rbind(tmp, pfBenchmark_V2(BMRET=bmret, start=Delhstr[ih], end=Delhend[ih+2])[c("BSE100", "BSESns"),])
			rownames(tmp)[1] = "OptPF"
			PerfListQ2 = append(PerfListQ2, list(tmp))
			#print(tmp)
			print(ih)
		}
	save(PerfListQ2, file=paste("PerfListQ2", '.Rdata', sep='') )
	} else load(paste("PerfListQ2", ".RData", sep=""))
	
	AnnRetQ2 = t(sapply(PerfListQ2, myfunc <- function(x) x[,1]))
	colnames(AnnRetQ2) = c("OptPF2_Q","BSE100", "BSESns")
	rownames(AnnRetQ2) = as.character(Delhend[seq(1,48,3)])
	
	barplot(t(AnnRetQ2), beside=T, legend.text	=colnames(AnnRetQ2),las=2,cex.axis=.7, cex.names=0.7,
		col = topo.colors(6)[c(6,2:1)],ylim= c(min(AnnRetQ2), max(AnnRetQ2)*1.1),
		args.legend = list(x = "topleft") )
		
 for (ih in 1:length(hstarts)) {
	#tst = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih])[,c("BSE100", "BSESns")]
	#chart.CumReturns(tst, legend.loc="topleft")
#	if (ih %in% c(29:35,44:48)) next
	if (ih %% 3 != 1) next
	print(ih)
	optPF = optPFlist2[[ih]]
	wts = getWeights(optPF@portfolio); wts = wts[wts>0]
	retNOW = Select_RET_V2(RET=ret2, start=Delhstr[ih], end=Delhend[ih+2], cut=0.1)  
	dta = retNOW[,match(names(wts), names(retNOW))]
	dta1 = dta %*% wts
	dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
	bma1 = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih+2])[,c("BSE100", "BSESns")]
	if (ih==1) AllRetQ2 = cbind( dta1,bma1)
	else AllRetQ2 = rbind(AllRetQ2, cbind( dta1,bma1))
}

	colnames(AllRetQ2)[1] = "OptPFQ2"
	dim(AllRetQ2)

	chart.CumReturns(AllRetQ2, legend.loc="topleft", main = "Cumulative Return of Optimal PF (quarterly tune-up)",
			col=   topo.colors(10)[c(9,4,1)],ylim=c(-.3, .6)  )

	
#------ now plot the weight distributions across the Opt PFs -----------------------------------------#
	wtsMat <- c()

	fnds = colnames(ret2)
	for (ih in 1:length(hstarts)) {
		if (ih %% 3 != 1) next
		optPF = optPFlist2[[ih]]
		wts = getWeights(optPF@portfolio) 
		ab = match(fnds, names(wts))
		nwts = wts[ab]
		names(nwts) = fnds
#		print(length(wts))
		if (ih==1) wtsMat = nwts
		else wtsMat = rbind(wtsMat,nwts)
	}

	colPos=c()
	for ( j in 1:dim(wtsMat)[2]) {
		ab = wtsMat[,j]
		if (sum(ab, na.rm = T) > 0 ) colPos = c(colPos, j)
	}
	wtsMatPos = round(wtsMat[, colPos],3)
	
	dim(wtsMatPos)
	rowSums(wtsMatPos, na.rm=T)
	
	fndPos.tpe = as.character(lgnd[match(colnames(wtsMatPos),lgnd$Abbreviations), "Asset.Type"])
	coll = rep(topo.colors(6)[2], length(fndPos.tpe))
	coll[fndPos.tpe=="Debt+Equity"] = topo.colors(6)[4]
	coll[fndPos.tpe=="Debt"] = topo.colors(6)[6]

	
	myplot2.Wts(wtsMatPos)
	
	charts.PerformanceSummary(AllRetQ)
	charts.PerformanceSummary(AllRetQ2)
	CVaR(AllRetQ);CVaR(AllRetQ2)
	StdDev.annualized(AllRetQ);StdDev.annualized(AllRetQ2)
	Return.annualized(AllRetQ);Return.annualized(AllRetQ2)
	maxDrawdown(AllRetQ);maxDrawdown(AllRetQ2)
	
	retChk = Select_RET_V2 (RET=ret1, start="2010-07-01", end="2014-06-30", cut=0.2, dbg=0)
	retChk2 = cbind(AllRetQ[,1], AllRetQ2, retChk)
	dim(retChk2)
	annRetChk = Return.annualized(retChk2)
	annRskChk = StdDev.annualized(retChk2)
	ESChk = CVaR(retChk2)
	mxDrwChk = maxDrawdown(retChk2)

	fndPos.tpe = as.character(lgnd[match(colnames(retChk),lgnd$Abbreviations), "Asset.Type"])
	coll = rep(topo.colors(6)[2], length(annRetChk))
	coll[fndPos.tpe=="Debt+Equity"] = topo.colors(6)[4]
	coll[fndPos.tpe=="Debt"] = topo.colors(6)[6]
	
	plot(annRskChk[,-c(1:4)],annRetChk[,-c(1:4)], pch=16, col=coll,xlab="annualized risk", ylab = "annualized return")
	points(annRskChk[,c(1:2)],annRetChk[,c(1:2)], pch=18, cex=2,col=c("orange","blue"))
	points(annRskChk[,c(3:4)],annRetChk[,c(3:4)], pch=19, cex=1.5,col=c("gray"))
	text(annRskChk, annRetChk+0.005, colnames(retChk2),cex=0.5, adj=1)

	plot(mxDrwChk[,-c(1:4)],annRetChk[,-c(1:4)], pch=16, col=coll,xlab="Max Drawdown", ylab = "Annualized Return")
	points(mxDrwChk[,c(1:2)],annRetChk[,c(1:2)], pch=18, cex=2,col=c("orange","blue"))
	points(mxDrwChk[,c(3:4)],annRetChk[,c(3:4)], pch=19, cex=1.5,col=c("gray"))
	text(mxDrwChk, annRetChk+0.005, colnames(retChk2),cex=0.5, adj=1)

	plot(ESChk[,-c(1:4)],annRetChk[,-c(1:4)], pch=16, col=coll,xlab="CVaR", ylab = "Annualized Return")
	points(ESChk[,c(1:2)],annRetChk[,c(1:2)], pch=18, cex=2,col=c("orange","blue"))
	points(ESChk[,c(3:4)],annRetChk[,c(3:4)], pch=19, cex=1.5,col=c("gray"))
	text(ESChk, annRetChk+0.005, colnames(retChk2),cex=0.5, adj=1)
	
#----------------(b) allowing up to 80% wt on individual fund ------------------------------------------#
	optPFlist3 = list()
	par(mfrow=c(4,4), mar = c(1,1,.1,.1))
	for (ih in 1:length(hstarts)) {
	## finds the optimum PF for the current horizon h
			if (ih %% 3 != 1) next # doing it quarterly 
			tpf = Tangency_PF_V2(RET=ret1, start=hstarts[ih], end=hends[ih], RF = NULL, 
				n_Frontier=35, grp=3, col.lgnd=lgnd, dbg=1)  # recommended PF to 	
			optPFlist3[[ih]] = tpf
	}
	par(mfrow=c(1,1), mar=c(4,4,1,1))
	

	if (!file.exists("PerfListQ3.RData")) {
		PerfListQ3 = list()
		for (ih in 1:length(hstarts)) {
		## for each opt PF, compute the performance of the PF & BM for the next month (DeltaH)
			#if (ih %in% c(29:35,44:48)) next
			if (ih %% 3 != 1) next
			optPF = optPFlist3[[ih]]
			tmp = pfBacktest_V2 (optPF, RET=ret1, start=Delhstr[ih], end=Delhend[ih+2])
			tmp = rbind(tmp, pfBenchmark_V2(BMRET=bmret, start=Delhstr[ih], end=Delhend[ih+2])[c("BSE100", "BSESns"),])
			rownames(tmp)[1] = "OptPF"
			PerfListQ3 = append(PerfListQ3, list(tmp))
			#print(tmp)
			print(ih)
		}
	save(PerfListQ3, file=paste("PerfListQ3", '.Rdata', sep='') )
	} else load(paste("PerfListQ3", ".RData", sep=""))
	
	AnnRetQ3 = t(sapply(PerfListQ3, myfunc <- function(x) x[,1]))
	colnames(AnnRetQ3) = c("OptPF3_Q","BSE100", "BSESns")
	rownames(AnnRetQ3) = as.character(Delhend[seq(1,48,3)])
	
	barplot(t(AnnRetQ3), beside=T, legend.text	=colnames(AnnRetQ3),las=2,cex.axis=.7, cex.names=0.7,
		col = topo.colors(6)[c(6,2:1)],ylim= c(min(AnnRetQ3), max(AnnRetQ3)*1.1),
		args.legend = list(x = "topleft") )
		
 #changes made on 11-08-2014 ------------- to save AllRetQ3 data for future use ----------------------#
	if (!file.exists("AllRetQ3.RData")) {
		for (ih in 1:length(hstarts)) {
			#tst = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih])[,c("BSE100", "BSESns")]
			#chart.CumReturns(tst, legend.loc="topleft")
		#	if (ih %in% c(29:35,44:48)) next
			if (ih %% 3 != 1) next
			print(ih)
			optPF = optPFlist3[[ih]]
			wts = getWeights(optPF@portfolio); wts = wts[wts>0]
			retNOW = Select_RET_V2(RET=ret1, start=Delhstr[ih], end=Delhend[ih+2], cut=0.1)  
			dta = retNOW[,match(names(wts), names(retNOW))]
			dta1 = dta %*% wts
			dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
			bma1 = Select_RET_V2(RET=bmret, start=Delhstr[ih], end=Delhend[ih+2])[,c("BSE100", "BSESns")]
			if (ih==1) AllRetQ3 = cbind( dta1,bma1)
			else AllRetQ3 = rbind(AllRetQ3, cbind( dta1,bma1))
		}		
	}
	colnames(AllRetQ3)[1] = "OptPFQ3"
	save(AllRetQ3, file=paste("AllRetQ3", '.Rdata', sep='') )
	} else load(paste("AllRetQ3", ".RData", sep=""))

	chart.CumReturns(AllRetQ3, legend.loc="topleft", main = "Cumulative Return of Optimal PF (quarterly tune-up)",
			col=   topo.colors(10)[c(9,4,1)],ylim=c(-.3, .6)  )
	
#------ now plot the weight distributions across the Opt PFs -----------------------------------------#
	wtsMat <- c()

	fnds = colnames(ret1)
	for (ih in 1:length(hstarts)) {
		if (ih %% 3 != 1) next
		optPF = optPFlist3[[ih]]
		wts = getWeights(optPF@portfolio) 
		ab = match(fnds, names(wts))
		nwts = wts[ab]
		names(nwts) = fnds
#------	print(length(wts)) ----------------------------------------------------------------------------#
		if (ih==1) wtsMat = nwts
		else wtsMat = rbind(wtsMat,nwts)
	}

	colPos=c()
	for ( j in 1:dim(wtsMat)[2]) {
		ab = wtsMat[,j]
		if (sum(ab, na.rm = T) > 0 ) colPos = c(colPos, j)
	}
	wtsMatPos = round(wtsMat[, colPos],3)
	
	dim(wtsMatPos)
	rowSums(wtsMatPos, na.rm=T)
	
	fndPos.tpe = as.character(lgnd[match(colnames(wtsMatPos),lgnd$Abbreviations), "Asset.Type"])
	coll = rep(topo.colors(6)[2], length(fndPos.tpe))
	coll[fndPos.tpe=="Debt+Equity"] = topo.colors(6)[4]
	coll[fndPos.tpe=="Debt"] = topo.colors(6)[6]
	
	myplot2.Wts(wtsMatPos)
#----------------------------------------------------------------------------------------------------------------#	
#	charts.PerformanceSummary(AllRetQ)
#	charts.PerformanceSummary(AllRetQ3)
#
	retChk = Select_RET_V2 (RET=ret1, start="2010-07-01", end="2014-06-30", cut=0.2, dbg=0)
	retChk2 = cbind(AllRetQ3[,1],AllRetQ[,1], AllRetQ2, retChk)
	dim(retChk2)
	annRetChk = Return.annualized(retChk2)
	annRskChk = StdDev.annualized(retChk2)
	ESChk = CVaR(retChk2)
	mxDrwChk = maxDrawdown(retChk2)
#
	fndPos.tpe = as.character(lgnd[match(colnames(retChk),lgnd$Abbreviations), "Asset.Type"])
	coll = rep(topo.colors(6)[2], length(annRetChk))
	coll[fndPos.tpe=="Debt+Equity"] = topo.colors(6)[4]
	coll[fndPos.tpe=="Debt"] = topo.colors(6)[6]
#	
	plot(annRskChk[,-c(1:5)],annRetChk[,-c(1:5)], pch=16, col=coll,xlab="annualized risk", ylab = "annualized return")
	points(annRskChk[,c(1:3)],annRetChk[,c(1:3)], pch=18, cex=2,col=c("maroon", "orange","blue"))
	points(annRskChk[,c(4:5)],annRetChk[,c(4:5)], pch=19, cex=1.5,col=c("gray"))
	text(annRskChk, annRetChk+0.005, colnames(retChk2),cex=0.5, adj=1)
#
	plot(mxDrwChk[,-c(1:5)],annRetChk[,-c(1:5)], pch=16, col=coll,xlab="Max Drawdown", ylab = "Annualized Return")
	points(mxDrwChk[,c(1:3)],annRetChk[,c(1:3)], pch=18, cex=2,col=c("maroon", "orange","blue"))
	points(mxDrwChk[,c(4:5)],annRetChk[,c(4:5)], pch=19, cex=1.5,col=c("gray"))
	text(mxDrwChk, annRetChk+0.005, colnames(retChk2),cex=0.5, adj=1)
#
	plot(ESChk[,-c(1:5)],annRetChk[,-c(1:5)], pch=16, col=coll,xlab="CVaR", ylab = "Annualized Return")
	points(ESChk[,c(1:3)],annRetChk[,c(1:3)], pch=18, cex=2,col=c("maroon", "orange","blue"))
	points(ESChk[,c(4:5)],annRetChk[,c(4:5)], pch=19, cex=1.5,col=c("gray"))
	text(ESChk, annRetChk+0.005, colnames(retChk2),cex=0.5, adj=1)
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#

