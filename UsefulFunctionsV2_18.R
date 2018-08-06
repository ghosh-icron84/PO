#-------------------------------------------------------------------------------#
#	Below is a copy of the UsefulFunctionsV3 appropriately modified to 
#	address the request for Marketing Presentation  
#-------------------------------------------------------------------------------#
#																				#
#    Start:		Sep 22, 2014							     					#
#    End:		Sep 11, 2014													#
#    R Version:	3.1.0 (2014-04-10) -- "Spring Dance"					     	#
#	 Author: 	Dr. S. Mukherjee							     				#
#-------------------------------------------------------------------------------#
#	Content / Index of 'Useful' Functions:
#		1. Select_RET_V2			# sub-setting the return data 
#		2. Tangency_PF_V4			# core program to compute the opt-PF 
#		3. pfBacktest_V2			# computes perf. summary of Opt-PF for a specified time period
#		4. pfBenchmark_V2			# computes perf. summary of Benchmark(s) for a specified time period
#		5. myplot3.Wts 				# earlier version of weight distribution plot... not used any more
#		6. getMyPFbacktestWeightsV3 # obtain the weights of Opt-PF at each testing time-frame
#		7. myplot5.Wts				# revised version of weight distribution plot... currently in use
#		8. myStartegyRetV4			# computes return of a specified Opt-PF
#		9. BackTestingTimeFrames	# create time frames for construction and back-testing Opt-PFs 
#		10.myplot6.Wts, myTextPlot	# the latter func. called by the former - an optional improvement to myplot5.Wts
#		11.SMart_Wrapper_ConservativeOptPF # see header information just above the function
#		12.myPF_CumReturns			# plots cum return (and saves the plot in Plots sub-folder)
#		13.myPF_QrtReturns			# plots Quarterly return (and saves the plot in Plots sub-folder)
#		14.myPF_Overall_Performance	# plots Overall Perf. (and saves the plot in Plots sub-folder)
#-------------------------------------------------------------------------------#
#-------------- packages required -----------------------------
#
	require(fPortfolio)
	require(timeSeries)
	require(PerformanceAnalytics)
	
#-----------------------------------------------------------------------------------------------------
# sub-setting the return data for the specified data range and handles NAs: 28-07-2014
#-----------------------------------------------------------------------------------------------------
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

#----------------------------------------------------------------------------------------------------#
	Tangency_PF_V4 = function(RET=NULL, start=NULL, end=NULL, RF =0,
					n_Frontier=5, col.lgnd=NULL, grp=0, dbg=0, robust=0)
	{
#	1st: subset the return data to the time-horizon specified 
	ret0 = Select_RET_V2(RET=RET,start=start, end=end,dbg=dbg)
	my_asset = as.timeSeries(ret0*100)
	
#	next: set-up of portfolio specifications	
	spec.pf = portfolioSpec() 
	setOptimize(spec.pf) = "minRisk"
	setType(spec.pf) = "MV"
	
#	handle robust specifications
	if (robust==1) {
		require(robustbase)
#		robustEst = mveEstimator(my_asset)
#		fastMveEst <- function(x, spec = NULL, ...) mveEstimator
		setEstimator(spec.pf ) <- "mveEstimator" # minimum volume ellipsoid
	} else if (robust==2) {
		require(robustbase)
		setEstimator(spec.pf ) <- "shrinkEstimator" #the shrinkage estimator
	} else if (robust==3) {
		require(robustbase)
		setEstimator(spec.pf ) <- "covMcdEstimator" # minimum covariance determinant 
	}
	
	setNFrontierPoints(spec.pf)<-n_Frontier
	
	if (grp==1)  { # only group constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c("LongOnly", 
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.6")
		)
	}
	else if (grp==2)  { # adding fund constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.3",sep=""),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.9")
		)
	}
	else if (grp==3)  { # adding fund constraints
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.8",sep=""),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.5"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Equity"))), collapse=","), ")]=0.9")
		)
	}	
	else if (grp==4)  { # riskaverse Portfolio with individual fund wt capped at 0.8
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.8",sep=""),
		paste("minsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.75"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.9")
		)
	}
	else if (grp==4.1)  { # Riskaverse Portfolio with individual fund wt capped at 0.4
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.4",sep=""),
		paste("minsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.75"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.9")
		)		
	}
	else if (grp==4.2)  { # Riskaverse Portfolio with individual fund wt capped at 0.35
		if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.35",sep=""),
		paste("minsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.75"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.9")
		)
	} 
      else if (grp==4.3) {# Conservative portfolio with indv fund wt capped at 0.4
               if (is.null(col.lgnd)) stop("must provide column legend info when grp !=0")
		grp.names = col.lgnd$Fund.Category[ match(colnames(my_asset), col.lgnd$Legends)]
		pfConstraints = c(# "LongOnly", ### warning - not working along with fund-level constraints!! 
		paste("minW[1:",dim(my_asset)[2],"]=0.0",sep=""),
		paste("maxW[1:",dim(my_asset)[2],"]=0.4",sep=""),
		paste("minsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.55"),
		paste("maxsumW[c(", paste(which(!is.na(match(grp.names, "Debt"))), collapse=","), ")]=0.75")
		)		
	} else  pfConstraints = "LongOnly"
	
	minv.pf = minvariancePortfolio(my_asset, spec.pf, constraints = pfConstraints)
	RF = round(getTargetReturn(minv.pf@portfolio)["mean"],4)
	setRiskFreeRate(spec.pf) <- RF
	minv.pf = minvariancePortfolio(my_asset, spec.pf, constraints = pfConstraints) 
#	running (the above line) again to set the RF --------------------------------------------------
#	
#	finally: finding the required Tangency PF	---------------------------------------------------
	tang.pf<-tangencyPortfolio(my_asset, spec.pf, constraints = pfConstraints)
	if (dbg) print("I am here")

	special = 0
	if (getTargetReturn(tang.pf@portfolio)["mean"] < RF) {special =1; opt.pf = minv.pf}
	else opt.pf = tang.pf

#	plotting the efficient frontier with Tangency PF marked on it	
	if (dbg) print(spec.pf)
	frontier.pf<-portfolioFrontier(my_asset, spec.pf, constraints = pfConstraints)
	if (dbg) print("I am still here")
	if (dbg)print(frontier.pf)
	#frontierPlot(frontier.pf,  xlim=c(-0.05,1.2), ylim=c(-0.01, 0.2))
	xy = frontierPoints(frontier.pf)
	plot(xy, xlim=c(0.0,2.2), ylim=c(-0.4, 0.6))
	if (dbg) print("I am still here 2")
	if(!special) tangencyLines(frontier.pf, col="blue")	
	abline(h = 0, col = "grey")
	abline(v = 0, col = "grey")
	text(-0.01,RF, RF, cex=0.7, col="blue", adj=1)
	text(0.1,.16, paste(start, "to" , end), cex=0.7, col="orange", adj=0)
	if (dbg) print("I am still here 3")
	##tangencyPoints(frontier.pf, col="blue", pch=18, cex=1.2)
	points(getTargetRisk(opt.pf@portfolio)["Sigma"], getTargetReturn(opt.pf@portfolio)["mean"], 
		col="blue", pch=18, cex=1.2)
	opt.pf		
	}

#------------ 30-07-2014 -----------------------------------------------------------------------------
 	pfBacktest_V2 = function(a.pf, RET=NULL, start=NULL, end=NULL, dbg=0)
	{
		retNOW = Select_RET_V2(RET=RET,start=start, end=end,dbg=dbg, cut=0.2)

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
		AnnRet = t(table.AnnualizedReturns(bmret1, scale=261))[,1]
		AnnRsk = t(table.AnnualizedReturns(bmret1, scale=261))[,2]
		maxDrawDown = t(table.DownsideRisk(bmret1))[,7]
		CVaR = t(table.DownsideRisk(bmret1))[,9]
		data.frame(AnnRet,AnnRsk,maxDrawDown,CVaR)
	}

	myplot3.Wts<-function(weightmat, pos.only=F, subttl="")
	{
		nr = dim(weightmat)[1]
		op = par(mar=c(0,0,0,0), oma=c(6,1,4,2))
		nf = layout(cbind(matrix(rep(1:nr, each=10), nr, 10, byrow = TRUE), (nr+1):(2*nr)))
		layout.show(nf)
		
		for (i in 1:nr){
			if (i==nr) bp = barplot(weightmat[i,], ylim=c(0,1), axes=F, col=coll,
						names.arg =colnames( weightmat),las=2, cex.names=1.5)
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

#----------------------------------------------------------------------------------------------------#
#------- added on 21-08-2014---------------
#--------------------------------------------------------------------------------------------------------#
#------- Now create a function to get the weights   -----------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#	
	getMyPFbacktestWeightsV3 = function(myPFlist, fnds = colnames(ret1), rownm = as.character(Delhstr), prefx = "PF" )
	{
		wtsMat = c()
		wtrwnm = c()
		for (ih in 1:length(myPFlist)) {
			optPF = myPFlist[[ih]]
			wts = getWeights(optPF@portfolio) 
			ab = match(fnds, names(wts))
			nwts = wts[ab]
			names(nwts) = fnds
	#------	print(length(wts)) ----------------------------------------------------------------------------#
			if (ih==1) {wtsMat = nwts; wtrwnm = paste(prefx,": ", rownm[ih],sep="")}
			else {wtsMat = rbind(wtsMat,nwts); wtrwnm = c(wtrwnm,paste(prefx,": ",rownm[ih],sep=""))}
		}
		colPos=c()
		for ( j in 1:dim(wtsMat)[2]) {
			ab = wtsMat[,j]
			if (sum(ab, na.rm = T) > 0 ) colPos = c(colPos, j)
		}
		wtsMatPos = round(wtsMat[, colPos],3)
		rownames(wtsMatPos) = wtrwnm
		
		wtsMatPos
	}
#----------------------------------------------------------------------------------------------------------#
#	now revising the plot function defined earlier to 
#	(a) give more clarity in labelling the time windows
#	(b) [NOT implemented yet ] to add a stacked bar chart of group-sums at each time window
#----------------------------------------------------------------------------------------------------------#
	myplot5.Wts = function(weightmat, collst =  topo.colors(6)[c(2,4,6)], ftyp = lgnd)
	{
		nr = dim(weightmat)[1]
		
		fndPos.tpe = as.character(ftyp[match(colnames(weightmat),ftyp$Legends), "Fund.Category"])
		coll = rep(collst[2], length(fndPos.tpe))
		coll[fndPos.tpe=="Equity"] = collst[1]
		coll[fndPos.tpe=="Debt"] = collst[3]
			
		lblstart = regexpr(":", rownames(weightmat)[1])[1]+2
		lblend = nchar(rownames(weightmat)[1])
		op = par(mar=c(0,0,0,0), oma=c(6,1,4,2))
		nf = layout(cbind(matrix(rep(1:nr, each=10), nr, 10, byrow = TRUE), (nr+1):(2*nr)))
		layout.show(nf)
##		coll= "grey"
		subttl = substr(rownames(weightmat)[1], 1, lblstart-3)
		
		for (i in 1:nr){
			if (i==nr) bp = barplot(weightmat[i,], ylim=c(0,1), axes=F, col=coll,
						names.arg =colnames( weightmat),las=2, cex.names=1.5)
			else bp = barplot(weightmat[i,], ylim=c(0,1), names.arg=rep("",length(weightmat[i,])), axes=F,col=coll)
			text(bp[weightmat[i,]>0], .45, round(weightmat[i,]*100,0)[weightmat[i,]>0], cex=0.8, col="navy")
		}
		for (i in 1:nr) {
			plot(c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="")
			text(.5,.5, substr(rownames(weightmat)[i],lblstart, lblend),cex=.9)
		}
		title(outer=T,line=1,
		  main=paste("Weights of Funds of Opt PFs across consecutive time windows\n", subttl), col.main="maroon")
		par(op)	
	}
#----------------------------------------------------------------------------------------------------#
#------ 						Revised on 23-05-2018 								-----------------#
#----------------------------------------------------------------------------------------------------#
#	Re-Computing Strategy Returns - as a function
	myStartegyRetV4 = function(myPFlist="optPF6h6mo", RET=ret1, tm)
	{
		 load(paste("QOuts_18\\", myPFlist ,".RData", sep=""))
		 for (ih in 1:length(tm$hstarts)) {
			print(ih)
			eval(parse(text = paste('optPF = ',myPFlist,'[[',ih,']]',sep='')))
			wts = getWeights(optPF@portfolio); wts = wts[wts>0] 
			rets = xts(RET, order.by=as.Date(rownames(RET)))
			retNOW = rets[paste(tm$Delhstr[ih], "/", end=tm$Delhend[ih], sep="")  ]
			dta = retNOW[,match(names(wts), names(retNOW))] 
			dta1 = dta %*% wts
			dta1 = xts(dta1, order.by=as.Date(rownames(timeSeries(dta))))
			if (ih==1) AllRetQ =  dta1
			else AllRetQ = rbind(AllRetQ, dta1)
		}
		colnames(AllRetQ)[1] = myPFlist
		AllRetQ
	}  
#----------------------------------------------------------------------------------------------------#
#------- added on 23-05-2018  -----------------------------------------------------------------------#
#------- creating time frames for Backtesting -------------------------------------------------------#
	
	BackTestingTimeFrames =  
		function(startDt='2016-01-01', endDt ='2017-12-31',  horizon = 12, shift = 3, test = shift)
	{
		require(lubridate)
		# ymd function parses dates in year-month-day format
		startDate = ymd(startDt)
		i=0
		repeat {
			i=i+1; # print(i)
			if (ymd('2017-12-31') < ymd('2016-01-01') %m+% months(i)){ break}
		}
		n = i -horizon-test
#		print(n)
		# The %m+% adds months to dates without exceeding the last day
		hstarts = as.Date(startDate %m+% months(seq(0,n,shift)))
		hends = hstarts %m+% months(horizon) -1 
		Delhstr = hends +1
		Delhend = hends %m+% months(test)
		tm = data.frame(hstarts, hends, Delhstr, Delhend)
		tm
	}
	
	#tme = BackTestingTimeFrames(horizon = 12)
	#tme = BackTestingTimeFrames(horizon = 6)
	#tme = BackTestingTimeFrames(horizon = 24)
#----------------------------------------------------------------------------------------------------------#
#	now further revising the plot function defined earlier to 
#	(a) give more clarity in labelling the time windows
#	(b) to add a text of group-sums at each time window
#----------------------------------------------------------------------------------------------------------#
	myplot6.Wts = function(weightmat, collst =  topo.colors(6)[c(2,4,6)], ftyp = lgnd)
	{
		nr = dim(weightmat)[1]
		
		fndPos.tpe = as.character(ftyp[match(colnames(weightmat),ftyp$Legends), "Fund.Category"])
		coll = rep(collst[2], length(fndPos.tpe))
		coll[fndPos.tpe=="Equity"] = collst[1]
		coll[fndPos.tpe=="Debt"] = collst[3]
		
			
		lblstart = regexpr(":", rownames(weightmat)[1])[1]+2
		lblend = nchar(rownames(weightmat)[1])
		op = par(mar=c(0,0,0,0), oma=c(6,1,4,2))
		nf = layout(cbind(cbind(matrix(rep(1:nr, each=10), nr, 10, byrow = TRUE), (nr+1):(2*nr)),(2*nr+1):(3*nr)))
		layout.show(nf)
##		coll= "grey"
		subttl = substr(rownames(weightmat)[1], 1, lblstart-3)
		
		for (i in 1:nr){
			if (i==nr) bp = barplot(weightmat[i,], ylim=c(0,1), axes=F, col=coll,
						names.arg =colnames( weightmat),las=2, cex.names=1.5)
			else bp = barplot(weightmat[i,], ylim=c(0,1), names.arg=rep("",length(weightmat[i,])), axes=F,col=coll)
			text(bp[weightmat[i,]>0], .45, round(weightmat[i,]*100,0)[weightmat[i,]>0], cex=0.8, col="navy")
		}
		for (i in 1:nr) {
			mywts = weightmat[i,]
			dwt = sum(mywts[fndPos.tpe=="Debt"],na.rm=T)
			ewt = sum(mywts[fndPos.tpe=="Equity"],na.rm=T)
			mwt = sum(mywts[fndPos.tpe=="Gold"],na.rm=T)
			myTextPlot(wts = c(dwt,mwt,ewt), lbls =c("D","B","E"),cols = topo.colors(6)[c(6,4,2)])
		}
		for (i in 1:nr) {
			plot(c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="")
			text(.5,.5, substr(rownames(weightmat)[i],lblstart, lblend),cex=.9)
		}
		title(outer=T,line=1,
		  main=paste("Weights of Funds of Opt PFs across consecutive time windows\n", subttl), col.main="maroon")
		par(op)	
	}
	

# spent some time on this function on Sept 1 but final outcome is poor - so going back to v5 of myplot. 
	myTextPlot = function(wts = c(70,10,20), lbls =c("Debt","Gold","Equity"),cols = topo.colors(6)[c(6,4,2)])
	{
		gwts =round(wts/sum(wts,na.rm=T)*100,0)
		#barplot(as.matrix(gwts,ncol=1),width=0.4,beside=F,col = cols, xlim = c(0,1.2), axes =F)
		#ypos = cumsum(gwts)- gwts/2
		#text(rep(1,3),ypos, paste(lbls,":",gwts ,"%",sep=""))
		plot(c(0,2),c(0,100), type="n", axes=F)
		text(rep(.3,3),c(15,50, 85), paste(lbls,":",sprintf("%2.0f",gwts) ,"%",sep=""),adj=0)
		box()
	}
	

	#-------------------------------------------------------------------------------#
	# The function below is a wrapper to run various steps related to conservative PF				
	#	Valid parameters (implemented cases) : 
	#	a) horizon = 12, 6, 24 (in months)
	#	b) group = 4, 4.1, & 4.2 (with Maximum indiv. fund wt 80%, 40%, 20%, resp) 
	#	c) method = "Basic", "Shrinkage", "MVE"
	#	The function outputs 
	#	(i)		Efficient Frontier Plots (multiple plots in one graph file)
	#	(ii)	Weight distribution Plot of the Optimum PF at each time windows
	#	(iii)	Intermediate data R/CSV files that will be needed for future analysis/plots
	#	Note: Data is saved in "Outputs" sub-folder and plots are saved in "Plots" sub-folders
	#-------------------------------------------------------------------------------#
	SMart_Wrapper_ConservativeOptPF = function(hor = 6, group = 4.3, method = "Basic", token = "Conservative")
	{
		tme = BackTestingTimeFrames(horizon = hor) # computes the time windows for construction & testing 
		#if (!(group %in% c(4, 4.1, 4.2,4.3))) 
		#	stop("Invalid specification of 'group' should be 4, 4.1,4.2!")
		# mapping of various parameters for 'method' .............
		if (method == "Basic") rob = 0
		else if (method =="MVE") rob = 1
		else if (method =="Shrinkage") rob = 2
		else stop("Invalid specification of 'method' - please check options!")
#-----------------------------------------------------------------------------------#
#------- Now get the optimal PFs across the time horizon   -------------------------#
#-----------------------------------------------------------------------------------#	
		win.graph(width=10, height=7, pointsize=11)
		if (!file.exists(paste("QOuts_18\\", token, ".RData",sep=""))) {
			optCPF = list()
			if (hor == 12) par(mfrow=c(5,6), mar = c(2,1,.1,.1))
			else if (hor == 24) par(mfrow=c(4,3), mar = c(2,1,.1,.1))
			else if (hor ==6) par(mfrow=c(5,6), mar = c(2,1,.1,.1))
			else stop("Invalid specification of 'hor' - please check options!")
			for (ih in 1:length(tme$hstarts)) {
				tpf = Tangency_PF_V4(RET=ret1, start=tme$hstarts[ih], end=tme$hends[ih], RF = NULL, 
					n_Frontier=35, grp=group, col.lgnd=lgnd, dbg=1, robust = rob)  # getting regular Optimized PF
				optCPF[[ih]] = tpf
			}
			par(mfrow=c(1,1), mar=c(4,4,1,1))
			savePlot(file = paste("QOuts_18\\", token, "EF.emf", sep=""), type="emf") # Plots of Efficient Frontiers
			eval (parse(text = paste(token, '= optCPF')))
			fnm = paste("QOuts_18/", token, ".Rdata", sep="")
			print(fnm)
			eval(parse(text = paste('save(', token,', file="',fnm,'" )',sep='')))
		} else load(paste("QOuts_18\\", token ,".RData", sep=""))	

#-----------------------------------------------------------------------------------#
#------- Now get the weights of those PFS  -----------------------------------------#
#-----------------------------------------------------------------------------------#	
		eval (parse(text = paste('optCPF=',token)))
		wt = getMyPFbacktestWeightsV3(optCPF, rownm = as.character(tme$Delhstr), 
				prefx = token)
#----------------------------------------------------------------------------------#
#	now using a revised function to plot the weights ...
#-----------------------------------------------------------------------------------#
		myplot5.Wts(wt) # Plots of Positive Weights
		savePlot(file = paste("QOuts_18\\", token, "Wts.emf", sep=""), type="emf") 
		write.csv(wt, paste("QOuts_18\\", token, "Wts.csv",sep=""))
		Sys.sleep(40); dev.off()
	}

#-----------------------------------------------------------------------------------#
#	Computing Cumulative Returns of one or more optimum PFs, added on 23-May-2018
#-----------------------------------------------------------------------------------#
	myPF_CumReturns = function(myPFstrList = c("optCPF3h6mo", "optCPF3h1yr"), bmRet = bmeqt, prefix="Conservative")
	{
		allRet = bmRet
		require(xts)
		for (pf in rev(myPFstrList)) {
			nlen = nchar(pf)
			if (substr(pf, nlen-2,nlen) == "1yr") tme = BackTestingTimeFrames(horizon = 12)
			else if (substr(pf, nlen-2,nlen) == "2yr") tme = BackTestingTimeFrames(horizon = 24)
			else if (substr(pf, nlen-2,nlen) == "6mo") tme = BackTestingTimeFrames(horizon = 6)
			else stop("cannot compute testing time periods")
			ret = myStartegyRetV4(myPFlist=pf, tm=tme, RET = ret1)
			allRet = cbind(ret, allRet)		
		}
		print(tail(allRet,20))
		win.graph(width=10, height=7, pointsize=11)
		par(mfrow=c(1,1), mar=c(4,4,2,2)) 
		chart.CumReturns(allRet, legend.loc="topleft", 
			main = "Comparing Cumulative Returns", 
				col=   c(topo.colors(6)[1:length(myPFstrList)], c("red", "blue","yellow")),ylim=c(-.6, 1.4) )	
		savePlot(file = paste("QOuts_18\\", prefix, "CumRet.emf", sep=""), type="emf") # Plots of Cum Returns
		Sys.sleep(40);dev.off()
	}


#-----------------------------------------------------------------------------------#
#	Computing Quarterly Returns of one or more optimum PFs, added on 23-May-2018
#-----------------------------------------------------------------------------------#
	myPF_QrtReturns = function(myPFstrList = c("optCPF3h1yr","optCPF3h6mo" ,"optCPF5_1h6mo"),
			ttl = "Comparing Quarterly Returns for CPF3 (@H= 1yr, 6mo) & CPF5 (@H = 6mo)",bmRet = bmeqt, prefix="Conservative")
	{
		allRet = bmRet
		require(xts)
		for (pf in rev(myPFstrList)) {
			nlen = nchar(pf)
			if (substr(pf, nlen-2,nlen) == "1yr") tme = BackTestingTimeFrames(horizon = 12)
			else if (substr(pf, nlen-2,nlen) == "2yr") tme = BackTestingTimeFrames(horizon = 24)
			else if (substr(pf, nlen-2,nlen) == "6mo") tme = BackTestingTimeFrames(horizon = 6)
			else stop("cannot compute testing time periods")
			ret = myStartegyRetV4(myPFlist=pf, tm=tme, RET = ret1)
			allRet = cbind(ret, allRet)		
		}
		
		tme6m = BackTestingTimeFrames(horizon = 6) # use the minimal horizon to cover all cases
		for (ih in 1:length(tme6m$hstarts)) {
			## for each opt PF, compute the performance of the PF for the next Qtr (DeltaH)
			#if (ih %in% c(29:35,44:48)) next
			myRet =  allRet[paste(as.character(tme6m$Delhstr[ih]),"/", as.character(tme6m$Delhend[ih]),sep="")]
			if (ih==1) myQRet=t(table.AnnualizedReturns(myRet, scale=261))[,1]
			else  myQRet=rbind(myQRet,t(table.AnnualizedReturns(myRet, scale=2))[,1])
		}
	
		rownames(myQRet) = as.character(tme6m$Delhend)
		#print(head(myQRet))
		myCols=   c(topo.colors(6)[1:length(myPFstrList)], c("orange", "green", "blue"))
		win.graph(width=10, height=7, pointsize=11)
		par(mfrow=c(1,1), mar=c(4,4,2,2))
		barplot(t(myQRet), beside=T, legend.text=colnames(myQRet),las=2,cex.axis=.7, cex.names=1,
			main = ttl, col.main= "darkseagreen" ,col = myCols,
			ylim= c(min(myQRet,na.rm=F), max(myQRet,na.rm=F)*1.1),
			args.legend = list(x = "topleft") )
		savePlot(file = paste("QOuts_18\\", prefix, "QrtRet.emf", sep=""), type="emf") # Plots of Cum Returns
		Sys.sleep(40);dev.off()
	}

	
#-----------------------------------------------------------------------------------#
#	Computing Overall Performance of one or more optimum PFs, added on 23-May-2018
#-----------------------------------------------------------------------------------#
	myPF_Overall_Performance = function(myPFstrList = c("optCPF3h1yr","optCPF3h6mo" ,"optCPF5_1h6mo"),
			ttl = "Overall Performance of CPF3 (@H= 1yr, 6mo) & CPF5 (@H = 6mo)",
				risk = "SD",	bmRet = bmeqt, prefix="Conservative")
	{
		allRet = bmRet
		require(xts)
		for (pf in rev(myPFstrList)) {
			nlen = nchar(pf)
			if (substr(pf, nlen-2,nlen) == "1yr") tme = BackTestingTimeFrames(horizon = 12)
			else if (substr(pf, nlen-2,nlen) == "2yr") tme = BackTestingTimeFrames(horizon = 24)
			else if (substr(pf, nlen-2,nlen) == "6mo") tme = BackTestingTimeFrames(horizon = 6)
			else stop("cannot compute testing time periods")
			ret = myStartegyRetV4(myPFlist=pf, tm=tme, RET = ret1)
			allRet = cbind(ret, allRet)		
		}
	
#		retChk = Select_RET_V2 (RET=ret1, start="2016-01-01", end="2017-12-31", cut=0.2, dbg=0)
		ret2 = xts(ret1, order.by=as.Date(rownames(ret1)))
#		ret2 = ret2["2010-07-01/2014-06-30"]
		retChk2 = cbind(allRet, ret2)
		retChk2 = retChk2["2016-01-01/2017-12-31"]
		annRetChk = Return.annualized(retChk2)
		if (risk == "SD") annRskChk = StdDev.annualized(retChk2)
		else if (risk == "CVaR") annRskChk = CVaR(retChk2)
		else if (risk == "MaxDrawDown") annRskChk = maxDrawdown(retChk2)
		else stop("Valid options for risk are: SD, CVaR, and MaxDrawDown") 
		
		n = dim(allRet)[2]
		myCols=   c(topo.colors(6)[1:length(myPFstrList)], c("red", "green", "orange"))
		xmin = min(0, min(annRskChk,na.rm=T)); xmax =max(0, max(annRskChk,na.rm=T))
		
		win.graph(width=10, height=7, pointsize=11)
		par(mfrow=c(1,1), mar=c(4,4,2,2))
		plot(annRskChk[,c(1:n)],annRetChk[,c(1:n)], pch=18, cex=2,col=myCols, ylim=c(0,max(annRetChk)),xlim=c(xmin,xmax),
			xlab=paste("Risk:",risk), ylab = "Annualized Return",	main= ttl,col.main= "darkseagreen")
		legend(xmin, max(annRetChk),  pch = 18, col =myCols, text.col =myCols, cex = .7,
				legend = colnames(annRskChk)[1:n])
		points(annRskChk[,-c(1:n)],annRetChk[,-c(1:n)], col="gray3") 		
		text(xmax,max(annRetChk), "Time Period = 2016-01-01 to 2017-12-31", col= "darkseagreen", adj=1, cex=0.8)	
#		savePlot(file = "OverallPerfPF3&5xH1yr2yr6mo.emf", type="emf") 
		savePlot(file = paste("QOuts_18\\", prefix, "OverallPerf_",risk,".emf", sep=""), type="emf") # Plots of Cum Returns
		Sys.sleep(40);dev.off()
	}


	