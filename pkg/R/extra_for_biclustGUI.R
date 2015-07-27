# Project: Master Thesis
# 
# Author: Gebruiker
###############################################################################



parallelCoordinates2 <- function (x, bicResult, number, info = TRUE) 
{
	bicRows = which(bicResult@RowxNumber[, number])
	bicCols = which(bicResult@NumberxCol[number, ])
	NotbicCols = which(bicResult@NumberxCol[number, ] == F)
	mat <- x[bicRows, ]
	mat2 <- cbind(mat[, bicCols], mat[, NotbicCols])
	colname <- c(bicCols, NotbicCols)
	plot(mat2[1, ], type = "l", col = "white", ylim = c(min(mat), 
					max(mat)), axes = F, xlab = "columns", ylab = "value")
	axis(1, 1:ncol(mat2), labels = colname)
	axis(2)
	if (length(bicCols) != ncol(mat2)) {
		for (i in 1:nrow(mat2)) {
			pos <- (length(bicCols) + 1):ncol(mat2)
			lines(pos, mat2[i, pos], type = "l")
		}
	}
	for (i in 1:nrow(mat2)) lines(mat2[i, c(1:length(bicCols))], 
				type = "l", col = "red")
	if (info) {
		title(main = paste("Bicluster", number, "\n(rows=", length(bicRows), 
						";", "columns=", length(bicCols), ")", sep = " "))
	}
}




parallelCoordinates3 <- function(x,bicResult,number,type2="default",plotBoth=FALSE,plotcol=TRUE,compare=TRUE,info=FALSE,bothlab=c("Rows","Columns"),order=FALSE,order2=0,ylab="Value",col=1,...){
	
	if(type2=="combined"){
		info=TRUE
		#assign("x",x,envir=.GlobalEnv)
		#assign("bicResult",bicResult,envir=.GlobalEnv)
		#doItAndPrint("parallelCoordinates2(x,bicResult,number,info)")
		parallelCoordinates2(x,bicResult,number,info)
	}
	
	if(type2=="default"){
		info=TRUE
		doItAndPrint(paste("       # Original biclust R-code: 'parallelCoordinates(x=...,bicResult=...,number=",number,",plotBoth=",plotBoth,",plotcol=",plotcol,",compare=",compare,")'",sep=""))
		parallelCoordinates(x,bicResult,number,plotBoth,plotcol,compare,info,bothlab,order,order2,ylab,col,...)
	}
	
}


biclust.quest.GUI <- function(x,method,ns,nd,sd,alpha,number,d,quant,vari){
	
	if(method=="BCQuest"){
		doItAndPrint(paste("       # Original biclust R-code: `biclust(x=...,method=BCQuest(),ns=",ns,",nd=",nd,",sd=",sd,",alpha=",alpha,",number=",number,")`",sep=""))
		return(biclust(x=x, method=BCQuest(), ns=ns, nd=nd, sd=sd, alpha=alpha, number=number))
		
	}
	
	if(method=="BCQuestord"){
		doItAndPrint(paste("       # Original biclust R-code: `biclust(x=...,method=BCQuestord(),d=",d,",ns=",ns,",nd=",nd,",sd=",sd,",alpha=",alpha,",number=",number,")`",sep=""))
		
		return(biclust(x=x, method=BCQuestord(), d=d, ns=ns, nd=nd, sd=sd, alpha=alpha, number=number))
	}
	
	if(method=="BCQuestmet"){
		doItAndPrint(paste("       # Original biclust R-code: `biclust(x=...,method=BCQuestmet(),quant=",quant,",vari=",vari,",ns=",ns,",nd=",nd,",sd=",sd,",alpha=",alpha,",number=",number,")`",sep=""))
		
		return(biclust(x=x, method=BCQuestmet(), quant=quant, vari=vari, ns=ns, nd=nd, sd=sd, alpha=alpha, number=number))
	}
	
}

biclust.bimax.GUI <- function(x,method,minr,minc,number,maxc,backfit,n2,maxbicheck){
	if(maxbicheck==2){
		doItAndPrint(paste("       # Original biclust R-code: 'biclust:::maxbimaxbiclust(x=...,minr=",minr,",minc=",minc,",backfit=",backfit,",n2=",n2,",number=",number,")'",sep=""))
		return(biclust:::maxbimaxbiclust(logicalmatrix=x,minr=minr,minc=minc,backfit=backfit,n2=n2,number=number))
	}
	
	if(maxbicheck==3){
		doItAndPrint(paste("       # Original biclust R-code: 'biclust(x=...,method=BCBimax(),minr=",minr,",minc=",minc,",number=",number,")'",sep=""))
		return(biclust(x=x,method=BCBimax(),minr=minr,minc=minc,number=number))
	}
	if(maxbicheck==1){
		doItAndPrint(paste("       # Original biclust R-code: 'biclust(x=...,method=BCrepBimax(),minr=",minr,",minc=",minc,",number=",number,")'",sep=""))
		return(biclust(x=x,method=BCrepBimax(),minr=minr,minc=minc,number=number,maxc=maxc))
	}
	
}


fabia.biplot <- function(x,dim1,dim2){
	plot.command <- paste("plot(x=x,dim=c(",dim1,",",dim2,"))",sep="")
	doItAndPrint(paste("       # Original R-code: 'plot(x=...,dim=c(",dim1,",",dim2,"))'",sep=""))
	.eval.command(plot.command)
}


spfabia.GUI <- function(X,p,alpha,cyc,spl,spz,non_negative,random,write_file,norm,scale,lap,nL,lL,bL,samples,initL,iter,quant,lowerB,upperB,dorescale,doini,eps,eps1,datatype,filename){
	if(write_file==TRUE){write_file <- 1}else{write_file <- 0}
	
	if(datatype==1){
		command <- paste("spfabia(X='",filename,"',p=",p,",alpha=",alpha,",cyc=",cyc,",spl=",spl,",spz=",spz,",non_negative=",non_negative,",random=",random,",write_file=",write_file,",norm=",norm,",scale=",scale,",lap=",lap,",lL=",lL,",samples=",samples,",initL=,",initL,",iter=",iter,",quant=",quant,",lowerB=",lowerB,",upperB=",upperB,",dorescale=",dorescale,",doini=",doini,",eps=",eps,",eps1=",eps,")",sep="")
		
		Setwd()
		
		doItAndPrint(paste("       # Original R-code: ",command,sep=""))
	
		return(eval(parse(text=command)))
		#return(spfabia(X=filename,p=p,alpha=alpha,cyc=cyc,spl=spl,spz=spz,non_negative=non_negative,random=random,write_file=write_file,norm=norm,scale=scale,lap=lap,lL=lL,samples=samples,initL=initL,iter=iter,quant=quant,lowerB=lowerB,upperB=upperB,dorescale=dorescale,doini=doini,eps=eps,eps1=eps1))
		
	}
	
	
	if(datatype==2){
		Setwd()
		
		
		.output.sparse.txt(X=X,file=filename)
		
		command <- paste("spfabia(X='",filename,"',p=",p,",alpha=",alpha,",cyc=",cyc,",spl=",spl,",spz=",spz,",non_negative=",non_negative,",random=",random,",write_file=",write_file,",norm=",norm,",scale=",scale,",lap=",lap,",lL=",lL,",samples=",samples,",initL=,",initL,",iter=",iter,",quant=",quant,",lowerB=",lowerB,",upperB=",upperB,",dorescale=",dorescale,",doini=",doini,",eps=",eps,",eps1=",eps,")",sep="")
		
		doItAndPrint(paste("       # Original R-code: ",command,sep=""))
		
		return(.eval.command(command))
		#return(spfabia(X=filename,p=p,alpha=alpha,cyc=cyc,spl=spl,spz=spz,non_negative=non_negative,random=random,write_file=write_file,norm=norm,scale=scale,lap=lap,lL=lL,samples=samples,initL=initL,iter=iter,quant=quant,lowerB=lowerB,upperB=upperB,dorescale=dorescale,doini=doini,eps=eps,eps1=eps1))
		
	}
	
}


isa.GUI <- function(data,thr.row.from,thr.row.to,thr.row.by,thr.col.from,thr.col.to,thr.col.by,no.seeds,dir.row,dir.col){
	
	thr.row <- seq(thr.row.from,thr.row.to,by=thr.row.by)
	thr.col <- seq(thr.col.from,thr.col.to,by=thr.col.by)
	
	direction <- c(dir.row,dir.col)
	doItAndPrint(paste("       # Original R-Code: isa(data=...,thr.row=seq(",thr.row.from,",",thr.row.to,",by=",thr.row.by,"),thr.col=seq(",thr.col.from,",",thr.col.to,",by=",thr.col.by,"),no.seeds=",no.seeds,",direction=c('",dir.row,"','",dir.col,"'))",sep=""))
	return(isa(data=data,thr.row=thr.row,thr.col=thr.col,no.seeds=no.seeds,direction=direction))
}



isa.summary <- function(ISA){
	if(dim(ISA[[1]])[2]==0){stop("Summary not available. No biclusters were discovered.",call.=FALSE)}
	if(dim(ISA[[3]])[1]==0){stop("Not available after superbiclust",call.=FALSE)}
	
	nclus <- dim(ISA$rows)[2]
	if(nclus<=10){ncluscat <- nclus}else{ncluscat <- 10}
	
	rob.cat <- round(sort(ISA[[3]]$rob,decreasing=TRUE)[1:ncluscat],2)
	index.cat <- order(ISA[[3]]$rob,decreasing=TRUE)[1:ncluscat]
	
	data <- isa.extract(ISA)
	
	cat(paste("Number of Clusters found: ",nclus,sep=""))
	cat("\n")
	cat("\n")
	cat(paste("Top ",ncluscat," robust biclusters (Robust Score):",sep=""))
	cat("\n")
	cat("\n")
	
	nrows <- sapply(data[index.cat],FUN=function(x){length(x$rows)})
	ncols <- sapply(data[index.cat],FUN=function(x){length(x$columns)})
	print.cat <- rbind(rob.cat,nrows,ncols)
	rownames(print.cat) <- c("Robust Score","Number of Rows","Number of Columns")

	print(print.cat)

}


isa.extract <- function(modules){
	mymodules <- lapply(seq(ncol(modules$rows)), function(x) {
						list(rows=which(modules$rows[,x] != 0),
						columns=which(modules$columns[,x] != 0))
		})

	nclus <- dim(modules$rows)[2]
	if(nclus<=10){max <- nclus}else{max <- 10}
	
	names(mymodules) <- sapply(seq(1:nclus),FUN=function(x){paste("BC",x,sep="")})
	
	#print(str(mymodules[1:max]))
	#cat("\n[list output truncated]")
	
	return(mymodules)
}


isa.scoreplots <- function(ISA,type="row",biclust=c(1)){
	layout(cbind(seq(length(biclust))))
	
	if(type=="row"){
		modules <- ISA$rows
	}
	if(type=="col"){
		modules <- ISA$columns
	}
	
	for(i.clust in biclust){
		par(mar=c(3,5,1,1))
		plot(modules[,i.clust],ylim=c(-1,1),ylab="Scores",xlab=NA,cex.lab=2,cex.axis=1.5,main=paste("BC",i.clust,sep=""))
	
	}
}


superbiclust.GUI <- function(x,index,type,method_result,extra.biclust=NULL,fabia.thresZ=0.5,fabia.thresL=NULL){
	
	### automize...
	# first, use transform on all the inputted objects...
	
	
	
	if(!is.null(extra.biclust)){
		
		
		# COMPARED WITH BCDIAG PRE-AMBLE: Do not need to check if in the names to use object for class(). Button will be blocked if object not available anymway
		# Need to have an IF for fabia

		temp.method_result <- .tobiclust_transf(method_result,thresZ=fabia.thresZ,thresL=fabia.thresL)
		temp.extra.biclust <- .tobiclust_transf(extra.biclust[1],thresZ=fabia.thresZ,thresL=fabia.thresL)	
		
		
		doItAndPrint(paste("x <- combine(",temp.method_result,",",temp.extra.biclust,")",sep=""))		
		if(length(extra.biclust)>1){
			for(i.extra in 2:length(extra.biclust)){
				temp.extra.biclust <- .tobiclust_transf(extra.biclust[i.extra],thresZ=fabia.thresZ,thresL=fabia.thresL)	
				doItAndPrint(paste("x <- combine(x,",temp.extra.biclust ,")",sep=""))
			}
		}
		doItAndPrint(paste("superbiclust.result <- BiclustSet(x)"))
	}
	if(is.null(extra.biclust)){
		temp.method_result <- .tobiclust_transf(method_result,thresZ=fabia.thresZ,thresL=fabia.thresL)
		
		doItAndPrint(paste("superbiclust.result <- BiclustSet(x=",temp.method_result,")"))
	
	}
	
	doItAndPrint(paste("superbiclust.sim <- similarity(x=superbiclust.result,index='",index,"',type='",type,"')",sep=""))
	doItAndPrint(paste("superbiclust.tree <- HCLtree(superbiclust.sim)",sep=""))
	doItAndPrint("superbiclust.result")

	
}


superbiclust.robust.GUI <- function(CutTree,show.which=FALSE){
	tree.table <- table(CutTree)
	
	table.index <- which(tree.table>1)
	
	#as.numeric(names(tree.table[table.index])),
	cluster.matrix <- rbind(as.numeric(names(tree.table[table.index])),tree.table[table.index])
	colnames(cluster.matrix) <- rep(NA,dim(cluster.matrix)[2])
	rownames(cluster.matrix) <- c("Robust Bicluster:","#BC's inside:")
	
	cat("\n")
	cat("Robust Biclusters created from more than 1 bicluster:")
	cat("\n")
	print(cluster.matrix)
	if(show.which==TRUE){
		cat("\n")
		cat("Which Biclusters Inside?")
		cat("\n")
		
		for(i.index in 1:length(table.index)){
			temp <- as.numeric(names(table.index[i.index]))
			cat("Robust BC",temp,": ",which(CutTree==temp))
			cat("\n")
		}
	}
}

plotSuper.GUI <- function(type,which.robust,CutTree){
	x <- which(CutTree==which.robust)
	
	x.print <- paste("c(",x[1]  ,sep="")
	for(i.x in 2:length(x)){
		x.print <- paste(x.print,",",x[i.x],sep="")
	}
	x.print <- paste(x.print,")",sep="")
	
	input.data <- ActiveDataSet()
	input.data <- paste("as.matrix(",input.data,")",sep="")
	
	if(type=="within"){
		
		doItAndPrint(paste("plotSuper(x=",x.print,",data=",input.data,",BiclustSet=superbiclust.result)"   ,sep=""))
		
		#plotSuper(x=x,data=data,BiclustSet=BiclustSet)
	}
	
	if(type=="all"){
		
		doItAndPrint(paste("plotSuperAll(x=",x.print,",data=",input.data,",BiclustSet=superbiclust.result)"   ,sep=""))
		#plotSuperAll(x=x,data=data,BiclustSet=BiclustSet)
	}
	
}




biclust.robust.fuse <- function(CutTree,method_result,superbiclust.result){
	
	robust.list <- list()
	
	tree.table <- table(CutTree)
	table.index <- which(tree.table>1)
	
	for(i.index in 1:length(table.index)){
		temp <- as.numeric(names(table.index[i.index]))
		
		robust.list[[i.index]] <- list()
		robust.list[[i.index]]$robust.number <- temp
		robust.list[[i.index]]$robust.inside <- which(CutTree==temp)
		
		
	}
	
	# Extract the original or combined biclust result from the superbiclust.result 
	# Necessary conversions are done in the step of making superbiclust.result !!!
	RowxNumber <- superbiclust.result@GenesMembership
	NumberxCol <- superbiclust.result@ColumnMembership
	
	
	# Create new RowxNumber and NumberxCol of the Robust Bicluster
	RowxCol <- robust.fuse.support(robust.list=robust.list,RowxNumber=RowxNumber,NumberxCol=NumberxCol)
	RowxNumber <- RowxCol$RowxNumber
	NumberxCol <- RowxCol$NumberxCol
			
		
#	Parameters <- list()
	#eval(parse(text=paste("Parameters <- ",method_result,"@Parameters",sep="")))
	new.biclust.command <- paste0("new.biclust <- new(\"Biclust\", Number = ",dim(RowxNumber)[2],", RowxNumber = RowxNumber,NumberxCol = NumberxCol,Parameters=list())")
#	assign("new.biclust",new.biclust,envir=.GlobalEnv)	
	
		
	# Making RowxNumber and NumberxCol in justDoIt - Very superfluous but necessary for using no global assigning for CRAN policies
	.matrix2vectorasstring <- function(x){
		dim.x <- dim(x)
		x <- as.vector(x)
		
		command <- "matrix(c("
	
		x.string <- sapply(x,function(y){return(paste0(y))})
		x.collapse <- paste0(x.string,collapse=",")
	
		command <- paste0(command,x.collapse,"),nrow=",dim.x[1],",ncol=",dim.x[2],")")
		return(command)
	}
	justDoIt(paste0("RowxNumber <- ",.matrix2vectorasstring(RowxNumber)))
	justDoIt(paste0("NumberxCol <- ",.matrix2vectorasstring(NumberxCol)))
	
	
	
	paste.cat <- paste("\nThe Original Bicluster result is saved in: ",method_result,".Original",sep="")
	cat(paste.cat)
	paste.cat <- paste("\nThe new result is saved in ",method_result,sep="")
	cat(paste.cat,"\n")
		
		
	doItAndPrint(paste(method_result,".Original <- ",method_result,sep=""))
	doItAndPrint(new.biclust.command)
	doItAndPrint(paste(method_result," <- new.biclust",sep=""))
	
	
}

robust.reset <- function(method_result){
	doItAndPrint(paste(method_result,"<- ",method_result,".Original",sep=""))
}


writeBic.GUI <- function(dset,fileName="",bicResult,bicname,mname=c("fabia","isa2","biclust"),append=TRUE,delimiter=" ",fabia.thresZ=0.5,fabia.thresL=NULL){
	
	# In the case, filename is not given, use save window:
	if(fileName==""){
		fileName <- tclvalue(tkgetSaveFile(initialfile="ExportResult.txt",filetypes="{{Text Files} {.txt}} {{All files} *}"))
		
	}
	else{
		fileName <- paste(fileName,".txt",sep="")
		
	}
		
	writeBic(dset=dset, fileName=fileName, bicResult=bicResult, bicname=bicname, mname =mname, append = append, delimiter = delimiter,fabia.thresZ=fabia.thresZ,fabia.thresL=fabia.thresL)
	
	
}	

rqubic.GUI <- function(x,eSetData.name,q,rank,minColWidth,report.no,tolerance,filter.proportion,check.disc){

	# x is already a eSet object. It will not be used here, just for show (make it fit in the framework)

	
	if(check.disc==TRUE){
	# Picking the correct data and doing Discretization!
	# 
		if(eSetData.name!="NULL"){
			doItAndPrint(paste0("data.disc <- quantileDiscretize(",eSetData.name,",q=",q,",rank=",rank,")"))
			
			justDoIt(paste0(eSetData.name,"_exprs <- as.data.frame(exprs(",eSetData.name,"))"))
			activeDataSet(paste0(eSetData.name,"_exprs"))
		}
		else{
			doItAndPrint(paste0("data.disc <- quantileDiscretize(as.ExprSet(",ActiveDataSet(),"),q=",q,",rank=",rank,")"))
		}
	}
	else{
		if(eSetData.name!="NULL"){
			doItAndPrint(paste0("data.disc <-" ,eSetData.name))
			
			justDoIt(paste0(eSetData.name,"_exprs <- as.data.frame(exprs(",eSetData.name,"))"))
			activeDataSet(paste0(eSetData.name,"_exprs"))
		}
		else{
			doItAndPrint(paste0("data.disc <- as.ExprSet(",ActiveDataSet(),")"))
		}
	}
	
	
	
	doItAndPrint(paste0("data.seed <- generateSeeds(data.disc,minColWidth=",minColWidth,")"))
	doItAndPrint(paste0("Rqubic <- quBicluster(data.seed,data.disc,report.no=",report.no,",tolerance=",tolerance,",filter.proportion=",filter.proportion,")"))
	doItAndPrint("Rqubic")
	
	
}

bicare.GUI <- function(Data,k,pGene,pSample,r,N,M,t,blocGene,blocSample,eSetData.name){
	
	if(pSample=="pGene"){pSample <- pGene}
	if(eSetData.name!="NULL"){
				
		justDoIt(paste0(eSetData.name,"_exprs <- as.data.frame(exprs(",eSetData.name,"))"))
		activeDataSet(paste0(eSetData.name,"_exprs"))
		
		doItAndPrint(paste0("BICARE <- FLOC(Data=",eSetData.name,",k=",k,",pGene=",pGene,",pSample=",pSample,",r=",r,",N=",N,",M=",M,",t=",t,",blocGene=",blocGene,",blocSample=",blocSample,")"))
		doItAndPrint("BICARE")
	}
	
	else{
		doItAndPrint(paste0("BICARE <- FLOC(Data=as.matrix(",ActiveDataSet(),"),k=",k,",pGene=",pGene,",pSample=",pSample,",r=",r,",N=",N,",M=",M,",t=",t,",blocGene=",blocGene,",blocSample=",blocSample,")"))
		doItAndPrint("BICARE")
		
	}
	
	
}

summaryBICARE <- function(x){
	return(x$mat.resvol.bic)
}

bicare.residuplot <- function(result){
	result_residu <- result$mat.resvol.bic[,1]
	x <- seq(1,length(result_residu),1)
	mr <- max(result_residu)
	plot(x,result_residu,xlab="Bicluster Number",ylab="Residu",main="Residues of Biclusters",ylim=c(0,mr+0.05*mr))
}

bicare.genesetenrichment <- function(result,setType,gene.from,gene.to){
	if(identical(annotation(result$ExpressionSet),character(0))){
		justDoIt(paste0("warning('Not a complete eSet!',call.=FALSE)"))
	}
	else{
		doItAndPrint(paste0("GSC <- GeneSetCollection(BICARE$ExpressionSet[",gene.from,":",gene.to,"],setType=",setType,")"))
		doItAndPrint(paste0("BICARE <- testSet(BICARE,GSC)"))
		doItAndPrint(paste0("BICARE$geneSet"))
	}
}



bicare.samplesenrichment <- function(result,covariates){
	if(dim(pData(result$ExpressionSet))[2]==0){
		justDoIt(paste0("warning('Not a complete eSet!',call.=FALSE)"))
	}
	else{
		eval(parse(text=paste0("temp <- names(pData(result$ExpressionSet))[",covariates,"]")))
		temp2 <- "c("
		for(i.names in 1:length(temp)){
			temp2 <- paste0(temp2,",'",temp[i.names],"'")
		}
		temp2 <- paste0(temp2,")")
		temp2 <- gsub("\\(,","\\(",temp2) 
		doItAndPrint(paste0("BICARE <- testAnnot(BICARE,annot=pData(BICARE$ExpressionSet),covariates=",temp2,")"))
		justDoIt("temp <- BICARE$covar")
		justDoIt("cov.counts <- temp[[1]]")
		justDoIt("p <- temp$pvalues")
		justDoIt("adjp <- temp$adjpvalues")
		justDoIt("colnames(p) <- .putbefore(colnames(p),'pvalue.')")
		justDoIt("colnames(adjp) <- .putbefore(colnames(adjp),'adjpvalue.')")
		justDoIt("padjp <- cbind(p,adjp)")
		justDoIt("samplesenrichment <- list(cov.counts=cov.counts,pvalues=padjp)")
		doItAndPrint("samplesenrichment")
	}
	
}

bicare.pdata <- function(result){
	doItAndPrint("pData(BICARE$ExpressionSet)")
}

bicare.makereport <- function(dirName){
	Setwd()
	doItAndPrint(paste0("makeReport(dirPath=getwd(),dirName='",dirName,"',resBic=BICARE,browse=TRUE)"))
}


chooseresultsGUI <- function(methodname,toolname){
	
	initializeDialog(title = gettextRcmdr("Choose Results...")) 
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	AllResults <- .makeResultList()
		
	# Remove the one which from which the superbiclust was used
	AllResults <- AllResults[!(AllResults==method_result)]
	
	resultFrame <- tkframe(top)
	buttonFrame <- tkframe(top)
		
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	onOK <- function(){
		# ALSO MAKE AN IF IF NOTHING IS SELECTED, USE ONCANCEL
		sel <- as.integer(tkcurselection(resultBox))+1
		if(length(AllResults)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
			onCancel()
		}
		else if(length(sel)==0){
			onCancel()
		}
		else{
			sel.result.name <- AllResults[sel]
			
			out.vector <- "c("
			for(i.sel in 1:length(sel.result.name)){
				out.vector <- paste0(out.vector,"'",sel.result.name[i.sel],"'")
				if(i.sel!=length(sel.result.name)){out.vector <- paste0(out.vector,",")}
			}
			out.vector <- paste0(out.vector,")")
			
			method_result <- gsub(" ","",methodname,fixed=TRUE)
			method_result <- gsub("-","",method_result,fixed=TRUE)
			
			biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
			
			eval(parse(text=paste0("temp.env <- biclustering.objects$ENVIR$",toolname,method_result)))
			
			eval(parse(text=paste0("tclvalue(new.frames[[2]][[1]]$entry.vars[[1]]) <- \"",out.vector,"\"")),envir=temp.env)
		
			onCancel()
		}
	}
	
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="multiple", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	
	tkgrid(labelRcmdr(top,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results (Select 1 or more):")),sticky="nw")
		
	tkgrid(resultBox,resultScroll,sticky="nws") 
	tkgrid.configure(resultScroll,sticky="nws")
	
	selectButton <- buttonRcmdr(buttonFrame,command=onOK,text=gettextRcmdr("Ok"),foreground="darkgreen",width="6",borderwidth=3,default="active")
	exitButton <- buttonRcmdr(buttonFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	
	tkgrid(selectButton,exitButton,sticky="ew")
	tkgrid.columnconfigure(buttonFrame, 0, weight=1)
	tkgrid.columnconfigure(buttonFrame, 1, weight=1)
	tkgrid.configure(selectButton,sticky="w")
	tkgrid.configure(exitButton,sticky="e")
	
	tkgrid(resultFrame,sticky="nwse")
	tkgrid(buttonFrame,pady="10",sticky="ew")
	
	dialogSuffix(onOK=onOK)
	
}
