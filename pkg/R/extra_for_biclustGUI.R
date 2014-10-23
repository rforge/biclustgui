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
		doItAndPrint(paste("       # Original biclust R-code: `biclust(x=...,method=BCQuest(),ns=",ns,",nd=",nd,",sd=",sd,",number=",number,")`",sep=""))
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


superbiclust.GUI <- function(x,index,type,method_result,extra.biclust=NULL,type.method="biclust"){
	if(type.method=="biclust"){
		if(!is.null(extra.biclust)){
			
			doItAndPrint(paste("x <- combine(",method_result,",",extra.biclust[1],")",sep=""))		
			if(length(extra.biclust)>1){
				for(i.extra in 2:length(extra.biclust)){
					doItAndPrint(paste("x <- combine(x,",extra.biclust[i.extra] ,")",sep=""))
				}
			}
			doItAndPrint(paste("superbiclust.result <- BiclustSet(x)"))
		}
		if(is.null(extra.biclust)){
			doItAndPrint(paste("superbiclust.result <- BiclustSet(x=",method_result,")"))
		}
	}
	
	if(type.method=="isa"){
		doItAndPrint(paste("superbiclust.result <- BiclustSet(x=isa.biclust(", method_result  ,"))"))
	}
	if(type.method=="fabia"){
		doItAndPrint(paste("superbiclust.result <- BiclustSet(x=",method_result,")"))
		
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




biclust.robust.fuse <- function(CutTree,method_result,type="biclust",superbiclust.result){
	
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
	
	if(type=="biclust"){

	
		# Create new RowxNumber and NumberxCol of the Robust Bicluster
		RowxCol <- robust.fuse.support(robust.list=robust.list,RowxNumber=RowxNumber,NumberxCol=NumberxCol)
		RowxNumber <- RowxCol$RowxNumber
		NumberxCol <- RowxCol$NumberxCol
			

		
		
		eval(parse(text=paste("Parameters <- ",method_result,"@Parameters",sep="")))
		new.biclust <- new("Biclust", Number = dim(RowxNumber)[2], RowxNumber = RowxNumber,NumberxCol = NumberxCol,Parameters=Parameters)
		assign("new.biclust",new.biclust,envir=.GlobalEnv)		
		
		paste.cat <- paste("\nThe Original Bicluster result is saved in: ",method_result,".Original",sep="")
		cat(paste.cat)
		paste.cat <- paste("\nThe new result is saved in ",method_result,sep="")
		cat(paste.cat,"\n")
		
		
		doItAndPrint(paste(method_result,".Original <- ",method_result,sep=""))
		doItAndPrint(paste(method_result," <- new.biclust",sep=""))
		#return(new.biclust)
		
	}
	
	if(type=="isa"){
		

		
		RowxCol <- robust.fuse.support(robust.list=robust.list,RowxNumber=RowxNumber,NumberxCol=NumberxCol)
		RowxNumber <- RowxCol$RowxNumber
		NumberxCol <- RowxCol$NumberxCol
		
		

		
		rows = (RowxNumber)+0
		columns = t(NumberxCol)+0
		
		
		eval(parse(text=paste("rundata <- ",method_result,"$rundata",sep="")))
	
		new.isa <- list(rows=rows,columns=columns,seeddata=data.frame(),rundata=rundata)
		assign("new.isa",new.isa,envir=.GlobalEnv)		
				
		
		paste.cat <- paste("\nThe Original Bicluster result is saved in: ",method_result,".Original",sep="")
		cat(paste.cat)
		paste.cat <- paste("\nThe new result is saved in ",method_result,sep="")
		cat(paste.cat,"\n")
		
		
		doItAndPrint(paste(method_result,".Original <- ",method_result,sep=""))
		doItAndPrint(paste(method_result," <- new.isa",sep=""))
		
	}
	
	if(type=="fabia"){

		
		RowxCol <- robust.fuse.support(robust.list=robust.list,RowxNumber=RowxNumber,NumberxCol=NumberxCol)
		RowxNumber <- RowxCol$RowxNumber
		NumberxCol <- RowxCol$NumberxCol
		
		
		Parameters <- list()
		new.biclust <- new("Biclust", Number = dim(RowxNumber)[2], RowxNumber = RowxNumber,NumberxCol = NumberxCol,Parameters=Parameters)
		assign("new.biclust",new.biclust,envir=.GlobalEnv)		
		
		paste.cat <- paste("\nThe Original Bicluster result is saved in: ",method_result,".Original",sep="")
		cat(paste.cat)
		paste.cat <- paste("\nThe new result is saved in ",method_result,sep="")
		cat(paste.cat,"\n")
		
		
		doItAndPrint(paste(method_result,".Original <- ",method_result,sep=""))
		doItAndPrint(paste(method_result," <- new.biclust",sep=""))
		
		

		
		
	}
	
	
}

robust.reset <- function(method_result){
	doItAndPrint(paste(method_result,"<- ",method_result,".Original",sep=""))
}


writeBic.GUI <- function(dset,fileName,bicResult,bicname,mname=c("fabia","isa2","biclust"),append=TRUE,delimiter=" "){
	fileName <- paste(fileName,".txt",sep="")
	writeBic(dset=dset, fileName=fileName, bicResult=bicResult, bicname=bicname, mname =mname, append = append, delimiter = delimiter)
	
	
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
