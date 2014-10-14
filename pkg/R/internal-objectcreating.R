# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


.add.frame <- function(input="plotdiagTab",type,frame.name,argument.names="",arguments="",initial.values=c(),title="",border=FALSE,entry.width="2",argument.values=c(),argument.types=c(),from=c(),to=c(),by=c(),length=c(),button.name="",button.function="",button.data="",arg.frames=c(),button.otherarg="",button.biclust="",save=TRUE,show=TRUE ,new.frames=new.frames){
	
	
	# Entry Fields
	if(type=="entryfields"){
		
		new <-  list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
	   
	   
	}
	
	# Radio buttons
	if(type=="radiobuttons"){
		
		new <-  list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,argument.values=argument.values,argument.types=argument.types)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)  # Make sure the button frames are the last ones in the list .
			return(new.frames)
		}
	}
	
	# Check Boxes
	if(type=="checkboxes"){
		new <- list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
		
	}
	
	# Slider Values
	if(type=="valuesliders"){
		new <- list(type=type,title=title,border=border,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,length=length)
		
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
	}
	
	# Spin Boxes
	if(type=="spinboxes"){
		new <- list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,entry.width=entry.width,title=title,border=border)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
	}
	
	
	### SPECIFIC INPUT FOR PLOTDIAG FRAME ###
	if(input=="plotdiagTab"){
		# Manual Buttons
		
		if(type=="buttons"){
			new <- list(frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.otherarg=button.otherarg,arg.frames=arg.frames,button.biclust=button.biclust,title="",border=FALSE,save=save,show=show)
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
		
		
		
		
	}
	
	
	### SPECIFIC INPUT FOR ENSEMBLE METHODS ###
	
	
	# TO BE ADDED IN FUTURE
	
	
	
	
}



.order.button.frames <- function(new.frames){
	
	boolean <- sapply(new.frames$plotdiagTab,FUN=function(x){x$type=="buttons"})
	new.frames$plotdiagTab <- new.frames$plotdiagTab[order(boolean)]
	
	return(new.frames)
}


#.as.var <- function(x){ return(eval.parent( as.name(x)  ,n=1))}



.find.frame <- function(x,frame.name){
	temp.names <- (lapply(x,FUN=function(d){return(d$frame.name)}))
	
	find.boolean <- temp.names==frame.name
	
	if(sum(find.boolean)==1){
		return(which(find.boolean))
	}
	
	else if(sum(find.boolean)>1){ 
		stop(paste("'",frame.name,"' is used for multiple frames!",sep=""),call.=FALSE)
	}
	else{
		stop(paste("'",frame.name,"' is not recognised as a framename. Check your 'grid.config' matrix.",sep=""),call.=FALSE)

	}
	
	
}


.eval.command <- function(x){return(eval.parent(parse(text=x),n=1))}


.combine.rows <- function(input,rows,title,border,grid.rows,grid.config){
	
	all.grid.rows <- grid.rows
	all.grid.config <- grid.config
	
	
	eval(parse(text=paste("grid.rows <- all.grid.rows$",input,sep="")))
	eval(parse(text=paste("grid.config <- all.grid.config$",input,sep="")))
	
	# The names of the frames involved in this combined row are extracted. This information is needed in the template function.
	name.frames <- as.vector(grid.config[rows,])
	name.frames <- name.frames[!is.na(name.frames)]
	
	new <- list(rows=rows,title=title,border=border,name.frames=name.frames)
	grid.rows[[length(grid.rows)+1]] <- new
	
	# In order to keep the grid.rows object correct. Sort the list, based on the rows inside an element:
	# This will ensure making a correct grid, even if the rows were combined like first 3 & 4, then 1 & 2
	
	grid.rows <- grid.rows[order( unlist(lapply(grid.rows,FUN=function(x){return(min(x$rows))})) )]
	
	
	eval(parse(text=paste("all.grid.rows$",input," <- grid.rows",sep="")))
	
	return(all.grid.rows)
}


.make.correct.frame <- function(title,border,window){
	
	if(title!="" & border==TRUE){
		
		return(ttklabelframe(window,text=gettextRcmdr(title)))
		
	}
	else{
		
		if(border==TRUE){relief<-"groove"} else {relief <- "flat"}
		
		return(tkframe(window,relief=relief,borderwidth=2))
		
	}
	
}


.update.biclustering.object <- function(object,where="all"){
	
	if(!("biclustering.objects" %in% ls(envir=.GlobalEnv))){
		biclustering.objects <- list()
		
		biclustering.objects$all <- c()
		biclustering.objects$bcdiag <- c()
		biclustering.objects$superbiclust <- c()
		
		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
	}
	
	if(where=="all"){
		#cat("UPDATEALL")
		biclustering.objects$all <- unique(c(biclustering.objects$all,object))
		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
	}
	
	if(where=="bcdiag"){
		biclustering.objects$bcdiag <- unique(c(biclustering.objects$bcdiag,object))
		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
	}
	
	if(where=="superbiclust"){
		biclustering.objects$superbiclust <- unique(c(biclustering.objects$superbiclust,object))
		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
	}
}

.initialize.new.frames <- function(){
	new.frames <- list()
	new.frames$clusterTab <- list()
	new.frames$plotdiagTab <- list()
	return(new.frames)
}

.initialize.grid.config <- function(){
	grid.config <- list()
	grid.config$clusterTab <- list()
	grid.config$plotdiagTab <- list()
	return(grid.config)
	
}

.initialize.grid.rows <- function(){
	grid.rows <- list()
	grid.rows$clusterTab <- list()
	grid.rows$plotdiagTab <- list()
	return(grid.rows)
	
}

.grid.matrix <- function(input,data,grid.config=grid.config,...){
	temp <- matrix(data=data,...)
	
	if(input=="clusterTab"){
		grid.config$clusterTab <- temp
		return(grid.config)
	}
	if(input=="plotdiagTab"){
		grid.config$plotdiagTab <- temp
		return(grid.config)
	}
	
}


.build.command.argument <- function(current.frame,command){
	
	if(current.frame$type=="entryfields"){
		
		number.entries <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.entries){
			
			if(current.frame$argument.types[j]=="num"){
				add.command <- if(tclvalue(current.frame$entry.vars[[j]])==""){""} else {paste(",",arguments[j],"=",tclvalue(current.frame$entry.vars[[j]]),sep="")}
			}
			if(current.frame$argument.types[j]=="char"){
				add.command <- if(tclvalue(current.frame$entry.vars[[j]])==""){""} else {paste(",",arguments[j],"='",tclvalue(current.frame$entry.vars[[j]]),"'",sep="")}
				
			}
			
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
	}
	
	if(current.frame$type=="radiobuttons"){
		
				
		temp <- (tclvalue(current.frame$radioVar))
		
		if(grepl("BUTTONSTART",temp,fixed=TRUE)){
			temp <- gsub("BUTTONSTART","",temp,fixed=TRUE)
		}
		
		if(current.frame$argument.types=="char"){
			add.command <- paste( ",",current.frame$arguments,"='",temp,"'",sep=""   )
		}
		if(current.frame$argument.types=="num"){
			add.command <- paste( ",",current.frame$arguments,"=",temp,sep=""   )
		}	
			
		command <- paste(command,add.command,sep="")
		return(command)
	}
	
	if(current.frame$type=="checkboxes"){
		
		number.checks <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.checks){
			
#			temp.command <- paste("temp.var <- as.character(tclvalue(",arguments[[j]],"Variable))" ,sep="")
#			.eval.command(temp.command)
			temp.var <- as.character(tclvalue(current.frame$checkVar[[j]]))
			
			if(temp.var=="1"){check.var <- TRUE} else {check.var <- FALSE}
			
			add.command <- paste(",",arguments[j],"=",check.var,sep="")
			
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
	}
	
	if(current.frame$type=="valuesliders"){
		number.sliders <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.sliders){
			
			add.command <- paste(",",arguments[j],"=",tclvalue(current.frame$slider.vars[[j]]),sep="")
			
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
		
	}
	
	if(current.frame$type=="spinboxes"){
		number.spins <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.spins){
			
			add.command <- paste(",",arguments[j],"=",tclvalue(current.frame$spin.vars[[j]]),sep="")
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
	}
	
	
}



.transform.vector2text <- function(x){
	if(length(x)==0){return("c()")}
	
	out <- "c("
	for(i.arg in 1:length(x)){
		out <- paste(out,"'",x[i.arg],"'",sep="")
		if(i.arg!=length(x)){out <- paste(out,",",sep="")}
	}
	out <- paste(out,")",sep="")
	return(out)
}



.build.button.function <- function(function.command,arg.names,button_result,new.frames,save){ 
	
	
	for(i.frame in arg.names){
	
		boolean <- sapply(new.frames$plotdiagTab,FUN=function(x){x$frame.name==i.frame})
		temp.index <- which(boolean==TRUE)
		if(sum(boolean) ==0){stop(paste("'",i.frame,"' is not defined in 'new.frames' object",sep=""))}
		if(sum(boolean)>1){stop(paste("'",i.frame,"' is defined multiple times in 'new.frames' object",sep=""))}
		if(sum(boolean)==1){
			current.arg.frame <- new.frames$plotdiagTab[[temp.index]]
		
			function.command <- .build.command.argument(current.arg.frame,function.command)
		}
	}

	function.command <- paste(function.command,")" ,sep="")
	
	if(save==TRUE){function.command <- paste(button_result," <- ",function.command,sep="")}
	#cat(function.command)
	
	return(function.command)
}


.give.doublequote <- function(x){return(paste("\"",x,"\"",sep=""))}


Setwd <- function (x=TRUE) 
{
	wd <- tclvalue(tkchooseDirectory(initialdir = getwd(), parent = CommanderWindow()))
	if (wd != "") 
		doItAndPrint(paste("setwd(\"", wd, "\")", sep = ""))
}


.output.sparse.txt <- function(X,file){
	file <- paste(file,".txt",sep="")
	
	nrow <- dim(X)[1]
	ncol <- dim(X)[2]
	
	write(nrow,file=file,ncolumns=ncol,append=FALSE,sep=" ")
	write(ncol,file=file,ncolumns=ncol,append=TRUE,sep=" ")
	
	for(i.row in 1:nrow){
		ind <- which(X[i.row,]!=0)-1
		num <- length(ind)
		val <- X[i.row,ind+1]
		
		write(num,file=file,ncolumns=ncol,append=TRUE,sep=" ")
		write(ind,file=file,ncolumns=ncol,append=TRUE,sep=" ")
		write(val,file=file,ncolumns=ncol,append=TRUE,sep=" ")
	}
}



.is.binary.matrix <-function(x) {identical(as.vector(x),as.numeric(as.logical(x)))}


.binary.activematrix.check <- function(){
	
	dataname <- ActiveDataSet()
	eval(parse(text=paste("x<-",dataname,sep="")))
	
	
	if(!.is.binary.matrix(as.matrix(x))){
		warning.command <- "warning('The current Active Data Set is not in binary format! Use the binarize option or a different data set.',call.=FALSE)"
		justDoIt(warning.command)
	}
}

.rcmdr.warning <- function(x){
	warning.command <- paste("warning('",x,"',call.=FALSE)",sep="")
	justDoIt(warning.command)
}




