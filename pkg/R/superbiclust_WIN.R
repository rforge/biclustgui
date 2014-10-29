# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


superbiclust_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	
	biclust.names <- c("Bimax","CC","Plaid","Questmotif","Spectral","XMotifs","IBBIG","Rqubic")
	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
	isa.names <- c("ISA")
	bicare.names <- c("BICARE")
	
	if(methodname %in% biclust.names){
		#extra.arg <- paste("x=",method_result,sep="")
		extra.arg <- paste(",method_result='",method_result,"',type.method='biclust'",sep="")
		#isa2 <- FALSE
		biclust.combine <- TRUE
		make.save.button <- TRUE
		save.type <- "biclust"
	}
	if(methodname %in% fabia.names){
		#extra.arg <- paste("x=",method_result,sep="")
		extra.arg <- paste(",method_result='",method_result,"',type.method='fabia'",sep="")
		#isa2 <- FALSE
		biclust.combine <- FALSE
		make.save.button <- TRUE
		save.type <- "fabia"
	}
	if(methodname %in% isa.names){
		#extra.arg <- paste("x=isa.biclust",method_result,")",sep="")
		extra.arg <- paste(",method_result='",method_result,"',type.method='isa'",sep="")
		#isa2 <- TRUE
		biclust.combine <- FALSE
		make.save.button <- TRUE
		save.type <- "isa"
	}
	
	if(methodname %in% bicare.names){
		extra.arg <- paste0(",method_result='",method_result,"',type.method='bicare'")
		biclust.combine <- FALSE
		make.save.button <- TRUE
		save.type <- "bicare"
	}
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "SuperBiclust"
	
	toolhelp <- "superbiclust" 
	

	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	
	#### 			  ####
	## EXTRA DATA INPUT ##
	#####			 #####
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "biclustcombine"  
	argument.names <- c("") 
	argument.types <- c("num")
	arguments <- c("extra.biclust")
	initial.values <- c("NULL")
	title <- "Vector of Biclust Objects (Example: c('biclust2','biclust3') )"
	border <- FALSE
	entry.width <- c("50")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	
	####						  ####
	##	SUPERBICLUST CONFIGURATION	##
	####						  ####
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "indexradio"
	argument.names <- c("Jaccard","Sorensen","Ochiai","Kulczynski","Sensitivity","Specificity")  
	arguments <- c("index")		
	argument.values <- c("jaccard","sorensen","ochiai","kulczynski","sensitivity","specificity") 
	argument.types <- "char"
	initial.values <- "jaccard" 
	title <- "Similarity?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "typeradio"
	argument.names <- c("Rows","Columns","Both")  
	arguments <- c("type")		
	argument.values <- c("rows","cols","both") 
	argument.types <- "char"
	initial.values <- "both" 
	title <- "Type?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "superbiclust"  
	button.name <- "SuperBiclust"  
	button.function <- "superbiclust.GUI" 
	button.data <- "" 
	button.biclust <-  "x"
	button.otherarg <- extra.arg
	save <- FALSE
	show <- FALSE
	
	if(biclust.combine==TRUE){
		arg.frames <- c("indexradio","typeradio","biclustcombine") 
	}
	else{
		arg.frames <- c("indexradio","typeradio") 
	}
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####		####
	## DENDOGRAM  ##
	####		####
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "drawtree"  
	button.name <- "Draw Tree"  
	button.function <- "plot" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "x=superbiclust.tree"
	save <- FALSE
	show <- TRUE
	arg.frames <- c() 
	

	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	

	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "cutentry"  
	argument.names <- c("Number of Biclusters","Height") 
	argument.types <- c("num","num")
	arguments <- c("k","h")
	initial.values <- c("NULL","NULL")
	title <- "Where to cut tree? (number overrides height)"
	border <- FALSE
	entry.width <- c("6","6")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "cutree"  
	button.name <- "Cut Tree"  
	button.function <- "cutree" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "tree=superbiclust.tree"
	save <- TRUE
	show <- TRUE
	arg.frames <- c("cutentry") 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####						####
	## Plotting Robust Biclusters ##
	####						####
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "showrobust"
	argument.names <- c("Show Inside Robust BC?") 
	arguments <- c("show.which") 
	initial.values <- c(0) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "robustbutton"  
	button.name <- "Robust BC"  
	button.function <- "superbiclust.robust.GUI" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "CutTree=CutTree"
	save <- FALSE
	show <- TRUE
	arg.frames <- c("showrobust") 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "plottyperadio"
	argument.names <- c("Within Biclusters","All Samples")  
	arguments <- c("type")		
	argument.values <- c("within","all") 
	argument.types <- "char"
	initial.values <- "within" 
	title <- "Type?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "whichrobust"  
	argument.names <- c("Number") 
	argument.types <- c("num")
	arguments <- c("which.robust")
	initial.values <- c("")
	title <- "Which Robust BC?"
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "superplot"  
	button.name <- "Draw Plot"  
	button.function <- "plotSuper.GUI" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "CutTree=CutTree"
	save <- FALSE
	show <- FALSE
	arg.frames <- c("plottyperadio","whichrobust") 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	###			 ####
	## SAVE BUTTON ##
	####		 ####
	
	# ONLY FOR BICLUST

	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #

	type <- "buttons"

	# Change variables accordingly:
	frame.name <- "savebutton"  
	button.name <- "Save"  
	button.function <- "biclust.robust.fuse" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- paste("CutTree=CutTree,superbiclust.result=superbiclust.result",",method_result='",method_result,"'",",type='",save.type,"'",sep="")
	save <- FALSE
	show <- TRUE
	arg.frames <- c() 


	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)

	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "resetbutton"  
	button.name <- "Reset"  
	button.function <- "robust.reset" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <-  paste("method_result='",method_result,"'",sep="")
	save <- FALSE
	show <- FALSE
	arg.frames <- c() 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	### CONFIGURING GRID ###
	
	
	if(biclust.combine==TRUE){
		if(make.save.button==TRUE){
			grid.config <- .grid.matrix(input=input,c("biclustcombine",NA,NA,"indexradio","typeradio","superbiclust","drawtree",NA,NA,"cutentry","cutree",NA,"showrobust","robustbutton",NA,"plottyperadio","whichrobust","superplot","savebutton","resetbutton",NA),byrow=TRUE,nrow=7,ncol=3,grid.config=grid.config)
		}
		else{
			grid.config <- .grid.matrix(input=input,c("biclustcombine",NA,NA,"indexradio","typeradio","superbiclust","drawtree",NA,NA,"cutentry","cutree",NA,"showrobust","robustbutton",NA,"plottyperadio","whichrobust","superplot"),byrow=TRUE,nrow=6,ncol=3,grid.config=grid.config)
			
		}
	}
	else{
		if(make.save.button==TRUE){
			grid.config <- .grid.matrix(input=input,c("indexradio","typeradio","superbiclust","drawtree",NA,NA,"cutentry","cutree",NA,"showrobust","robustbutton",NA,"plottyperadio","whichrobust","superplot","savebutton","resetbutton",NA),byrow=TRUE,nrow=6,ncol=3,grid.config=grid.config)
			
		}
		else{
			grid.config <- .grid.matrix(input=input,c("indexradio","typeradio","superbiclust","drawtree",NA,NA,"cutentry","cutree",NA,"showrobust","robustbutton",NA,"plottyperadio","whichrobust","superplot"),byrow=TRUE,nrow=5,ncol=3,grid.config=grid.config)
		}
	}
	

	
	
	### COMBINING ROWS ###
	
	if(biclust.combine==TRUE){
		
		grid.rows <- .combine.rows(input=input,rows=c(1),title="Extra Biclust Data Input",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(2),title="Superbiclust Configuration",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(3,4),title="Dendogram",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(5,6),title="Robust Bicluster Gene Profiles",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		if(make.save.button==TRUE){
				grid.rows <- .combine.rows(input=input,rows=c(7),title="Save the Robust Biclusters (Biclust & BcDiag Only)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
			}
		
	}
	else{
		grid.rows <- .combine.rows(input=input,rows=c(1),title="Superbiclust Configuration",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(2,3),title="Superbiclust Configuration",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Robust Bicluster Gene Profiles",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		if(make.save.button==TRUE){
			grid.rows <- .combine.rows(input=input,rows=c(6),title="Save the Robust Biclusters (Biclust & BcDiag Only)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
			
		}
	}
	
	
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
