# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################

# NOTE: Note that newtool can ALSO be used to make new dialogs bound a button in the plotdiag Frame !!!

bcdiagwrite_WINDOW <- function(methodname,fabia.thresZ=0.5,fabia.thresL=NULL){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)	
	
	# Special Case names
	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
	
	## Special Case: Fabia  (Because of the need for thresholds)
	if(methodname %in% fabia.names){
		eval(parse(text=paste("method_class <- class(",method_result,")",sep=""))) # Can only do it here and not earlier, cause not sure of the object exists at this point
		if(method_class == "Factorization" ){
			#bcdiag.fabia <- TRUE
			extra.arg <- paste0(method_result,",mname='fabia',fabia.thresZ=",fabia.thresZ,",fabia.thresL=",fabia.thresL)
			
		}
		else{ # This handles the case if superbiclust has been applied to Fabia object
			#bcdiag.fabia <- FALSE
			result_temp <- .tobiclust_transf(method_result)
			extra.arg <- paste0(result_temp,",mname='biclust'")
		}
	}
	## General Case
	else{
		#bcdiag.fabia <- FALSE
		result_temp <- .tobiclust_transf(method_result)
		extra.arg <- paste0(result_temp,",mname='biclust'")
	}
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "BCDIAG Output"
	
	toolhelp <- "writeBic"
	
	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entryframe"  
	argument.names <- c("Title","Delimiter (def.=' ')") 
	argument.types <- c("char","char")
	arguments <- c("bicname","delimiter")
	initial.values <- c("Output Result"," ")
	title <- ""
	border <- FALSE
	entry.width <- c("15","15")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "appendframe"
	argument.names <- c("Append?") 
	arguments <- c("append") 
	initial.values <- c(1) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
#	####	    	MANUAL BUTTONS FRAME 			  ####
#	#                               					 #
#	
#	type <- "buttons"
#	
#	# Change variables accordingly:
#	frame.name <- "button_setwd"  
#	button.name <- "Set Work. Dir."  
#	button.function <- "Setwd" 
#	button.data <- "" 
#	button.biclust <-  "" 
#	button.otherarg <- "x=TRUE"
#	save <- FALSE
#	show <- FALSE
#	arg.frames <- c() 
#	
#	# Do not change this line:
#	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_write"  
	button.name <- "Write"  
	button.function <- "writeBic.GUI" 
	button.data <- "dset" 
	button.biclust <-  "" 
	button.otherarg <- paste0(",bicResult=",extra.arg)
	save <- FALSE
	arg.frames <- c("entryframe","appendframe") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	### CONFIGURING GRID ###
	grid.config <- .grid.matrix(input=input,c("entryframe","appendframe","button_write",NA),byrow=TRUE,nrow=2,ncol=2,grid.config=grid.config)
	
	
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="Writing a Summary Output to text-file:",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
