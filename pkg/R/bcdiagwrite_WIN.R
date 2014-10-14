# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################

# NOTE: Note that newtool can ALSO be used to make new dialogs bound a button in the plotdiag Frame !!!

bcdiagwrite_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	# Some extra code to determine the input type: "biclust", "fabia", "isa2"
	biclust.names <- c("Bimax","CC","Plaid","Questmotif","Spectral","XMotifs","IBBIG")
	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
	isa.names <- c("ISA")
	
	if(methodname %in% biclust.names){
		extra.arg <- ",mname='biclust'"
	}
	if(methodname %in% fabia.names){
		extra.arg <- ",mname='fabia'"
	}
	if(methodname %in% isa.names){
		extra.arg <- ",mname='isa2'"
	}
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "BCDIAG Output"
	
	toolhelp <- "writeBic"
	
	data.matrix <- TRUE
	
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
	argument.names <- c("Filename","Title","Delimiter (def.=' ')") 
	argument.types <- c("char","char","char")
	arguments <- c("fileName","bicname","delimiter")
	initial.values <- c("output.txt","Output Result"," ")
	title <- ""
	border <- FALSE
	entry.width <- c("15","15","15")  
	
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
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_setwd"  
	button.name <- "Set Work. Dir."  
	button.function <- "Setwd" 
	button.data <- "" 
	button.biclust <-  "" 
	button.otherarg <- "x=TRUE"
	save <- FALSE
	show <- FALSE
	arg.frames <- c() 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_write"  
	button.name <- "Write"  
	button.function <- "writeBic" 
	button.data <- "dset" 
	button.biclust <-  "bicResult" 
	button.otherarg <- extra.arg
	save <- FALSE
	arg.frames <- c("entryframe","appendframe") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	### CONFIGURING GRID ###
	grid.config <- .grid.matrix(input=input,c("entryframe","appendframe","button_setwd","button_write"),byrow=TRUE,nrow=2,ncol=2,grid.config=grid.config)
	
	
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="Writing a Summary Output to text-file:",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,data.matrix=data.matrix,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
