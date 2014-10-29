# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


bcdiag_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	# Some extra code to determine the input type: "biclust", "fabia", "isa2"
	biclust.names <- c("Bimax","CC","Plaid","Questmotif","Spectral","XMotifs","IBBIG","Rqubic")
	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
	isa.names <- c("ISA")
	bicare.names <- c("BICARE")
	
	if(methodname %in% biclust.names){
		extra.arg <- ",mname='biclust'"
	}
	if(methodname %in% fabia.names){
		extra.arg <- ",mname='fabia'"

		# Exception when superbiclust has been used on fabia:
		method_result <- gsub(" ","",methodname,fixed=TRUE)
		method_result <- gsub("-","",method_result,fixed=TRUE)
		if(method_result %in% ls(envir=.GlobalEnv)){
			eval(parse(text=paste("method_class <- class(",method_result,")",sep="")))
			if(method_class=="Biclust"){
				extra.arg <- ",mname='biclust'"
			}
		}
	}
	if(methodname %in% isa.names){
		extra.arg <- ",mname='isa2'"
	}
	
	if(methodname %in% bicare.names){
		extra.arg <- ",mname='bicare'"
	}
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "BCDIAG"
	
	toolhelp <- "BcDiag-package" # TO BE FILLED IN WHEN PACKAGE AGAIN AVAILABLE ON CRAN
	
	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	####						 ####
	##	The anomedOnlybic Function ##
	####						 ####
	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio_anomed"
	argument.names <- c("Diagnostic Plots","Tukey Additivity Plot","Anova Plots","Mpolish Plots","Anova & Mpolish")  
	arguments <- c("fit")		
	argument.values <- c("aplot","mplot","anovbplot","mpolishbplot","boxplot") 
	argument.types <- "char"
	initial.values <- "boxplot" 
	title <- "Plot Type:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entry_number"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("bnum")
	initial.values <- c(1)
	title <- ""
	border <- FALSE
	entry.width <- c("3")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_anomed"  
	button.name <- "Draw Plot"  
	button.function <- "anomedOnlybic" 
	button.data <- "dset" 
	button.biclust <-  "bres" 
	button.otherarg <- extra.arg
	save <- FALSE
	arg.frames <- c("radio_anomed","entry_number") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	####													   ####
	##	    EXPLORATORY PLOTS (biclustered & Clustered Data)     ##
	####						 							   ####
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio1_explore"
	argument.names <- c("Mean","Median","Variance","Median Absolute Deviation","Quantile","All")  
	arguments <- c("pfor")		
	argument.values <- c("mean","median","variance","mad","quant","all") 
	argument.types <- "char"
	initial.values <- "mean" 
	title <- "Plot for:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio2_explore"
	argument.names <- c("Genes","Conditions")  
	arguments <- c("gby")		
	argument.values <- c("genes","conditions") 
	argument.types <- "char"
	initial.values <- "genes" 
	title <- "Dimension:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_explore"  
	button.name <- "Bicl. & Clust."  
	button.function <- "exploreBic" 
	button.data <- "dset" 
	button.biclust <-  "bres" 
	button.otherarg <- extra.arg
	save <- FALSE
	arg.frames <- c("radio1_explore","radio2_explore","entry_number") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button2_explore"  
	button.name <- "Only Biclust."  
	button.function <- "exploreOnlybic" 
	button.data <- "dset" 
	button.biclust <-  "bres" 
	button.otherarg <- extra.arg
	save <- FALSE
	arg.frames <- c("radio1_explore","radio2_explore","entry_number") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####					####
	##	    PROFILE PLOTS     ##
	####					####
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio1_profile"
	argument.names <- c("All","Lines","Boxplot","Histogram","3D")  
	arguments <- c("bplot")		
	argument.values <- c("all","lines","boxplot","histogram","threeD") 
	argument.types <- "char"
	initial.values <- "all" 
	title <- "Plot Type:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio2_profile"
	argument.names <- c("Genes","Conditions")  
	arguments <- c("gby")		
	argument.values <- c("genes","conditions") 
	argument.types <- "char"
	initial.values <- "genes" 
	title <- "Dimension:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	

	####		VALUE SLIDER FRAME - EXAMPLE 				  ####
	#                               							 #
	# Only for numerical values
	type <- "valuesliders"
	
	# Change variables accordingly:
	frame.name <- "slider_profile"
	argument.names <- c("Theta: ","Phi:     ") 
	arguments <- c("teta","ph")
	initial.values <- c(120,30)
	from <- c(-180,-180) 
	to <- c(180,180) 
	by <- c(10,10)  
	length <- c(125,125) 
	title <- "3D Rotation:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,title=title,border=border,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,length=length,new.frames=new.frames)
	
	
	
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_profile"  
	button.name <- "Draw Plot"  
	button.function <- "profileBic" 
	button.data <- "dset" 
	button.biclust <-  "bres" 
	button.otherarg <- extra.arg
	save <- FALSE
	arg.frames <- c("radio1_profile","radio2_profile","entry_number","slider_profile") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_summaryoutput"  
	button.name <- "Summary"  
	button.function <- "bcdiagwrite_WINDOW" 
	button.data <- "" 
	button.biclust <-  ""
	
	temp <- paste("methodname='",methodname,"'",sep="")
	button.otherarg <- temp
	save <- FALSE
	show <- FALSE
	arg.frames <- c() 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	
	### CONFIGURING GRID ###
	
	grid.config <- .grid.matrix(input=input,c("entry_number","button_summaryoutput",NA,"radio_anomed","button_anomed",NA,"radio1_explore","radio2_explore",NA,"button_explore","button2_explore",NA,"radio1_profile","radio2_profile","slider_profile","button_profile",NA,NA),byrow=TRUE,nrow=6,ncol=3,grid.config=grid.config)
	
	
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Plotting Cluster Number & Summary Output",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2),title="ANOVA & Median Polish Residual Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(3,4),title="Exploratory Plots for Biclustered & Clustered data",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(5,6),title="Profile Plots for Biclustered & Clustered data",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
