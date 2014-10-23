# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


newmethod_WINDOW <- function(){     # Change newmethod to your own method name
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	# List of frame objects. add.frame will return new.frames with an additional frame
	# Possible improvement: Instead of overwriting new.frames each time. 
	#      Maybe save the variable to something global or in a separate environment.
	
	
		
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
	#####################################################
	
	# Note that the idea is that each new dialog coincides with 1 clustering function on which 
	# multiple plot or diagnostic functions can be used (or even general ones).
	
	# compatibility (with superclust & bcdiag)
	# clusterfunction, plotfunctions, diagnosticfunctions
	# name
	
	
	# Define the name of the method as it will appear in the top of the window:
	# Note: no special characters allowed in title (only "-" is allowed)
	methodname <- "A new method"

	# Define the function (as it is named in your package)
	# Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
	methodfunction <- "testfunc"
	testfunc <- function(d,arg1,arg2,arg3){return(runif(1,1,1000)+d[1,1]+arg1+arg2+arg3)}
	
	# Define the name of the data argument for your function
	data.arg <- "d"

	# Transform the data from data.arg
	data.transf <- "matrix" # Values: "matrix" (default), "ExprSet"
	
#	# Data will be loaded in as a dataframe, should it be transformed to a matrix for your function?
#	data.matrix <- FALSE
#	# Or should the data be transformed into a ExpressionSet object ("Biobase" package)
#	data.ExprSet <- FALSE
	
	# Define if you want the object in which the results are shown to be printed
	methodshow <- TRUE
	
	# Define if the result should be saved. (ADVANCED USERS ONLY!)
	methodsave <- TRUE  # FALSE will imply methodshow=FALSE
	
	# Define any other arguments in the function, which should not be changed by the user.
	# These arguments may also include a certain method for your function, since it is the idea to give each method a separate window.
	other.arg <- ",method='BCPlaid',arg='default'"  # Comma in the beginning but not at the end ! 
	
	other.arg <- ""

	# Which object should be searched for concerning the help button?
	methodhelp <- "testfunc" # For most functions this will probably be the function itself. There are however exceptions like biclust(...,method=BCPlaid(),..) in which the search object should be "BCPlaid")

	###
	## Some methods use binarized or discretized data, this can be achieved with some functions inside of `biclust`:
	
	# If this is set to TRUE an extra box will be made avaible to discretize your data with the "discretize" function of the 'biclust' package (note: this function needs data in a matrix)
	# Note that if you want to use this functionality, data.matrix should be put to TRUE
	data.discr <- FALSE
	
	# Make box available to binarize the data
	data.bin <- FALSE
	
	
	# Possibility to give a seed ?
	methodseed <- TRUE
	
	## COMPATIBILITY? ##
	
	# BcDiag
	bcdiag.comp <- FALSE
	
	# SuperBiclust
	superbiclust.comp <- FALSE
	
	
	# Only for biclust:
	extrabiclustplot <- FALSE
	
	# Only for ISA2:
	extrabiclustforisa <- FALSE
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	###############################################################################################################################################################################
	## ADDING OF ARGUMENT FRAMES ##
	###############################
	
	# Below you can add argument frames. Each frame is 1 type of arguments and there can be multiple of each.
	# Further down below each frame can be ordered in a grid.
	
	# Types of argument:
	#	- check boxes  (= "checkboxes")
	#	- radio buttons (= "radiobuttons")
	# 	- list boxes	(="listboxes")   # ONLY FOR ENSEMBLE METHODS
	#	- entry fields	(="entryfields")
	#	- entry fields depending on a checkbox (="checkentryfields")   # NOTE: MAYBE KEEP THIS OUT: SHOW IN GUIDELINE HOW TO MAKE IT WITH EXISTING TYPES ! (e.g. row1->checkbox  ; row2 -> options ; combine row 1 & 2 in frame)
	# 	- sliders (="sliders")
	#	- spinbox (="spinboxes")
	#	- ...
	
	# Type of options:
	#	- number of options for an argument
	#	- frametitle
	#	- border
	# 	Note: Combination of frametitle and border yes or not leads to different results
	#	Note2: If you want a border (with our without title), around multiple rows of frames.
	#		   Don't define it in each frame, use the options in the grid configuration.
	
	# Defaults for each type (with the options) can be found below. 
	# If you need more of a type, you can copy it multiple times (with a different name).
	# The ones you do not need, delete these from this R-script.
	
	########################
	#### CLUSTERING TAB ####
	########################
	
	input <- "clusterTab"
	
	######		  ENTRY FIELDS FRAME - EXAMPLE 				#####
	#							    		 				    #
	# Note on entry fields: They will all be placed below each other. 
	# If you want multiple columns, you will need to add multiple frames.	
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entryframe1"  
	argument.names <- c("Argument 1","Argument 2","Argument 3") # Argument names which to appear in the dialog
	argument.types <- c("num","num","char") # Define if the argument is numeric (='num') or a string/character (='char'). The last one might be helpful for plot labels. Note that the " are not necessary in the entry box. Suggested to keep char and num entries separated (to have nicer entry widths)
						# "num" and "char" basically means if there should be a ' ' around the value of the argument. For example for a formula argument, you would still choose "num"
	arguments <- c("arg1","arg2","arg3") # Arguments as used in your package function
	initial.values <- c(1,2,"a")
	title <- "A Title"
	border <- FALSE
	entry.width <- c("2","2","6")  # Width of the entry box ("2" = width for 2 numbers/characters). Will affect all entry widths in this frame
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	# TEST LINES: DELETE LATER !!	
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=c(1,2,3),title=title,border=border,entry.width=entry.width,argument.types=c("num","num","num")  ,new.frames=new.frames)

	new.frames <- .add.frame(input=input,type=type,frame.name="entryframe2",argument.types=c("num","num","num"),argument.names=argument.names,arguments=c("arg4","arg5","arg6"),initial.values=c(1,2,3),title="",border=FALSE,entry.width=entry.width  ,new.frames=new.frames)
	new.frames <- .add.frame(input=input,type=type,frame.name="entryframe3",argument.types=c("num","num","num"),argument.names=argument.names,arguments=arguments,initial.values=initial.values,title="More entries!",border=TRUE,entry.width=entry.width  ,new.frames=new.frames)
	
	new.frames <- .add.frame(input=input,type=type,frame.name="entryframe4",argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width  ,new.frames=new.frames)
	
	
	#                               							 #
	##############################################################
	
	
	####		RADIO BUTTONS FRAME - EXAMPLE 				####
	#                               						   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radioframe1"
	argument.names <- c("Button 1","Button 2","Button 3")   # The button names as they should appear in the window
	arguments <- c("buttonarg")		# The name of the argument these radiobuttons correspond with 
	argument.types <- "char"  # Decides if the values should have a ' ' around them in the function. It applies to all buttons !
	argument.values <- c("b1","b2","b3") # the possible values the argument can take (same order as argument.names !)
	# Note: Due to the button creation, values may NOT start with a number !
	# Note: SOLUTION TO THE ABOVE !: "0"  -> "NUMBERSTART0" # NOTE: This is not even necessary anymore !
	initial.values <- "b3" #Should only be 1 value !
	title <- "Button Options"
	border <- TRUE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	#															##
	##############################################################
	# TESTING LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name="radioframe2",argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title="More buttons!",border=FALSE,new.frames=new.frames)	
	
	
	
	
	
	####		CHECK BOXES FRAME - EXAMPLE 				  ####
	#                               							 #
			
	# Note on checkboxes: They will all be placed below each other. 
	# If you want multiple columns, you will need to add multiple frames.
	# Checkboxes are only used for TRUE/FALSE arguments.

	type <- "checkboxes"

	# Change variables accordingly:
	frame.name <-  "checkboxframe1"
	argument.names <- c("Check 1","Check 2","Check 3")  # The checkbox names as they should appear in the window
	arguments <- c("checkarg1","checkarg2","checkarg3") # The name of the arguments the checkboxes correspond with
	initial.values <- c(0,1,1)  # 0 -> FALSE ; 1 -> TRUE
	title <- "title"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
		
	# TEST LINES:
	new.frames <- .add.frame(input=input,type=type,frame.name="checkboxframe2",argument.names=argument.names,arguments=c("a1","a2","a3"),initial.values=initial.values,title="",border=TRUE,new.frames=new.frames)
	new.frames <- .add.frame(input=input,type=type,frame.name="checkboxframe3",argument.names=argument.names,arguments=c("aa1","aa2","aa3"),initial.values=initial.values,title="Checkboxes wooo:",border=TRUE,new.frames=new.frames)
	new.frames <- .add.frame(input=input,type=type,frame.name="checkboxframe4",argument.names=argument.names,arguments=arguments,initial.values=initial.values,title="Even More???",border=FALSE,new.frames=new.frames)
	
	#															##
	##############################################################
	
	
	####		VALUE SLIDER FRAME - EXAMPLE 				  ####
	#                               							 #
	# Only for numerical values
	type <- "valuesliders"

	# Change variables accordingly:
	frame.name <- "sliderframe1"
	argument.names <- c("Slider 1  ","Slider 2  ","Slider 3  ") # The name of the arguments as they appear in the window
	arguments <- c("sliderarg1","sliderarg2","sliderarg3") # The actual argumentname which is used in the methodfunction.
	initial.values <- c(1,5,10)
	from <- c(1,1,1) # Starting points of the sliders
	to <- c(5,50,500) # Ending points of the sliders
	by <- c(1,10,50)  # Determines how much each movement of the slider will change the number
	length <- c(50,100,150) # Length of the slider
	title <- "Title"
	border <- TRUE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,title=title,border=border,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,length=length,new.frames=new.frames)

	
	# TEST LINE:
	new.frames <- .add.frame(input=input,type=type,title="",border=FALSE,frame.name="sliderframe2",argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,length=length,new.frames=new.frames)

	#															##
	##############################################################
	
	####	    	SPIN BOX FRAME - EXAMPLE 				  ####
	#                               							 #
	# Note: Only for numerical values

	
	
	type <- "spinboxes"

	# Change variables accordingly:
	frame.name <- "spinboxframe1"
	argument.names <- c("Spin Box 1: ","Spin Box 2: ","Spin Box 3: ") # Names of the arguments as they will appear in the window
	arguments <- c("spinarg1","spingarg2","spingarg3") # Names of the actual arguments in the function
	initial.values <- c(5,10,20)
	from <- c(1,5,10)  # Starting point of spinbox
	to <- c(10,20,30)	# Ending points of spinbox
	by <- c(1,1,1)
	entry.width <- "2"  # Width of spinbox (applies to all of them in this frame)
	title <- "Spin Box !"
	border <- TRUE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,entry.width=entry.width,title=title,border=border,new.frames=new.frames)
	
	# TEST LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name="spinboxframe2",argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,entry.width=entry.width,title="",border=FALSE,new.frames=new.frames)

	#															##
	##############################################################
	

	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
	##################################################
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	# Put the frame names you have defined above in a matrix. If the rows are unequal of length, fill them up with NA's. 
	# (Note: the NA's should always be on the right, not the left. 

	# - The place of the frame is determined by a sticky variable down below.
	# - The place of the frame will always be north-west.
	
	# EXAMPLES:
	# 1.
	grid.config <- .grid.matrix(input=input,c("entryframe1","entryframe2"),nrow=1,ncol=2,grid.config=grid.config)
	grid.config
	#      [,1]          [,2]         
	# [1,] "entryframe1" "entryframe2"
	
	# 2.
	grid.config <- .grid.matrix(input=input,c("entryframe1","entryframe2"),nrow=2,ncol=1,grid.config=grid.config)
	grid.config
	#      [,1]         
	# [1,] "entryframe1"
	# [2,] "entryframe2"

	# 3.
	grid.config <-.grid.matrix(input=input,c("entryframe1","entryframe3","entryframe2",NA),nrow=2,ncol=2,grid.config=grid.config)
	grid.config
	#      [,1]          [,2]         
	# [1,] "entryframe1" "entryframe2"
	# [2,] "entryframe3" NA        

	# 4. 
	grid.config <- .grid.matrix(input=input,c("entryframe1",NA,"entryframe2","entryframe3","entryframe4",NA),nrow=3,ncol=2,byrow=TRUE,grid.config=grid.config)
	grid.config
	#      [,1]          [,2]         
	# [1,] "entryframe1" NA           
	# [2,] "entryframe2" "entryframe3"
	# [3,] "entryframe4" NA           

	# 5. 
	grid.config <- .grid.matrix(input=input,c("entryframe1","entryframe2","entryframe3","entryframe4"),nrow=1,ncol=4,byrow=TRUE,grid.config=grid.config)
 	grid.config
	#      [,1]          [,2]          [,3]          [,4]         
	# [1,] "entryframe1" "entryframe2" "entryframe3" "entryframe4"

	# 6.

	grid.config <- .grid.matrix(input=input,c("entryframe1",NA,"entryframe2","entryframe3"),nrow=2,ncol=2,byrow=TRUE,grid.config=grid.config)
	grid.config
	#      [,1]          [,2]         
	# [1,] "entryframe1" NA           
	# [2,] "entryframe2" "entryframe3"

	# 7. 
	grid.config <- .grid.matrix(input=input,c("radioframe1"),nrow=1,ncol=1,grid.config=grid.config)
	grid.config
	
		
	# 8. 
	grid.config <- .grid.matrix(input=input,c("entryframe1"),nrow=1,ncol=1,grid.config=grid.config)
	grid.config
	
	
	# 9. 
	grid.config <- .grid.matrix(input=input,c("entryframe1","radioframe1","entryframe2","entryframe3",NA,NA,"radioframe2",NA,NA),ncol=3,nrow=3,byrow=TRUE,grid.config=grid.config)
   	grid.config
	#      [,1]          [,2]          [,3]         
	# [1,] "entryframe1" "radioframe1" "entryframe2"
	# [2,] "entryframe3" NA            NA           
	# [3,] "radioframe2" NA            NA    

	# 10.
	grid.config <- .grid.matrix(input=input,c("entryframe1","entryframe2","radioframe1",NA),nrow=2,ncol=2,byrow=TRUE,grid.config=grid.config)
	grid.config
	#      [,1]          [,2]         
	# [1,] "entryframe1" "entryframe2"
	# [2,] "radioframe1" NA           

	# 11.
	grid.config <- .grid.matrix(input=input,c("checkboxframe1"),grid.config=grid.config)

	# 12.
	grid.config <- .grid.matrix(input=input,c("checkboxframe1",NA,"checkboxframe2","checkboxframe3","checkboxframe4",NA),nrow=3,ncol=2,byrow=TRUE,grid.config=grid.config)
	grid.config
	#      [,1]             [,2]            
	# [1,] "checkboxframe1" NA              
	# [2,] "checkboxframe2" "checkboxframe3"
	# [3,] "checkboxframe4" NA              

	# 13.
	grid.config <- .grid.matrix(input=input,c("entryframe1","radioframe1","entryframe2","checkboxframe1",NA,NA,"radioframe2","checkboxframe2",NA),nrow=3,ncol=3,byrow=TRUE,grid.config=grid.config)
	grid.config
	#      [,1]             [,2]             [,3]         
	# [1,] "entryframe1"    "radiofram1"     "entryframe2"
	# [2,] "checkboxframe1" NA               NA           
	# [3,] "radioframe2"    "checkboxframe2" NA    

	# 14.
	grid.config <- .grid.matrix(input=input,c("sliderframe1"),grid.config=grid.config)
	
	# 15.
	grid.config <- .grid.matrix(input=input,c("spinboxframe1"),grid.config=grid.config)
	
	# 16
	grid.config <- .grid.matrix(input=input,c("entryframe1","radioframe1","spinboxframe1","checkboxframe1","checkboxframe2",NA,NA,NA,"spinboxframe2","sliderframe1",NA,NA,"radioframe2","sliderframe2",NA,NA,"entryframe2","checkboxframe3","entryframe3",NA),nrow=5,ncol=4,byrow=TRUE,grid.config=grid.config)
	grid.config
	#      [,1]             [,2]             [,3]            [,4]            
	# [1,] "entryframe1"    "radioframe1"    "spinboxframe1" "checkboxframe1"
	# [2,] "checkboxframe2" NA               NA              NA              
	# [3,] "spinboxframe2"  "sliderframe1"   NA              NA              
	# [4,] "radioframe2"    "sliderframe2"   NA              NA              
	# [5,] "entryframe2"    "checkboxframe2" "entryframe3"   NA              

	
	####################################
	#### COMBINING ROWS -CLUSTERTAB ####
	####################################
	
	# With this option you can combine row(s) in the sense of drawing a border (with or without title) around 1 or more rows of frames. 
	# Note: You can use the `.combine.row' function multiple times, but make sure you use each row only once.	
	# Note: Only bordering matrix rows can be combined!
	# Note: Why use a combine row without title or border? -> If some of your frames are jumping to the right due to the grid nature of Rcmdr/tcltk , just combine that 1 row without a title or border !

	# EXAMPLES ( on example 2/4 of grid.config) :

	# 1.	
	grid.rows <- .combine.rows(input=input,rows=c(3,4),title="A nice box: ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="",border=FALSE,grid.rows=grid.rows,grid.config=grid.config)
	#grid.rows <- .combine.rows(input=input,rows=c(4),title="",border=FALSE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(5),title="",border=FALSE,grid.rows=grid.rows,grid.config=grid.config)
	
	# 2.
	
	rows <- c(1,2)
	title <- "Everything: "
	border <- TRUE
	
	grid.rows <- .combine.rows(input=input,rows=rows,title=title,border=border,grid.rows=grid.rows,grid.config=grid.config)
	
	
	# NOTE: ALSO ADD POSSIBILITY TO DRAW BORDER AROUND A MULTIPLE OF FRAMES
	# (ONLY AROUND FRAMES NEXT TO EACH OTHER? OR NOT?)



	# IDEA: FIRST MAKE MATRIX, then ask for border around multiple rows or border.title around multiple rows
	

	# NOTE: LAST STEP: INCLUDE A MATRIX IN WHICH USERS CAN PROVIDE THE STICKY VARIABLE


	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	
	
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
	
	
	# Note: All the kind of frames above can be added in this tab aswell!
	#		-> Additionally buttons tied to functions can be added here too !
	
	# NOTE: - able to make custom buttons, each tied to a certain function (plot, diag)
	#		- Don't forget the errorcondition on the plotbutton (if you did not do show results yet)
	

	# NOTE: TO AVOID UNFORESEEN ERRORS: GIVE THESE FRAME DIFFERENT NAMES !!!


	type <- "entryfields"

	# Change variables accordingly:
	frame.name <- "entryframe10"  
	argument.names <- c("Argument 1","Argument 2","Argument 3") # Argument names which to appear in the dialog
	argument.types <- c("num","num","char") # Define if the argument is numeric (='num') or a string/character (='char'). The last one might be helpful for plot labels. Note that the " are not necessary in the entry box. Suggested to keep char and num entries separated (to have nicer entry widths)
	arguments <- c("arg1","arg2","arg3") # Arguments as used in your package function
	initial.values <- c(1,2,"a label")
	title <- "Test:"
	border <- FALSE
	entry.width <- c("2","2","2")  # Width of the entry box ("2" = width for 2 numbers/characters). Will affect all entry widths in this frame

	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)

	# TEST LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name="entryframe20",argument.names=argument.names,arguments=arguments,initial.values=c(1,2,3),title=title,border=border,entry.width=entry.width,argument.types= c("num","num","num")  ,new.frames=new.frames)

	
	####	    	MANUAL BUTTONS FRAME - EXAMPLE 				  ####
	#                               								 #

	# NOTE: Only for the plotTab !!
	# Note: Only 1 button per frame
	# Note: The button will automatically use the "Show result" object
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "buttonframe1"  
	button.name <- "Button 1"  # What should be ON the button, NO special characters
	button.function <- "buttonfunction" # The function
	button.data <- "d" # The name of the data argument (here the "show result" object will be filled in)
	button.biclust <-  "biclust" # The name of the argument in which the biclust object should be
	# Note: If any of the 2 above is empty, it will not be used
	arg.frames <- c("frame1","frame2") # due to possible similar arguments between button functions, give here the frames containing the arguments for this particular button
	
	arg.frames <- c()
	
	save <- TRUE # If this is set to true, the  result of the function will be saved in an object with the same name as the button (no spaces)
	show <- TRUE
	button.otherarg <- "" # NOT FOR PUBLIC USE ! ONLY WORKS WHEN NOT USING '' ( ALSO USING A , IN THE BEGINNING DEPENDS ON IF YOU ARE USING button.data OR button.biclust
	# otherarg DOES work if you do it like this: button.otherarg <- " method=\"default\" "
	# So for quotations on the inside, you need to use \"
	# NOTE: NOW WORKS WITH ' ' !
	button.width <- "12"
	button.data.transf <- "matrix" # Can be "matrix" or "ExprSet"
	
	#showprint <- FALSE # For own use only, will not print first function
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,button.width=button.width,save=save,show=show,new.frames=new.frames,button.data.transf=button.data.transf)

	###############################################################
	
	# TEST LINES
	testfunc2 <- function(d,arg1,arg2,arg3){return(d+arg1+arg2+arg3)}
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,arg.frames=c("entryframe20"),new.frames=new.frames)
	
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
	###################################################
	
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	grid.config <- .grid.matrix(input=input,c("entryframe10","entryframe20"),grid.config=grid.config)
	
	
	
	grid.config <- .grid.matrix(input=input,c("entryframe20","buttonframe1"),nrow=1,ncol=2,grid.config=grid.config)
	
	grid.config <- .grid.matrix(input=input,c("buttonframe1"),nrow=1,ncol=1,grid.config=grid.config)
	
	
	########################
	#### COMBINING ROWS ####
	########################
	
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Plot 1",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.transf=data.transf,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot,methodshow=methodshow,extrabiclustforisa=extrabiclustforisa,methodsave=methodsave)
	
}
