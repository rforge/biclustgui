# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################

# NOTE: Note that newtool can ALSO be used to make new dialogs bound a button in the plotdiag Frame !!!

newtool_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "A new tool"

	toolhelp <- "testool"

	
	# Do not change this line:
	input <- "plotdiagTab"

	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	
	### CONFIGURING GRID ###
	
	
	### COMBINING ROWS ###
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
		
	
}
