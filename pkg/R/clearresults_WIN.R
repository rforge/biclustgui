# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################

#tkmessageBox(title="Error Message", message="Homogeneity parameter has to be in interval [0,1]!",icon="warning",type="ok")

clearresults_WINDOW <- function(){
	
	## USE TKMESSAGEBOX
	ReturnVal <- tkmessageBox(title="Clear all Bicluster Results",message = "Are you sure?",icon = "warning", type = "yesno", default = "no")
	if(tclvalue(ReturnVal)=="yes"){
		rm(list=.makeResultList(),envir=.GlobalEnv)
		rm(list="biclustering.objects",envir=.GlobalEnv)
	}
	
	
	## DO THE WARNING BOX MANUALLY
	
#	initializeDialog(title = gettextRcmdr("Clear all Bicluster Results...")) 
#	
#	onOK <- function(){
#		rm(list=.makeResultList(),envir=.GlobalEnv)
#		rm(list="biclustering.objects",envir=.GlobalEnv)
#		
#		if (GrabFocus()) 
#			tkgrab.release(top)
#		tkdestroy(top)
#		tkfocus(CommanderWindow())
#		
#	}
#	
#	onCancel <- function() {
#		if (GrabFocus()) 
#			tkgrab.release(top)
#		tkdestroy(top)
#		tkfocus(CommanderWindow())
#	}
#	
#	
#	
#	qFrame <- tkframe(top)
#	buttonFrame <- tkframe(top)
#	
#	tkgrid(labelRcmdr(qFrame,text=gettextRcmdr("Are you sure?")),sticky="s",padx="25",pady="12")
#	tkgrid(qFrame,sticky="s",padx="30")
#	
#	yesButton <- buttonRcmdr(buttonFrame,command=onOK,text=gettextRcmdr("Yes"),foreground="darkgreen",default="active",width="12",borderwidth=3)
#	noButton <- buttonRcmdr(buttonFrame,command=onCancel,text=gettextRcmdr("No"),foreground="darkgreen",default="active",width="12",borderwidth=3)
#	
#	tkgrid(yesButton,noButton,sticky="swe")
#	tkgrid(buttonFrame,sticky="swe")
#	
#	tkgrid.columnconfigure(buttonFrame, 0, weight=1)
#	tkgrid.columnconfigure(buttonFrame, 1, weight=1)
#	tkgrid.configure(yesButton,sticky="w")
#	tkgrid.configure(noButton,sticky="e")
#
#	
#	dialogSuffix(onOK=onOK)
}
