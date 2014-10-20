# Project: search_test
# 
# Author: Ewoud
###############################################################################


# This function needs to be expanded as soon as more search criteria are getting added
SearchMethodData <- function(data,type){
	
	out <- data[data$type==type,1]
	
	return(out)
}




search_WINDOW <- function(){

	initializeDialog(title = gettextRcmdr("Search Methods... (WORK IN PROGRESS)")) # Change to Search Biclustering Methods...
	
	.makesearchdata()
	method_data <- biclustGUI_biclusteringsearchdata # Save the global variable in method_data
	
	onOK <- function(){
		tkdelete(methodBox, "0", "end")
		type <- tclvalue(radiotypeVariable)
		method.names <- SearchMethodData(method_data,type)
		for (name in method.names) tkinsert(methodBox, "end", name)
		global.variable.list<<- method.names
		
	}
	
	onWindow <- function(){
		if(!("global.variable.list" %in% ls(envir=.GlobalEnv))){justDoIt("warning('Search for methods first',call.=FALSE)")}
		else{
			sel1 <- as.integer(tkcurselection(methodBox))+1
			sel2 <- which(global.variable.list[sel1]==method_data$name)
			# Code in comment to be used when implementing it in package itself
			tkdestroy(top)
			eval(parse(text=paste(method_data$window[sel2])))
			#print(method_data$window[sel2])
		}
	}
	
	searchFrame <- tkframe(top)
	
	typeFrame <- ttklabelframe(searchFrame,text=gettextRcmdr("Bicluster Type"))
	
	radioButtons(typeFrame,name="radiotype",buttons=c("Constant","CoherentValues","CoherentEvolution"),values=c("Constant","Coherent Values","Coherent Evolution"),labels=gettextRcmdr(c("Constant Values","Coherent Values","Coherent Evolution")),initialValue="Constant",title="")
	tkgrid(radiotypeFrame)
	
	
	searchButton <- buttonRcmdr(searchFrame,command=onOK,text=gettextRcmdr("Search"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	
	resultFrame <- ttklabelframe(searchFrame,text=gettextRcmdr("Results:"))
	
	

	methodBox <- tklistbox(resultFrame, height=5, exportselection="FALSE",
			selectmode="single", background="white")
	methodScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(methodBox, ...))
	tkconfigure(methodBox, yscrollcommand=function(...) tkset(methodScroll, ...))
	
	
	openwindowButton <- buttonRcmdr(resultFrame,command=onWindow,text=gettextRcmdr("Go to"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	
	tkgrid(methodBox,methodScroll,openwindowButton) #,sticky="ns"
	tkgrid.configure(methodScroll,sticky="ns")
	
	tkgrid(typeFrame,searchButton,sticky="sw")
	tkgrid(resultFrame)
	tkgrid(searchFrame,sticky="nw")
	

	onCancel <- function(){}
	
	
	dialogSuffix(onOK=onOK)
	
	
}


#search_WINDOW()