# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################



# A function to check if an object is a result of ANY of the biclustering methods in this GUI
.isbiclustGUIresult <- function(x){
	
	if(class(x)=="character"){
		eval(parse(text=paste("x <- ",x,sep="")))
	}
	
	if(class(x)=="iBBiG"){return(TRUE)}
	if(class(x)=="Biclust"){return(TRUE)}
	if(class(x)=="Factorization"){return(TRUE)}
	if(class(x)=="QUBICBiclusterSet"){return(TRUE)}
	if(class(x)=="biclustering"){return(TRUE)}
	if(.isISA(x)){return(TRUE)}
	return(FALSE)

}

# Some other classes are extensions of "Biclust" or simply the same. These are listed with this function.
.isBiclustresult <- function(x){
	if(class(x)=="character"){
		eval(parse(text=paste("x <- ",x,sep="")))
	}
	if(class(x)=="iBBiG"){return(TRUE)}
	if(class(x)=="Biclust"){return(TRUE)}
	if(class(x)=="QUBICBiclusterSet"){return(TRUE)}
	return(FALSE)
}


PLACEHOLDER <- function(){}

