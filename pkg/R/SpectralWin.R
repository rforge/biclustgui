SpectralWin <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-Spectral"))
  
  ########################## 
  ## Specification frame ###
  ########################## 

  specFrame <- tkframe(top, borderwidth=5)
  radioButtons(specFrame , name = "normalize", buttons = c("log", "irrc", "bistochastization"), values = c("log", "irrc", "bistochastization"), 
        labels = gettextRcmdr(c("log", "irrc", "bistochastization")), title = gettextRcmdr("Normalization:"))

  eigenFrame <- tkframe(specFrame )
  eigenVal <- tclVar("3")
  eigenField <- ttkentry(eigenFrame , width = "2",textvariable = eigenVal )

  minrFrame <- tkframe(specFrame )
  minrVal <- tclVar("2")
  minrField <- ttkentry(minrFrame , width = "2",textvariable = minrVal )

  mincFrame <- tkframe(specFrame )
  mincVal <- tclVar("2")
  mincField <- ttkentry(mincFrame , width = "2",textvariable = mincVal )

  withinVarFrame <- tkframe(specFrame)
  withinVarVal <- tclVar("1")
  withinVarField <- ttkentry(withinVarFrame , width = "2",textvariable = withinVarVal )


  onOK <- function() {
    normalize <- as.character(tclvalue(normalizeVariable))
    .activeDataSet <- ActiveDataSet()
   
     eigen<- paste(",numberOfEigenvalues=", tclvalue(eigenVal ), sep="" )

    minr <- paste(",minr =", tclvalue(minrVal ), sep="" )
    minc <- paste(",minc =", tclvalue(mincVal ), sep="" )
    withinVar <- paste(",withinVar =", tclvalue(withinVarVal ), sep="" )

    doItAndPrint(paste("Spectralbics <- biclust(as.matrix(",.activeDataSet, "),method=BCSpectral(),normalization='",normalize , "'",
           minr , minc ,withinVar ,")" , sep="") )
    doItAndPrint(paste("Spectralbics"))
    tkfocus(CommanderWindow())

    }

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Spectral spesifications:")),  sticky="w")

   tkgrid(normalizeFrame,sticky = "nw")
   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("")),  sticky="w")

   tkgrid(labelRcmdr(eigenFrame     , text=gettextRcmdr("Number of eigen values:         ")), eigenField , sticky="w")
   tkgrid(labelRcmdr(minrFrame      , text=gettextRcmdr("Minimum number of rows:      ")), minrField , sticky="w")
   tkgrid(labelRcmdr(mincFrame      , text=gettextRcmdr("Minimum number of columns: ")), mincField , sticky="w")
   tkgrid(labelRcmdr(withinVarFrame , text=gettextRcmdr("maximum within variation:      ")), withinVarField , sticky="w")

   tkgrid(eigenFrame     ,sticky="w")  
   tkgrid(minrFrame      ,sticky="w")  
   tkgrid(mincFrame      ,sticky="w")  
   tkgrid(withinVarFrame ,sticky="w")  


   tkgrid(specFrame, sticky = "w")


## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("Spectralbics ", "BCSpectral" )
    }


   Plotbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Plots"), 
        foreground = "darkgreen", width = "12", command = onPlot, 
        default = "active", borderwidth = 3)

    onCancel <- function() {
            if (GrabFocus()) 
            tkgrab.release(top)
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }

   exitButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Exit"), 
        foreground = "red", width = "12", command = onCancel, borderwidth = 3)

   onHelpSpec <- function() {
                tkgrab.release(window)
                 print(help("BCSpectral"))
                }

   helpButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Help"), 
        foreground = "red", width = "12", command = onHelpSpec, borderwidth = 3)


   tkgrid(OKbutton,Plotbutton ,exitButton , helpButton , sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}


### SpectralWin ()
