BimaxWin <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-Bimax"))

  ##########################
  ## Specification frame ###
  ##########################

  specFrame <- tkframe(top, borderwidth=5)
  MinRFrame <- tkframe(specFrame )
  MinRVal <- tclVar("2")
  MinRField <- ttkentry(MinRFrame , width = "2",textvariable = MinRVal )

  MinCFrame <- tkframe(specFrame )
  MinCVal <- tclVar("2")
  MinCField <- ttkentry(MinCFrame , width = "2",textvariable = MinCVal)

  numberFrame <- tkframe(specFrame )
  numberVal <- tclVar("100")
  numberField <- ttkentry(numberFrame, width = "3",textvariable = numberVal)

  onOK <- function() {
    .activeDataSet <- ActiveDataSet()
    MinR <-  paste(tclvalue(MinRVal))
    MinC <-  paste(tclvalue(MinCVal))
    number <- paste(tclvalue(numberVal))


    doItAndPrint(paste("Bimaxbics <- biclust(as.matrix(",.activeDataSet, "),method=BCBimax(), minr=", MinR, ", minc= ", MinC, ", number= ",number ,")" , sep="") )
    doItAndPrint("Bimaxbics") 
    tkfocus(CommanderWindow())

    }

   buttonsFrame <- tkframe(specFrame , borderwidth = 5)


   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Bimax Spezification:")),  sticky="w")

   tkgrid(labelRcmdr(MinRFrame , text=gettextRcmdr("Minimum Rows:")), MinRField , sticky="w")
   tkgrid(labelRcmdr(MinCFrame, text=gettextRcmdr("Minimum Columns:")), MinCField , sticky="w")

   tkgrid(MinRFrame, MinCFrame,sticky="w")


   tkgrid(labelRcmdr(numberFrame, text=gettextRcmdr("Maximal Numbers :  ")), numberField, sticky="w")
   tkgrid(numberFrame,sticky="w")
   tkgrid(specFrame, sticky = "w")


## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("Bimaxbics", "BCBimax" )
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

   tkgrid(OKbutton,Plotbutton ,exitButton , sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}

