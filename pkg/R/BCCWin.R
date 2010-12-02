BCCWin <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-CC"))

  ##########################
  ## Specification frame ###
  ##########################

  specFrame <- tkframe(top, borderwidth=5)
  deltaFrame <- tkframe(specFrame )
  deltaVal <- tclVar(" ")
  deltaField <- ttkentry(deltaFrame , width = "2",textvariable = deltaVal )

  alphaFrame <- tkframe(specFrame )
  alphaVal <- tclVar("2")
  alphaField <- ttkentry(alphaFrame , width = "2",textvariable = alphaVal )

  numberFrame <- tkframe(specFrame )
  numberVal <- tclVar("100")
  numberField <- ttkentry(numberFrame, width = "3",textvariable = numberVal)

  onOK <- function() {
    .activeDataSet <- ActiveDataSet()

    delta <-  paste(tclvalue(deltaVal ))

    alpha <-  paste(tclvalue(alphaVal ))

    number <- if (tclvalue( numberVal) == "") "100"
            else paste(tclvalue(numberVal))


    doItAndPrint(paste("CCbics <- biclust(as.matrix(",.activeDataSet, "),method=BCCC(), delta=", delta, ", alpha= ", alpha, 
      ", number= ",number ,")" , sep="") )
    
    doItAndPrint("CCbics ") 
    
    tkfocus(CommanderWindow())

    }

   buttonsFrame <- tkframe(specFrame , borderwidth = 5)


   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("CC Specification:")),  sticky="w")

   tkgrid(labelRcmdr(deltaFrame , text=gettextRcmdr("Delta:")), deltaField , sticky="w")
   tkgrid(labelRcmdr(alphaFrame, text=gettextRcmdr("Alpha:")), alphaField , sticky="w")

   tkgrid(deltaFrame , alphaFrame,sticky="w")


   tkgrid(labelRcmdr(numberFrame, text=gettextRcmdr("Maximal Numbers :  ")), numberField, sticky="w")
   tkgrid(numberFrame,sticky="w")
   tkgrid(specFrame, sticky = "w")


## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("CCbics", "BCCC" )
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

