PlaidWin <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-Plaid"))
  
  ########################## 
  ## Specification frame ###
  ########################## 

  specFrame <- tkframe(top, borderwidth=5)
  radioButtons(specFrame , name = "tocluster", buttons = c("r", "c", "b"), values = c("r", "c", "b"), 
        labels = gettextRcmdr(c("row", "coloumns", "both")), title = gettextRcmdr("To Cluster:"))

  radioButtons(specFrame , name = "model", buttons = c("Linear", "Other"), values = c("Linear", "Other"), 
       labels = gettextRcmdr(c("Linear", "Other")), title = gettextRcmdr("Model:"))

  checkBoxes(specFrame , "backgroundFrame", boxes=c("background"), initialValues=1, labels=gettextRcmdr("Background:"))


  shuffleFrame <- tkframe(specFrame )
  shuffleVal <- tclVar("3")
  shuffleField <- ttkentry(shuffleFrame , width = "2",textvariable = shuffleVal )

  iterFrame <- tkframe(specFrame )
  iterStartupVal <- tclVar("5")
  iterField <- ttkentry(iterFrame , width = "2",textvariable = iterStartupVal)

  iter.layerFrame <- tkframe(specFrame )
  iter.layerVal <- tclVar("10")
  iter.layerField <- ttkentry(iter.layerFrame, width = "2",textvariable = iter.layerVal)

  backfitFrame <- tkframe(specFrame )
  backfitVal <- tclVar("0")
  backfitField <- ttkentry(backfitFrame , width = "2",textvariable = backfitVal )

  maxlayersFrame <- tkframe(specFrame)
  maxlayersVal <- tclVar("20")
  maxlayersField <- ttkentry(maxlayersFrame , width = "2",textvariable = maxlayersVal )


  onOK <- function() {
    tocluster <- as.character(tclvalue(toclusterVariable))
    #closeDialog()
    .activeDataSet <- ActiveDataSet()
    background <- if (tclvalue(backgroundVariable) == "1") ""
            else ", background =FALSE"

    shuffle <- if (tclvalue(shuffleVal ) == "") ""
            else paste(",shuffle=", tclvalue(shuffleVal), sep="" )

    iterStartup <- if (tclvalue(iterStartupVal ) == "") ""
            else paste(",iter.startup=", tclvalue(iterStartupVal ), sep="" )

    iter.layer <- if (tclvalue( iter.layerVal) == "") ""
            else paste(",iter.layer=", tclvalue( iter.layerVal), sep="" )

    backfit <- if (tclvalue(backfitVal ) == "") ""
            else paste(",back.fit=", tclvalue(backfitVal ), sep="" )

    maxlayers <- if (tclvalue(maxlayersVal ) == "") ""
            else paste(",max.layers=", tclvalue(maxlayersVal ), sep="" )

    doItAndPrint(paste("Plaidbics <- biclust(as.matrix(",.activeDataSet, "),method=BCPlaid(),cluster='",tocluster , "'",background,
           shuffle, iterStartup,iter.layer,maxlayers  ,backfit   ,")" , sep="") )

    tkfocus(CommanderWindow())

    }

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Plaid spesifications:")),  sticky="w")

   tkgrid(toclusterFrame, modelFrame,sticky = "nw")
   tkgrid(backgroundFrame, sticky="w")

   tkgrid(labelRcmdr(shuffleFrame , text=gettextRcmdr("Shuffle :            ")), shuffleField , sticky="w")
   tkgrid(labelRcmdr(iterFrame, text=gettextRcmdr("Iteration startup:")), iterField , sticky="w")

   tkgrid(shuffleFrame, iterFrame,sticky="w")  

   tkgrid(labelRcmdr(iter.layerFrame, text=gettextRcmdr("Iteration layer :  ")), iter.layerField, sticky="w")
   tkgrid(labelRcmdr(backfitFrame , text=gettextRcmdr("Back Fit :           ")), backfitField, sticky="w")

   tkgrid(backfitFrame ,iter.layerFrame, sticky="w")  

   tkgrid(labelRcmdr(maxlayersFrame , text=gettextRcmdr("Max Layers :      ")), maxlayersField , sticky="w")
   tkgrid(maxlayersFrame , sticky="w")  

   tkgrid(specFrame, sticky = "w")


## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("Plaidbics", "BCPlaid" )
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

