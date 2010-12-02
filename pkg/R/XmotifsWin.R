XmotifsWin <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-Xmotifs"))

  ##########################
  ## Specification frame ###
  ##########################

  specFrame <- tkframe(top, borderwidth=5)

  nsFrame <- tkframe(specFrame )
  nsVal <- tclVar("10")
  nsField <- ttkentry(nsFrame , width = "2",textvariable = nsVal )

  ndFrame <- tkframe(specFrame )
  ndVal <- tclVar("10")
  ndField <- ttkentry(ndFrame , width = "2",textvariable = ndVal )

  sdFrame <- tkframe(specFrame )
  sdVal <- tclVar("5")
  sdField <- ttkentry(sdFrame , width = "2",textvariable = sdVal )

  alphaFrame <- tkframe(specFrame )
  alphaVal <- tclVar("0.05")
  alphaField <- ttkentry(alphaFrame , width = "2",textvariable = alphaVal )

  numberFrame <- tkframe(specFrame )
  numberVal <- tclVar("10")
  numberField <- ttkentry(numberFrame, width = "3",textvariable = numberVal)

  onOK <- function() {

    .activeDataSet <- ActiveDataSet()

    ns <-  paste(tclvalue(nsVal ))
    nd <-  paste(tclvalue(ndVal ))
    sd <-  paste(tclvalue(sdVal ))
    alpha <-  paste(tclvalue(alphaVal ))
    number <- paste(tclvalue(numberVal))

     doItAndPrint("## Discretizing the data matrix ##")
     doItAndPrint(paste("x <- discretize(as.matrix(",.activeDataSet, "))",sep="" ))
     doItAndPrint("## The Xmotif biclustering: ##")
     doItAndPrint(paste("Xmotifsbics <- biclust(x,method=BCXmotifs(), ns=",  ns , ",nd=",nd, ",sd=", sd,  ", alpha= ", alpha, 
      ", number= ",number ,")" , sep="") )
    
    doItAndPrint("Xmotifsbics ") 


    tkfocus(CommanderWindow())

    }

   buttonsFrame <- tkframe(specFrame , borderwidth = 5)

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Xmotifs Specification:")),  sticky="w")

   tkgrid(labelRcmdr(nsFrame , text=gettextRcmdr("Number of rows choosen:")), nsField , sticky="w")
   tkgrid(labelRcmdr(ndFrame, text=gettextRcmdr("Number of repetitions:")), ndField , sticky="w")
   tkgrid(labelRcmdr(sdFrame , text=gettextRcmdr("Sample size in repetitions:")), sdField , sticky="w")
   tkgrid(labelRcmdr(alphaFrame, text=gettextRcmdr("Scaling factor:")), alphaField , sticky="w")

   tkgrid(nsFrame , ndFrame,sticky="w")

   tkgrid(sdFrame , sdFrame ,sticky="w")

   tkgrid(labelRcmdr(numberFrame, text=gettextRcmdr("Number of biclusters:  ")), numberField, sticky="w")
   tkgrid(numberFrame,sticky="w")
   tkgrid(specFrame, sticky = "w")


## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("Xmotifsbics", "Xmotifs" )
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

