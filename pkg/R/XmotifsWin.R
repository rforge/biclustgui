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
  nsField <- ttkentry(nsFrame , width = "5",textvariable = nsVal )

  ndFrame <- tkframe(specFrame )
  ndVal <- tclVar("10")
  ndField <- ttkentry(ndFrame , width = "5",textvariable = ndVal )

  sdFrame <- tkframe(specFrame )
  sdVal <- tclVar("5")
  sdField <- ttkentry(sdFrame , width = "5",textvariable = sdVal )

  alphaFrame <- tkframe(specFrame )
  alphaVal <- tclVar("0.05")
  alphaField <- ttkentry(alphaFrame , width = "5",textvariable = alphaVal )

  discFrame <- tkframe(specFrame )
  discVal <- tclVar("FALSE")
  discField <- ttkentry(discFrame, width = "5",textvariable = discVal)

  numberFrame <- tkframe(specFrame )
  numberVal <- tclVar("10")
  numberField <- ttkentry(numberFrame, width = "5",textvariable = numberVal)

  onOK <- function() {

    .activeDataSet <- ActiveDataSet()

    ns <-  paste(tclvalue(nsVal ))
    nd <-  paste(tclvalue(ndVal ))
    sd <-  paste(tclvalue(sdVal ))
    alpha <-  paste(tclvalue(alphaVal ))
    number <- paste(tclvalue(numberVal))
    disc <- if(tclvalue(discVal)=="FALSE") FALSE
    else TRUE

    if(!disc)
    {
        doItAndPrint("## Discretizing the data matrix ##")
        doItAndPrint(paste("x <- discretize(as.matrix(",.activeDataSet, "))",sep="" ))

        doItAndPrint("## The Xmotif biclustering: ##")
        doItAndPrint(paste("Xmotifsbics <- biclust(x,method=BCXmotifs(), ns=",  ns , ",nd=",nd, ",sd=", sd,  ", alpha= ", alpha, ", number= ",number ,")" , sep="") )

    }
    else
    {
        doItAndPrint("## The Xmotif biclustering: ##")
        doItAndPrint(paste("Xmotifsbics <- biclust(as.matrix(",.activeDataSet, "), method=BCXmotifs(), ns=",  ns , ",nd=",nd, ",sd=", sd,  ", alpha= ", alpha, ", number= ",number ,")" , sep=""))
    }

    doItAndPrint("Xmotifsbics ")

    assign("Xmotifsbics", Xmotifsbics, envir=.GlobalEnv)

    tkfocus(CommanderWindow())

    }

   buttonsFrame <- tkframe(specFrame , borderwidth = 5)

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Xmotifs Specification:")),  sticky="w")

   tkgrid(labelRcmdr(nsFrame , text=gettextRcmdr("Number of rows choosen:")), nsField , sticky="w")
   tkgrid(labelRcmdr(ndFrame, text=gettextRcmdr("Number of repetitions:")), ndField , sticky="w")
   tkgrid(labelRcmdr(sdFrame , text=gettextRcmdr("Sample size in repetitions:")), sdField , sticky="w")
   tkgrid(labelRcmdr(alphaFrame, text=gettextRcmdr("Scaling factor:          ")), alphaField , sticky="w")

   tkgrid(nsFrame , ndFrame,sticky="w")

   tkgrid(sdFrame , alphaFrame ,sticky="w")

   tkgrid(labelRcmdr(numberFrame, text=gettextRcmdr("Number of biclusters:       ")), numberField, sticky="w")
   tkgrid(labelRcmdr(discFrame, text=gettextRcmdr("Data already discrete: ")), discField, sticky="w")

   tkgrid(numberFrame, discFrame, sticky="w")
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

   onHelpXmotif <- function() {
                tkgrab.release(window)
                 print(help("BCXmotifs"))
                }

   helpButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Help"), 
        foreground = "red", width = "12", command = onHelpXmotif , borderwidth = 3)



   tkgrid(OKbutton,Plotbutton ,exitButton ,helpButton , sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}


###XmotifsWin ()

