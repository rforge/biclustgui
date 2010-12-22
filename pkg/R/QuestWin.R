QuestWin <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-Quest"))

  ##########################
  ## Specification frame ###
  ##########################

  specFrame <- tkframe(top, borderwidth=10)

  dFrame <- tkframe(specFrame )
  dVal <- tclVar("0")
  dField <- ttkentry(dFrame , width = "4",textvariable = dVal )

  quantFrame <- tkframe(specFrame )
  quantVal <- tclVar("0")
  quantField <- ttkentry(quantFrame , width = "4",textvariable = quantVal )

  variFrame <- tkframe(specFrame )
  variVal <- tclVar("0")
  variField <- ttkentry(variFrame , width = "4",textvariable = variVal )



  nsFrame <- tkframe(specFrame )
  nsVal <- tclVar("10")
  nsField <- ttkentry(nsFrame , width = "4",textvariable = nsVal )

  ndFrame <- tkframe(specFrame )
  ndVal <- tclVar("10")
  ndField <- ttkentry(ndFrame , width = "4",textvariable = ndVal )

  sdFrame <- tkframe(specFrame )
  sdVal <- tclVar("5")
  sdField <- ttkentry(sdFrame , width = "4",textvariable = sdVal )

  alphaFrame <- tkframe(specFrame )
  alphaVal <- tclVar("0.05")
  alphaField <- ttkentry(alphaFrame , width = "4",textvariable = alphaVal )

  numberFrame <- tkframe(specFrame )
  numberVal <- tclVar("10")
  numberField <- ttkentry(numberFrame, width = "4",textvariable = numberVal)

  onOK <- function()
  {
    .activeDataSet <- ActiveDataSet()

    quant <-  paste(tclvalue(quantVal ))
    vari <-  paste(tclvalue(variVal ))
    d <-  paste(tclvalue(dVal ))
    ns <-  paste(tclvalue(nsVal ))
    nd <-  paste(tclvalue(ndVal ))
    sd <-  paste(tclvalue(sdVal ))
    alpha <-  paste(tclvalue(alphaVal ))
    number <- paste(tclvalue(numberVal))
    if(d==0 & quant==0)
    {
        doItAndPrint("## The Quest biclustering: ##")
        doItAndPrint(paste("Questbics <- biclust(as.matrix(",.activeDataSet, "), method=BCQuest(), ns=",  ns , ",nd=",nd, ",sd=", sd,  ", alpha= ", alpha, ", number= ",number ,")" , sep="") )
        doItAndPrint("Questbics ")
    }
    else
    {
        if(d>0)
        {
            doItAndPrint("## The Quest ordinal biclustering: ##")
            doItAndPrint(paste("Questbics <- biclust(as.matrix(",.activeDataSet, "), method=BCQuestord(), d=",  d, ", ns=",  ns , ",nd=",nd, ",sd=", sd,  ", alpha= ", alpha, ", number= ",number ,")" , sep="") )
            doItAndPrint("Questbics ")
        }
        else
        {
            doItAndPrint("## The Quest metric biclustering: ##")
            doItAndPrint(paste("Questbics <- biclust(as.matrix(",.activeDataSet, "), method=BCQuestmet(), quant=",  quant, ", vari=",  vari , ", ns=",  ns , ",nd=",nd, ",sd=", sd,  ", alpha= ", alpha, ", number= ",number ,")" , sep="") )
            doItAndPrint("Questbics ")
        }

    }



    tkfocus(CommanderWindow())


  }



   buttonsFrame <- tkframe(specFrame , borderwidth = 5)

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Quest Specification:")),  sticky="w")

   tkgrid(labelRcmdr(nsFrame , text=gettextRcmdr("Number of rows choosen: ")), nsField , sticky="w")
   tkgrid(labelRcmdr(ndFrame, text=gettextRcmdr("Number of repetitions:")), ndField , sticky="w")
   tkgrid(labelRcmdr(sdFrame , text=gettextRcmdr("Sample size in repetitions:")), sdField , sticky="w")
   tkgrid(labelRcmdr(alphaFrame, text=gettextRcmdr("Scaling factor:            ")), alphaField , sticky="w")

   tkgrid(nsFrame , ndFrame,sticky="w")

   tkgrid(sdFrame , alphaFrame ,sticky="w")

   tkgrid(labelRcmdr(numberFrame, text=gettextRcmdr("Number of biclusters:       ")), numberField, sticky="w")
   tkgrid(numberFrame,sticky="w")
   tkgrid(labelRcmdr(dFrame, text=gettextRcmdr("If using ordinal data specify interval: ")), dField, sticky="w")
   tkgrid(dFrame,sticky="w")
   tkgrid(labelRcmdr(quantFrame, text=gettextRcmdr("If using metric data specify quantile:  ")), quantField, sticky="w")
   tkgrid(quantFrame,sticky="w")
   tkgrid(labelRcmdr(variFrame, text=gettextRcmdr("If using metric data specify variance: ")), variField, sticky="w")
   tkgrid(variFrame,sticky="w")

    tkgrid(specFrame, sticky = "w")


## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)

   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"),
        foreground = "darkgreen", width = "12", command = onOK,
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("Questbics", "Quest" )
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

