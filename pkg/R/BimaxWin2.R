BimaxWin2 <-
function () {
  require(biclust)
  initializeDialog(title = gettextRcmdr("Biclustering-Bimax"))

  ##########################
  ## Specification frame ###
  ##########################

  specFrame <- tkframe(top, borderwidth=5)
  MinRFrame <- tkframe(specFrame)
  MinRVal <- tclVar("2")
  MinRField <- ttkentry(MinRFrame , width = "2",textvariable = MinRVal )

  MinCFrame <- tkframe(specFrame)
  MinCVal <- tclVar("2")
  MinCField <- ttkentry(MinCFrame , width = "2",textvariable = MinCVal)

  numberFrame <- tkframe(specFrame)
  numberVal <- tclVar("100")
  numberField <- ttkentry(numberFrame, width = "3",textvariable = numberVal)



  backfitFrame <- tkframe(specFrame)
  backfitVal <- tclVar("3")
  backfitField <- ttkentry(backfitFrame , width = "2",textvariable = backfitVal )

  checkBoxes(specFrame ,frame="maxBimax", boxes=c("maxBi"),
         initialValues=0, labels=gettextRcmdr(c("Find Maximal Size?")) )


  n2Frame <- tkframe(specFrame )
  n2Val <- tclVar("30")
  n2Field <- ttkentry(n2Frame , width = "3",textvariable = n2Val)

  checkBoxes(specFrame ,frame="binarize", boxes=c("binarize"),
         initialValues=1, labels=gettextRcmdr(c("Binarize Data")) )

   thFrame <- tkframe(specFrame )
   thVal <- tclVar("NA")
   thField <- ttkentry(thFrame , width = "3",textvariable = thVal )

  onOK <- function() {
    .activeDataSet <- ActiveDataSet()
    MinR <-  paste(tclvalue(MinRVal))
    MinC <-  paste(tclvalue(MinCVal))
    number <- paste(tclvalue(numberVal))
    thres <- paste(tclvalue(thVal))
    backfit <- paste(tclvalue(backfitVal))
    n2 <- paste(tclvalue(n2Val))

    if(tclvalue(binarizeVariable) == "1")
    {
        doItAndPrint(paste("x <- binarize(as.matrix(",.activeDataSet, "),threshold= ",thres ,")" , sep="") )
    }
    else
    {
        doItAndPrint(paste("x <- as.matrix(",.activeDataSet, ")" , sep="") )
    }


    if(tclvalue(maxBiVariable) == "1")
    {
        doItAndPrint(paste("Bimaxbics <- biclust:::maxbimaxbiclust(x, minr=", MinR, ", minc= ", MinC, ", backfit= ",backfit, ", n2= ",n2, ", number= ",number, ")" , sep="") )
    }
    else
    {
        doItAndPrint(paste("Bimaxbics <- biclust(x,method=BCBimax(), minr=", MinR, ", minc= ", MinC, ", number= ",number ,")" , sep="") )
    }

    assign("Bimaxbics", Bimaxbics , envir=.GlobalEnv)

    doItAndPrint("Bimaxbics")
    tkfocus(CommanderWindow())
}

   buttonsFrame <- tkframe(specFrame , borderwidth = 5)


   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Binarize Data:")),  sticky="w")

   tkgrid(labelRcmdr(thFrame, text=gettextRcmdr("Threshold:")), thField , sticky="w")

   tkgrid(binarize, thFrame,sticky="w")

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Bimax Spezification:")),  sticky="w")

   tkgrid(labelRcmdr(MinRFrame , text=gettextRcmdr("Minimum Rows:")), MinRField , sticky="w")
   tkgrid(labelRcmdr(MinCFrame, text=gettextRcmdr("Minimum Columns:")), MinCField , sticky="w")

   tkgrid(MinRFrame, MinCFrame,sticky="w")
   tkgrid(labelRcmdr(numberFrame, text=gettextRcmdr("Maximal Numbers :  ")), numberField, sticky="w")
   tkgrid(numberFrame,sticky="w")

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("maxBimax Spezification:")),  sticky="w")
   tkgrid(maxBimax,sticky="w")

   tkgrid(labelRcmdr(backfitFrame , text=gettextRcmdr("Backfit:")), backfitField , sticky="w")
   tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("Repititions:")), n2Field , sticky="w")
   tkgrid(backfitFrame, n2Frame,sticky="w")

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

  onHelpBimax <- function() {
                tkgrab.release(window)
                 print(help("BCBimax"))
                }

   helpButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Help"), 
        foreground = "red", width = "12", command = onHelpBimax , borderwidth = 3)



   tkgrid(OKbutton,Plotbutton ,exitButton ,helpButton , sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}

###BimaxWin2 ()
