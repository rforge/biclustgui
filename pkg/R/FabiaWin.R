
FabiaWin <- function () {
  #require(fabia)
  initializeDialog(title = gettextRcmdr("Biclustering-Fabia Laplace Prior"))
  
  ########################## 
  ## Specification frame ###
  ########################## 

  specFrame <- tkframe(top, borderwidth=5)
  radioButtons(specFrame , name = "center", buttons = c("c1", "c2", "c3", "c4"), values = c("1", "2", "3", "4"), initialValue=2,
        labels = gettextRcmdr(c("none", "Mean", "Median", "Mode")), title = gettextRcmdr("Data  Centering:"))

  radioButtons(specFrame , name = "norm", buttons = c("n", "q", "v"), values = c("0", "1", "2"), initialValue=2, 
        labels = gettextRcmdr(c("none", "0.75-0.25 quantiles", "Var=1")), title = gettextRcmdr("Data  Normalization:"))

  pFrame <- tkframe(specFrame )
  pVal <- tclVar("5")
  pField <- ttkentry(pFrame, width = "4",textvariable = pVal )

  alphaFrame <- tkframe(specFrame )
  alphaVal <- tclVar("0.1")
  alphaField <- ttkentry(alphaFrame , width = "4",textvariable = alphaVal)

  cycFrame <- tkframe(specFrame )
  cycVal <- tclVar("500")
  cycField <- ttkentry(cycFrame , width = "4",textvariable = cycVal )

  splFrame <- tkframe(specFrame )
  splVal <- tclVar("0.5")
  splField <- ttkentry(splFrame , width = "4",textvariable = splVal )

  spzFrame <- tkframe(specFrame )
  spzVal <- tclVar("0.5")
  spzField <- ttkentry(spzFrame , width = "4",textvariable = spzVal )

  randomFrame <- tkframe(specFrame)
  randomVal <- tclVar("1")
  randomField <- ttkentry(randomFrame , width = "4",textvariable = randomVal )

  scaleFrame <- tkframe(specFrame)
  scaleVal <- tclVar("0")
  scaleField <- ttkentry(scaleFrame , width = "4",textvariable = scaleVal )

  lapFrame <- tkframe(specFrame)
  lapVal <- tclVar("1")
  lapField <- ttkentry(lapFrame , width = "4",textvariable = lapVal )


   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Fabia spesifications:")),  sticky="w")
   tkgrid(labelRcmdr(specFrame, text=gettextRcmdr("")) )
   tkgrid(labelRcmdr(pFrame , text=gettextRcmdr("Number of biclusters :             ")), pField, sticky="w")
   tkgrid(labelRcmdr(alphaFrame , text=gettextRcmdr("   Sparseness loadings (0.1 - 1.0):        ")), alphaField, sticky="w")

   tkgrid(pFrame, alphaFrame ,sticky="w")  
   tkgrid(labelRcmdr(cycFrame , text=gettextRcmdr("Number of iterations:              ")), cycField , sticky="w")
   tkgrid(labelRcmdr(splFrame , text=gettextRcmdr("   Sparseness prior loadings (0.5 - 2.0):")), splField, sticky="w")

   tkgrid(cycFrame ,splFrame , sticky="w")  
   tkgrid(labelRcmdr(spzFrame , text=gettextRcmdr("Sparseness factors (0.5 - 2.0):")), spzField, sticky="w")
   tkgrid(labelRcmdr(randomFrame , text=gettextRcmdr("   Random initialization of loadings:       ")), randomField , sticky="w")

   tkgrid(spzFrame , randomFrame , sticky="w")  
   tkgrid(labelRcmdr(specFrame, text=gettextRcmdr("")) )
   tkgrid(normFrame, centerFrame,sticky = "nw")
   tkgrid(specFrame, sticky = "w")



  onOK <- function() {
    .activeDataSet <- ActiveDataSet()
    center <- as.character(tclvalue(centerVariable))
    norm <- as.character(tclvalue(normVariable))

    p     <- tclvalue(pVal )
    alpha <- tclvalue(alphaVal )
    cyc   <- tclvalue(cycVal)
    spl   <- tclvalue(splVal )
    spz   <- tclvalue(spzVal )
    random<- tclvalue(randomVal )
    scale <- tclvalue(scaleVal )
    lap   <- tclvalue(lapVal )

    doItAndPrint(paste("Bicfabia  <- fabia(as.matrix(",.activeDataSet,"), p=",p, ",alpha=", alpha, ",cyc=",cyc, ",spl=", spl,
    ",spz=", spz, ",random=",random, ",scale=", scale, ",lap=", lap, ",center=",center, ",norm=", norm  ,")", sep="") )

    doItAndPrint("summary(Bicfabia)") 
    tkgrab.release(top)
    tkfocus(CommanderWindow())

    }
  ## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotFabWin (bicObj="Bicfabia",method="Fabia")

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


onHelpfabia <-
function() {
                tkgrab.release(window)
                 print(help("fabia"))
                }

   exitButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Exit"), 
        foreground = "red", width = "12", command = onCancel, borderwidth = 3)


   helpButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Help"), 
        foreground = "red", width = "12", command = onHelpfabia , borderwidth = 3)


   tkgrid(OKbutton,Plotbutton ,exitButton ,helpButton , sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}


