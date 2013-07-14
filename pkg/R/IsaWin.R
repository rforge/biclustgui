

IsaWin <- function () {
  require(isa2)

  initializeDialog(title = gettextRcmdr("Biclustering-ISA"))
  
  ########################## 
  ## Specification frame ###
  ########################## 

  specFrame <- tkframe(top, borderwidth=5)
  specFrame1 <- tkframe(specFrame )
  specFrame2 <- tkframe(specFrame )
  treshFrame <- tkframe(top, borderwidth=5)
  specFrame3 <- tkframe(treshFrame )

  checkBoxes(specFrame1 , "uprowFrame",   boxes=c("uprow"), initialValues=1,   labels=gettextRcmdr("up:    "))
  checkBoxes(specFrame1 , "downrowFrame", boxes=c("downrow"), initialValues=1, labels=gettextRcmdr("down:"))
  checkBoxes(specFrame2 , "upcolFrame",  boxes=c("upcol"), initialValues=1,    labels=gettextRcmdr("up:    "))
  checkBoxes(specFrame2 , "downcolFrame", boxes=c("downcol"), initialValues=1, labels=gettextRcmdr("down:"))

  thrFrame <- tkframe(specFrame3 )
  thrrowVal <- tclVar("seq(1,3,by=0.5)")
  thrrowField <- ttkentry(thrFrame , width = "30",textvariable = thrrowVal )

  thrcolVal <- tclVar("seq(1,3,by=0.5)")
  thrcolField <- ttkentry(thrFrame , width = "30",textvariable = thrcolVal )

  seedFrame <- tkframe(specFrame3 )
  seedVal <- tclVar("100")
  seedField <- ttkentry(seedFrame , width = "4",textvariable = seedVal )


  onOK <- function() {
    .activeDataSet <- ActiveDataSet()
   
     #row1 <<- ifelse(tclvalue(uprowVariable) == "1", 1, 0)
     #row2 <<- ifelse(tclvalue(downrowVariable) == "1", 1, 0)

     idctr1 <- 1+(as.numeric(tclvalue(uprowVariable))+  2*as.numeric(tclvalue(downrowVariable)))
     idctr2 <- 1+(as.numeric(tclvalue(upcolVariable))+  2*as.numeric(tclvalue(downcolVariable)))

    switch(idctr1 , "1"= {dirRow="updown"},"2"={dirRow="up"} , "3"={dirRow="down"}, "4"={dirRow="updown"}           )
    switch(idctr2 , "1"= {dirCol="updown"},"2"={dirCol="up"} , "3"={dirCol="down"}, "4"={dirCol="updown"}           )


     no.seeds<- paste("no.seeds=", tclvalue(seedVal ), sep="" )

     throw <- paste("thr.row=", tclvalue(thrrowVal ), sep="" )

     thcol  <- paste("thr.col=", tclvalue(thrcolVal ), sep="" )

    doItAndPrint(paste("isa.result <- isa(as.matrix(",.activeDataSet, "), direction=c('" , dirRow, "','", dirCol,"')," , no.seeds,"," ,
    throw, ",", thcol , ")" , sep="") )

    doItAndPrint(paste("ISAbics <- isa.biclust(isa.result)"))
    assign("ISAbics", ISAbics, envir=.GlobalEnv)
  
    doItAndPrint(paste("ISAbics "))
    tkfocus(CommanderWindow())

    }

   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("Isa spesifications:")),  sticky="w")
   tkgrid(labelRcmdr(specFrame , text=gettextRcmdr("            ")),  sticky="w")

   tkgrid(labelRcmdr(specFrame1 , text=gettextRcmdr("Direction row")),  sticky="w")
   tkgrid(uprowFrame, sticky="w")
   tkgrid(downrowFrame, sticky="w")

   tkgrid(labelRcmdr(specFrame2 , text=gettextRcmdr("Direction column")),  sticky="w")
   tkgrid(upcolFrame, sticky="w")
   tkgrid(downcolFrame, sticky="w")

   tkgrid(labelRcmdr(thrFrame , text=gettextRcmdr("Threshold row   :         ")), thrrowField , sticky="w")
   tkgrid(labelRcmdr(thrFrame , text=gettextRcmdr("Threshold column:         ")), thrcolField , sticky="w")

   tkgrid(thrFrame ,sticky="w")  

   tkgrid(labelRcmdr(seedFrame     , text=gettextRcmdr("Seed number:         ")), seedField , sticky="w")
   tkgrid(seedFrame     ,sticky="w")  

   tkgrid(specFrame, sticky = "w")
   tkgrid(specFrame1,specFrame2, sticky = "w")
   tkgrid(treshFrame , sticky = "w")

   tkgrid(specFrame3, sticky = "w")
## Button Frame ###

   buttonsFrame <- tkframe(top , borderwidth = 5)
    
   OKbutton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Show Result"), 
        foreground = "darkgreen", width = "12", command = onOK, 
        default = "active", borderwidth = 3)


   onPlot <- function() {
            PlotWin ("ISAbics", "BC ISA" )
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

   onHelpIsa <- function() {
                tkgrab.release(window)
                 print(help("isa"))
                }

   helpButton <- buttonRcmdr(buttonsFrame, text = gettextRcmdr("Help"), 
        foreground = "red", width = "12", command = onHelpIsa , borderwidth = 3)


   tkgrid(OKbutton,Plotbutton ,exitButton , helpButton ,sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}


##IsaWin ()
