

IsaWin <- function () {
  require(isa2)

  initializeDialog(title = gettextRcmdr("Biclustering-ISA"))
  
  ########################## 
  ## Specification frame ###
  ########################## 

  specFrame <- tkframe(top, borderwidth=5)
  specFrame1 <- tkframe(specFrame )
  specFrame2 <- tkframe(specFrame )
  specFrame3 <- tkframe(specFrame )


  checkBoxes(specFrame1 , "uprowFrame",   boxes=c("uprow"), initialValues=1,   labels=gettextRcmdr("up:    "))
  checkBoxes(specFrame1 , "downrowFrame", boxes=c("downrow"), initialValues=1, labels=gettextRcmdr("down:"))
  checkBoxes(specFrame2 , "upcolFrame",  boxes=c("upcol"), initialValues=1,    labels=gettextRcmdr("up:    "))
  checkBoxes(specFrame2 , "downcolFrame", boxes=c("downcol"), initialValues=1, labels=gettextRcmdr("down:"))

  seedFrame <- tkframe(specFrame3 )
  seedVal <- tclVar("100")
  seedField <- ttkentry(seedFrame , width = "4",textvariable = seedVal )


  onOK <- function() {
    .activeDataSet <- ActiveDataSet()
   
     #row1 <<- ifelse(tclvalue(uprowVariable) == "1", 1, 0)
     #row2 <<- ifelse(tclvalue(downrowVariable) == "1", 1, 0)

     idctr1 <- 1+(as.numeric(tclvalue(uprowVariable))+  2*as.numeric(tclvalue(downrowVariable)))
     idctr2 <- 1+(as.numeric(tclvalue(upcolVariable))+  2*as.numeric(tclvalue(downcolVariable)))

    switch(idctr1 , "0"= {dirRow="updown"},"1"={dirRow="up"} , "2"={dirRow="down"}, "3"={dirRow="updown"}           )
    switch(idctr2 , "0"= {dirCol="updown"},"1"={dirCol="up"} , "2"={dirCol="down"}, "3"={dirCol="updown"}           )


     no.seeds<- paste("no.seeds=", tclvalue(seedVal ), sep="" )

    doItAndPrint(paste("isa.result <- isa(as.matrix(",.activeDataSet, "), direction=c('" , dirRow, "','", dirCol,"')," , no.seeds ,")" , sep="") )

    doItAndPrint(paste("ISAbics <- isa.biclust(isa.result)"))
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

   tkgrid(labelRcmdr(seedFrame     , text=gettextRcmdr("Seed number:         ")), seedField , sticky="w")
   tkgrid(seedFrame     ,sticky="w")  
   tkgrid(specFrame, sticky = "w")
   tkgrid(specFrame1,specFrame2, sticky = "w")
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

   tkgrid(OKbutton,Plotbutton ,exitButton , sticky = "w")
   tkgrid(buttonsFrame, sticky = "w")
   dialogSuffix(rows = 2, columns = 2)
}


#ISAWin ()
