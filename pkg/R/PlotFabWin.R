

########################## 
### Plots Main Window ####
########################## 


PlotFabWin <- function (bicObj,method) {

   initializeDialog(title = gettextRcmdr(paste("Fabia Plots", method, sep=" - ") ))
   Part0Frame <- tkframe(top,relief="groove", borderwidth=2)
   Part1Frame <- tkframe(top,relief="groove", borderwidth=2)
   Part2Frame <- tkframe(top,relief="groove", borderwidth=2)



   ########################### 
   ##### Show  Plot Frame ####
   ###########################
   showplotFrame <- tkframe(Part0Frame )
   showplotFrame2 <- tkframe(Part0Frame )
   showplotFrame3 <- tkframe(Part0Frame )

   radioButtons(showplotFrame , name = "show", buttons = c("c1","c2", "c3", "c4"),
     values = c("1", "2", "3", "4"), 
        labels = gettextRcmdr(c("Information content of biclusters", "Information content of samples", "Loadings per bicluster",
       "Factors per bicluster" )), title = gettextRcmdr("Summary Plot:"))

   #################### 
   ## Draw show plot ##
   #################### 

   onDRAW0 <- function() {
     .activeDataSet <- ActiveDataSet()
     show<- tclvalue(showVariable)

     doItAndPrint(paste("showSelected(",bicObj, ",which=", show ,")" , sep="") )
   }

   buttonsPlotFrame0 <- tkframe(showplotFrame2 , borderwidth = 5)
   
   Drawbutton <- buttonRcmdr(buttonsPlotFrame0 , text = gettextRcmdr("Draw Plot"), 
        foreground = "darkgreen", width = "12", command = onDRAW0 , 
        default = "active", borderwidth = 3)

   tkgrid(showFrame , sticky = "w")

   tkgrid(Drawbutton , sticky = "w")
   tkgrid(buttonsPlotFrame0 , sticky = "w")
   tkgrid(labelRcmdr(showplotFrame3, text=gettextRcmdr("                      ")),  sticky="w")

   tkgrid(showplotFrame , showplotFrame2 ,showplotFrame3,  sticky = "w")
   #tkgrid(showplotFrame2 , sticky = "w")


   ############################### 
   ##### Extract Plot  Frame ####
   ############################### 

   ExtractplotFrame <- tkframe(Part1Frame )
   ExtractplotFrame2 <- tkframe(Part1Frame)

   radioButtons(ExtractplotFrame , name = "extract", buttons = c("c0", "c1","c2", "c3", "c4","c5", "c6", "c7", "c8"),
     values = c("0","1", "2", "3", "4", "5", "6", "7", "8"), 
        labels = gettextRcmdr(c("All", "Noise free data*", "Data", "Reconstructed data", "Error", "Absolute factors", "Absolute loadings", 
         "Reconstructed matrix sorted", "Original matrix sorted"
         )), title = gettextRcmdr("Extract Plot:"))

   thresZFrame <- tkframe(ExtractplotFrame2 )
   thresZVal <- tclVar("0.5")
   thresZField <- ttkentry(thresZFrame , width = "3",textvariable = thresZVal )
   

   thresLFrame <- tkframe(ExtractplotFrame2 )
   thresLVal <- tclVar(" ")
   thresLField <- ttkentry(thresLFrame , width = "3",textvariable = thresLVal )


   YFrame <- tkframe(ExtractplotFrame2 )
   YVal <- tclVar(" ")
   YField <- ttkentry(YFrame , width = "10",textvariable = YVal )

   ###################### 
   ## Draw extract plot##
   ###################### 
   onDRAW <- function() {
     .activeDataSet <- ActiveDataSet()
     extract <- as.character(tclvalue(extractVariable))
          extract <- as.character(tclvalue(extractVariable))
     extract2   <- if (extract  == 0) ""
            else paste(",which=", extract  , sep="" )


     thresZ  <- tclvalue(thresZVal )

     thresL  <- if (tclvalue(thresLVal ) == "") ""
            else paste(",thresL =", tclvalue(thresLVal ), sep="" )


     Y  <- if (tclvalue(YVal ) == "") ""
            else paste(",Y=", tclvalue(YVal ), sep="" )

     doItAndPrint(paste("extplot <- extractPlot(",bicObj, ",thresZ=" ,thresZ ,thresL , Y  ,extract2,    ",ti='FABIA')" , sep="") )
   }

   buttonsPlotFrame <- tkframe(ExtractplotFrame2 , borderwidth = 5)
   
   Drawbutton <- buttonRcmdr(buttonsPlotFrame , text = gettextRcmdr("Draw Plot"), 
        foreground = "darkgreen", width = "12", command = onDRAW , 
        default = "active", borderwidth = 3)


   tkgrid(labelRcmdr(thresZFrame , text=gettextRcmdr("Threshold for sample:   ")), thresZField , sticky="w")
   tkgrid(labelRcmdr(thresLFrame , text=gettextRcmdr("Threshold for loadings:  ")), thresLField , sticky="w")
   tkgrid(labelRcmdr(YFrame , text=gettextRcmdr("Noise free data matrix:  ")), YField , sticky="w")
   tkgrid(Drawbutton ,  sticky = "w")


   tkgrid(extractFrame , sticky = "w")
   tkgrid(labelRcmdr(ExtractplotFrame2, text=gettextRcmdr("")),  sticky="w")

   tkgrid(thresZFrame ,sticky="w")  
   tkgrid(thresLFrame ,sticky="w")  
   tkgrid(YFrame ,sticky="w")  
   tkgrid(labelRcmdr(ExtractplotFrame2, text=gettextRcmdr("")),  sticky="w")

   tkgrid(buttonsPlotFrame ,sticky="w")  
   tkgrid(labelRcmdr(ExtractplotFrame2, text=gettextRcmdr("")),  sticky="w")
   tkgrid(labelRcmdr(ExtractplotFrame2, text=gettextRcmdr("* if available")),  sticky="w")
   tkgrid(labelRcmdr(ExtractplotFrame2, text=gettextRcmdr("")),  sticky="w")
   tkgrid(labelRcmdr(ExtractplotFrame2, text=gettextRcmdr("")),  sticky="w")

   tkgrid(ExtractplotFrame , ExtractplotFrame2, sticky = "w")





   ############################### 
   ##### Bicluster Plot  Frame ####
   ############################### 

   plotBicFrame <- tkframe(Part2Frame ,relief="groove")
   plotBicFrame2 <- tkframe(Part2Frame ,relief="groove")
   plotBicFrame3 <- tkframe(Part2Frame ,relief="groove")

   numClustFrame <- tkframe(plotBicFrame )
   numClustVal <- tclVar("1")
   numClustField <- ttkentry(numClustFrame , width = "3",textvariable = numClustVal )


   checkBoxes(plotBicFrame ,frame="plot1Frame", boxes=c("plot1"),
         initialValues=1, labels=gettextRcmdr(c("Data Matrix")) )
  
   checkBoxes(plotBicFrame ,frame="plot2Frame", boxes=c("plot2"),
         initialValues=1, labels=gettextRcmdr(c("Bicluster only")) )

   checkBoxes(plotBicFrame ,frame="oppFrame", boxes=c("opp"),
         initialValues=0, labels=gettextRcmdr(c("Opposite")) )


  thresZFrame2 <- tkframe(plotBicFrame2 )
   thresZVal2 <- tclVar("0.5")
   thresZField2 <- ttkentry(thresZFrame2 , width = "3",textvariable = thresZVal2 )
   
   thresLFrame2 <- tkframe(plotBicFrame2 )
   thresLVal2 <- tclVar(" ")
   thresLField2 <- ttkentry(thresLFrame2 , width = "3",textvariable = thresLVal2 )



 onDRAW2 <- function() {
     .activeDataSet <- ActiveDataSet()

    if ( tclvalue(plot1Variable) == "1" & tclvalue(plot2Variable) == "1") toplot <- ""
      else {
          if ( tclvalue(plot1Variable) == "1") toplot <- ",which=1"

          if ( tclvalue(plot2Variable) == "1") toplot <- ",which=2"
          }

     opp <- if ( tclvalue(oppVariable) == "1") ",opp=TRUE"
           else ",opp=FALSE"

     thresZ  <- tclvalue(thresZVal )

     thresL  <- if (tclvalue(thresLVal ) == "") ""
            else paste(",thresL =", tclvalue(thresLVal ), sep="" )

     clusterVal <- if (tclvalue(numClustVal ) == "") ""
            else paste(",p=", tclvalue(numClustVal), sep="" )


   doItAndPrint(paste("extfabia <-extractBic(",bicObj, ",thresZ=",thresZ, thresL, " )" , sep="") )
   doItAndPrint(paste("plotBicluster(extfabia", clusterVal , toplot,opp ," )" , sep="") )
      
     }

   buttonsbicFrame <- tkframe(plotBicFrame2 , borderwidth = 5)
   
   Drawbiclst <- buttonRcmdr(buttonsbicFrame , text = gettextRcmdr("Draw Plot"), 
        foreground = "darkgreen", width = "12", command = onDRAW2 , 
        default = "active", borderwidth = 3)

   tkgrid(labelRcmdr(plotBicFrame , text=gettextRcmdr("Bicluster Plot:")),  sticky="w")
   tkgrid(labelRcmdr(numClustFrame , text=gettextRcmdr("Cluster Number :")), numClustField , sticky="w")

   tkgrid(labelRcmdr(thresZFrame2 , text=gettextRcmdr("  Threshold for sample:    ")), thresZField2 , sticky="w")
   tkgrid(labelRcmdr(thresLFrame2 , text=gettextRcmdr("  Threshold for loadings:  ")), thresLField2 , sticky="w")
  

   tkgrid(Drawbiclst , sticky = "w")

   tkgrid(numClustFrame ,sticky="w")  
   tkgrid(plot1Frame ,sticky="w")  
   tkgrid(plot2Frame ,sticky="w")  
   tkgrid(oppFrame,sticky="w")  

   tkgrid(thresZFrame2 ,sticky="w")  
   tkgrid(thresLFrame2 ,sticky="w")  
   tkgrid(labelRcmdr(plotBicFrame2 , text=gettextRcmdr(" ")),  sticky="w")

   tkgrid(buttonsbicFrame ,sticky="w")  


   tkgrid(labelRcmdr(plotBicFrame3 , text=gettextRcmdr("                       ")),  sticky="w")


   tkgrid(plotBicFrame ,plotBicFrame2 ,plotBicFrame3 ,sticky="w")  


   tkgrid(Part0Frame ,sticky="w")  
   tkgrid(Part1Frame ,sticky="w")  
   tkgrid(Part2Frame ,sticky="w")  


#######################
## Exit button frame ##
#######################


   ExitFrame <- tkframe(top , borderwidth = 5)

    onCancel <- function() {
            if (GrabFocus()) 
            tkgrab.release(top)
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }

   exitButton <- buttonRcmdr(ExitFrame , text = gettextRcmdr("Exit"), 
        foreground = "red", width = "12", command = onCancel, borderwidth = 3)

   tkgrid(exitButton , sticky = "w")
   tkgrid(ExitFrame , sticky = "w")
   onOK <- function () {}
 
   dialogSuffix(rows = 1, columns = 1)
}

#PlotFabWin (bicObj="Bicfabia",method="Fabia")

