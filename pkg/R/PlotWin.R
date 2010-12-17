PlotWin <-
function (bicObj,method) {
   ###############################
   ##### Parallel plots Frame ####
   ###############################
  initializeDialog(title = gettextRcmdr(paste("Plots", method, sep=" - ") ))

   plotsFrame <- tkframe(top,relief="groove", borderwidth=2)

   checkBoxes(plotsFrame ,frame="paralelPlotFrame", boxes=c("plotColoumn", "plotBoth", "Compare","type2"),
         initialValues=c(rep(1, 3),0), labels=gettextRcmdr(c("Plot Coloumn", "Plot Both", "Compare", "Type 2")) )

   numClustFrame <- tkframe(plotsFrame )
   numClustVal <- tclVar("1")
   numClustField <- ttkentry(numClustFrame , width = "2",textvariable = numClustVal )

   ######################
   ## Draw paralel plot##
   ######################
   onDRAW <- function() {
     .activeDataSet <- ActiveDataSet()
     plotcol <- if (tclvalue(plotColoumnVariable) == "1") ", plotcol=TRUE"
            else ", plotcol=FALSE"

     plotBoth <- if (tclvalue(plotBothVariable) == "1") ", plotBoth=TRUE"
            else ", plotBoth=FALSE"

     compare<- if (tclvalue(plotBothVariable) == "1") ", compare=TRUE"
            else ", compare=FALSE"

     clusterVal <- if (tclvalue(numClustVal) == "") ""
            else paste(",number=", tclvalue(numClustVal ), sep="" )


    if (tclvalue(type2Variable) == "1")
              doItAndPrint(paste("parallelCoordinates2(as.matrix(",.activeDataSet, "),bicResult=",bicObj, clusterVal,",info=TRUE)" , sep="") )

     else {
            doItAndPrint(paste("parallelCoordinates(as.matrix(",.activeDataSet, "),bicResult=",bicObj, clusterVal ,
                plotBoth , compare,",info=TRUE)" , sep="") )
              }

    }

   buttonsPlotFrame <- tkframe(plotsFrame , borderwidth = 5)

   Drawbutton <- buttonRcmdr(buttonsPlotFrame , text = gettextRcmdr("Draw Plot"),
        foreground = "darkgreen", width = "12", command = onDRAW ,
        default = "active", borderwidth = 3)


   tkgrid(labelRcmdr(plotsFrame , text=gettextRcmdr("Parallel Coordinate Plot:")),  sticky="w")

   tkgrid(labelRcmdr(numClustFrame , text=gettextRcmdr("Cluster Number :")), numClustField , sticky="w")

   tkgrid(Drawbutton ,  sticky = "w")

   tkgrid(paralelPlotFrame, numClustFrame ,buttonsPlotFrame ,sticky="w")

   tkgrid(plotsFrame , sticky = "w")

   ########################
   #### Heat Map Frame ####
   ########################
   heatMapFrame <- tkframe(top,relief="groove", borderwidth=2)

   checkBoxes(heatMapFrame ,frame="HeatPlotFrame", boxes=c("local"),
         initialValues=1, labels=gettextRcmdr(c("local")) )


   numClust2Frame <- tkframe(heatMapFrame )
   numClust2Val <- tclVar("1")
   numClust2Field <- ttkentry(numClust2Frame , width = "2",textvariable = numClust2Val )

   ## Draw Heat Map ##
    onDRAW2 <- function() {
     .activeDataSet <- ActiveDataSet()
     local <- if (tclvalue(localVariable) == "1") ", local=TRUE"
            else ", local=FALSE"

     cluster2Val <- if (tclvalue(numClust2Val ) == "") ""
            else paste(",number=", tclvalue(numClust2Val), sep="" )

     doItAndPrint(paste("drawHeatmap(as.matrix(",.activeDataSet, "),bicResult=",bicObj, cluster2Val ,
            local , " )" , sep="") )

     }

   buttonsHeatFrame <- tkframe(heatMapFrame , borderwidth = 5)

   DrawbuttonHeat <- buttonRcmdr(buttonsHeatFrame , text = gettextRcmdr("Draw Plot"),
        foreground = "darkgreen", width = "12", command = onDRAW2 ,
        default = "active", borderwidth = 3)

   tkgrid(labelRcmdr(heatMapFrame, text=gettextRcmdr("Heatmap Plot:")),  sticky="w")
   tkgrid(labelRcmdr(numClust2Frame , text=gettextRcmdr("Cluster Number :")), numClust2Field , sticky="w")
   tkgrid(DrawbuttonHeat , sticky = "w")
   tkgrid(HeatPlotFrame, numClust2Frame ,buttonsHeatFrame, sticky="w")

   tkgrid(heatMapFrame, sticky = "w")

  ########################
   #### Clustmember Map Frame ####
   ########################
   memberMapFrame <- tkframe(top,relief="groove", borderwidth=2)

   checkBoxes(memberMapFrame ,frame="MemberPlotFrame", boxes=c("mid"),
         initialValues=1, labels=gettextRcmdr(c("mid")) )


   cllabelFrame <- tkframe(memberMapFrame )
   cllabelVal <- tclVar(" ")
   cllabelField <- ttkentry(cllabelFrame , width = "8",textvariable = cllabelVal )

   ## Draw Member Map ##
   onDRAW3 <- function() {
     .activeDataSet <- ActiveDataSet()
     mid <- if (tclvalue(midVariable) == "1") ", mid=TRUE"
            else ", mid=FALSE"

     cluster2Val <- if (tclvalue(cllabelVal ) == "") ""
            else paste(",cl_label=\"", tclvalue(cllabelVal),"\"", sep="" )

     doItAndPrint(paste("biclustmember(bicResult=",bicObj, ",as.matrix(",.activeDataSet,")", cluster2Val ,
            mid , " )" , sep="") )

     }

   buttonsMemberFrame <- tkframe(memberMapFrame , borderwidth = 5)

   DrawbuttonMember <- buttonRcmdr(buttonsMemberFrame , text = gettextRcmdr("Draw Plot"),
        foreground = "darkgreen", width = "12", command = onDRAW3 ,
        default = "active", borderwidth = 3)

   tkgrid(labelRcmdr(memberMapFrame, text=gettextRcmdr("Biclustmember Plot:")),  sticky="w")
   tkgrid(labelRcmdr(cllabelFrame , text=gettextRcmdr("Cluster Label :")), cllabelField , sticky="w")
   tkgrid(DrawbuttonMember , sticky = "w")
   tkgrid(MemberPlotFrame, cllabelFrame ,buttonsMemberFrame, sticky="w")

   tkgrid(memberMapFrame, sticky = "w")



## Exit button frame ##

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

