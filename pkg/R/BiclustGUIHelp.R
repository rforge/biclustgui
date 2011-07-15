`BiclustGUIHelp` <-
function() {
          tkgrab.release(window)
    	    helpIndex <- file.path(system.file("doc",package="RcmdrPlugin.BiclustGUI"),"index.html")
          browseURL(helpIndex)
            }
