## PACKAGES AVAILABLE ON CRAN ##
install.packages("biclust")
install.packages("BcDiag")
install.packages("superbiclust")
install.packages("Rcmdr")
install.packages("isa2")
install.packages("s4vd")
# NOTE: At this moment, it is suggested to install s4vd through github
#library(devtools)
#install_github("mwsill/s4vd")

install.packages("gplots") # Extra package

## PACKAGES AVAILABLE ON BIOCONDUCTOR ##
source("http://bioconductor.org/biocLite.R")
biocLite("iBBiG")
biocLite("fabia")
biocLite("rqubic")
biocLite("BicARE")

## Biclust GUI - In Development Version ##
install.packages("RcmdrPlugin.BiclustGUI",
repos="http://R-Forge.R-project.org")


## Note on initial launch of Rcmdr: 
## You might be prompted to isntall a few additional packages
