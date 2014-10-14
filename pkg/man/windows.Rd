\name{Window Functions}
\alias{bcdiag_WINDOW}
\alias{bcdiagwrite_WINDOW}
\alias{biclustextraplots_WINDOW}
\alias{biclustbimax_WIN}
\alias{biclustCC_WIN}
\alias{fabialaplace_WIN}
\alias{fabiapostprojection_WIN}
\alias{fabiasparsenessprojection_WIN}
\alias{fabiaSPARSE_WIN}
\alias{iBBiG_WIN}
\alias{isaextraplots_WINDOW}
\alias{isadefault_WIN}
\alias{biclustplaid_WIN}
\alias{biclustquest_WIN}
\alias{biclustspectral_WIN}
\alias{superbiclust_WINDOW}
\alias{biclustXMotif_WIN}
\alias{newmethod_WINDOW}
\alias{newtool_WINDOW}

\title{Window Functions}

\description{List of window functions, responsible for creating all the dialogs.}
\usage{
bcdiag_WINDOW(methodname)
bcdiagwrite_WINDOW(methodname)
biclustextraplots_WINDOW(methodname)
biclustbimax_WIN()
biclustCC_WIN()
fabialaplace_WIN()
fabiapostprojection_WIN()
fabiasparsenessprojection_WIN()
fabiaSPARSE_WIN()
iBBiG_WIN()
isaextraplots_WINDOW(methodname)
isadefault_WIN()
biclustplaid_WIN()
biclustquest_WIN()
biclustspectral_WIN()
superbiclust_WINDOW(methodname)
biclustXMotif_WIN()

}

\arguments{
\item{methodname}{These windows are called from a specific biclustering method window. The argument is a string containing the name of the method it is called from.}
}

\details{All these window functions for all the biclustering dialogs were created using the template scripts (\code{newmethod_script.R} & \code{newtool_script.R)}.
A detailed description with an example on how to use these scripts and how to create the dialogs, can be found in the appendix (Guideline to New Implementations).

Please note that these template scripts are also located in the \code{doc} subdirectory of the package.
}


\author{Ewoud De Troyer \email{ewoud.detroyer@student.uhasselt.be}}

