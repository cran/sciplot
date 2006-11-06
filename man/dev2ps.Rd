\name{dev2ps}
\alias{dev2ps}
\title{Save Graph as Postscript File}
\description{
  This function calls \code{\link{dev.print}} to save an existing graphics
  device as a postscript file.
}
\usage{
dev2ps(file, horizontal=FALSE, title=NULL, ...)
}
\arguments{
  \item{file}{filename of saved graph}
  \item{horizontal}{logical. TRUE for landscape, FALSE for portrait
    orientation}
  \item{title}{default is to use the filename as the title.}
  \item{...}{further parameters to \code{\link{dev.print}}.}

}
\seealso{
  \code{\link{dev.print}}
}
\keyword{device}
