\name{se}
\alias{se}
\title{Standard Error}
\description{
    This function computes the standard error of the values in a
    vector 'x'.  If 'na.rm' is 'TRUE' then missing values are removed
    before computation proceeds.
}
\usage{
se(x, na.rm=TRUE)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\seealso{
  \code{sd}
}
\examples{se(1:10) ^ 2}
\keyword{univar}
