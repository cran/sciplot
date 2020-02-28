\name{bargraph.CI}
\alias{bargraph.CI}
\title{Bar Graph of Calculated Means and CI}
\description{
    Barplot of the mean and standard error (or other summary statistics)
    of a response variable for one-way or higher experimental designs.
}
\usage{
bargraph.CI(x.factor, response, group=NULL, split=FALSE,
    xlab=NULL, ylab=NULL, col=NULL, angle=NULL, density=NULL,
    lc=TRUE, uc=TRUE, legend=FALSE, ncol=1,
    leg.lab=NULL, x.leg=NULL, y.leg=NULL, cex.leg=1,
    bty="n", bg="white", space=if(split) c(-1,1),
    err.width=if(length(levels(as.factor(x.factor)))>10) 0 else .1,
    err.col="black", err.lty=1,
    fun = function(x) mean(x, na.rm=TRUE),
    ci.fun= function(x) c(fun(x)-se(x), fun(x)+se(x)),
    ylim=NULL, xpd=FALSE, data=NULL, subset=NULL, ...)

}
\arguments{
    \item{x.factor}{a factor (required) whose levels will form the x
     axis.}
    \item{response}{a numeric variable giving the response.}
    \item{group}{grouping factor (optional) whose levels will form
          groups of bars for each level of x.factor.}
    \item{split}{logical.  Should groups of bars be displayed back to back.
          See details below.}
    \item{xlab}{default x-axis label. If not specified will parse name from
          x.factor. To leave blank set to NA}
    \item{ylab}{default y-axis label. If not specified will parse name from
          response. To leave blank set to NA}
    \item{col}{default color(s) for bars.}
    \item{angle}{default angle of shading lines.}
    \item{density}{default density of shading lines.}
    \item{lc, uc}{logical.  Should upper or lower CI be drawn?}
    \item{legend}{logical.  Should a legend be included?}
    \item{ncol}{number of columns to use for legend.}
    \item{leg.lab}{legend labels for trace factors.}
    \item{x.leg, y.leg}{optional values to over ride the default legend
          placement.}
    \item{cex.leg}{character expansion value for legend labels.}
    \item{bty}{line type of bounding box for legend.  Defaults to
          'none'.}
    \item{space}{set spacing for bars.}
    \item{err.width}{set width of whiskers for error bars.}
    \item{err.col}{color for error bars. Defaults to "black".}
    \item{err.lty}{line type for error bars.}
    \item{bg}{background color of legend.}
    \item{fun}{the function to compute the summary statistic.
          Should return a single real value.  Defaults to mean with NA
          values removed.}
    \item{ci.fun}{the functions to compute the CI.  Should return a
          vector of length 2 defining the lower and upper limits of the
          CI's.  Defaults to the mean +/- 1 standard error, with NA
          values removed.}
    \item{ylim}{range of y axis.}
    \item{xpd}{defualts to xpd=FALSE (in contrast to \code{barplot})
          to cut off the range of data plotted.}
    \item{data}{an optional data frame.}
    \item{subset}{an optional expression indicating the subset of the
          rows of 'data' that should be used in the plot.}
    \item{...}{further graphical parameters.  See barplot() for
          additional options.}
}
\details{
    Plots a response as a function of treatment (factor) combinations for
    one-way and higher designs.  This is a "wrapper" function for barplot
    that adds confidence intervals - \code{barplot} should be consulted for
    details. Note that the option "beside=TRUE" from barplot is
    hardcoded and that the default for value xpd is FALSE. If the option
    split is specified, the response associated with the second
    grouping factor will be made negative and displayed back-to-back with
    the first.
}
\seealso{
    \code{barplot}, \code{\link{se}}
}
\examples{
data(ToothGrowth)

# One way design
bargraph.CI(x.factor = dose, response = len, data = ToothGrowth) 

# Two-way design with options
bargraph.CI(dose, len, group = supp, data = ToothGrowth,
            xlab = "Dose", ylab = "Growth", cex.lab = 1.5, x.leg = 1,
            col = "black", angle = 45, cex.names = 1.25,
            density = c(0,20), legend = TRUE)
}
\keyword{hplot}
