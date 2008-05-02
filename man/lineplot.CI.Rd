\name{lineplot.CI}
\alias{lineplot.CI}
\title{Line Plot with CI}
\description{
    Lineplot of the mean and standard error (or other summary statistics)
    of a response variable for one-way or higher experimental designs.
}
\usage{
lineplot.CI(x.factor, response, group=NULL, type="b", x.cont=FALSE,
    legend=TRUE, trace.label=NULL, leg.lab=NULL, fixed=FALSE, x.leg=NULL,
    y.leg=NULL, cex.leg=1, ncol=1, 
    pch=c(16, 21, 15, 22, 17, 24, c(3:14)), 
    fun = function(x) mean(x, na.rm=TRUE),
    ci.fun= function(x) c(fun(x)-se(x), fun(x)+se(x)),
    err.width = if(length(levels(as.factor(x.factor))) > 10) 0 else 0.1,
    err.col = col, err.lty = 1, 
    xlim=NULL, ylim=NULL, cex=NULL, lwd=NULL, col="black", cex.axis=1,
    xaxt="s", data=NULL, subset=NULL, ...)
}
\arguments{
    \item{x.factor}{a factor (required) whose levels will form the x
          axis.}
    \item{response}{a numeric variable giving the response.}
    \item{group}{grouping factor (optional) whose levels will form 
        the traces.}
    \item{type}{the type of plot: lines, points, or both. Defaults to
          both.}
    \item{x.cont}{logical.  Treat x.factor as a continuous variable?}
    \item{legend}{logical.  Should a legend be included?}
    \item{trace.label}{overall legend label.}
    \item{leg.lab}{legend labels for trace factors.}
    \item{fixed}{logical.  Should the legend be in the order of the
          levels of 'trace.factor' or in the order of the traces at
          their right-hand ends?}
    \item{x.leg, y.leg}{optional values to over ride the default legend
          placement.}
    \item{cex.leg}{character expansion value for legend labels.}
    \item{ncol}{number of columns to use for legend.}
    \item{pch}{a vector of plotting symbols or characters.}
    \item{fun}{the function to compute the summary statistic.
          Should return a single real value.  Defaults to mean with NA
          values removed.}
    \item{ci.fun}{the functions to compute the CI.  Should return a
          vector of length 2 defining the lower and upper limits of the
          CI's.  Defaults to the mean +/- 1 standard error, with NA
          values removed.}
    \item{err.width}{set width of whiskers for error bars.}
    \item{err.col}{color for error bars. Defaults to col.}
    \item{err.lty}{line type for error bars.}
    \item{xlim, ylim}{range for x and y axes.}
    \item{cex}{overall plot character expansion value.}
    \item{lwd}{deterimines line width.}
    \item{col}{default color(s) for plot.}
    \item{cex.axis}{character expansion value for axis labels.}
    \item{xaxt}{should x-axis be drawn?}
    \item{data}{an optional data frame.}
    \item{subset}{an optional expression indicating the subset of the
          rows of 'data' that should be used in the plot.}
    \item{...}{further graphical parameters.}
}
\details{
    Plots a response as a function of treatment (factor) combinations for
    one-way and higher designs.  This is a "wrapper" function for plot in
    one-way designs and interaction.plot in higher-way designs that adds
    confidence intervals - those functions should be consulted for
    details. Notable changes from the defaults for interaction.plot
    include the removal of NA values by default, options to specify a
    subset of data and greater flexibility in the placement of legends.
    This function replicates some of the functionality of plotmeans from
    the package gplots, with differences in the treatment of two-way and
    higher designs.
}
\seealso{
    \code{interaction.plot}, \code{\link{se}}
}
\examples{
data(ToothGrowth)

# One way design
lineplot.CI(x.factor = dose, response = len, data = ToothGrowth) 

# Two-way design with options
lineplot.CI(dose, len, group = supp, data = ToothGrowth, cex = 2,
            xlab = "Dose", ylab = "Growth", cex.lab = 1.5, x.leg = 1,
            col = c("blue","red"), pch = c(16,16))
}
\keyword{hplot}
