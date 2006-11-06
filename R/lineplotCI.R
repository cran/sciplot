# Function begins here
lineplot.CI <- function(x.factor, response, group = NULL, type = "b",
                        legend = TRUE, trace.label = NULL, leg.lab = NULL,
                        fixed = FALSE, x.leg = NULL, y.leg = NULL, cex.leg = 1,
                        ncol = 1, pch = c(16,21,15,22,17,24, c(3:14)),
                        fun = function(x) mean(x, na.rm=TRUE),
                        ci.fun = function(x) c(fun(x)-se(x), fun(x)+se(x)),
                        xlim = NULL, ylim = NULL, cex = NULL, lwd = NULL,
                        col = "black", cex.axis = 1, xaxt = "s", data = NULL,
                        subset = NULL, ...) {
  # Set up environment
  subset <- eval(substitute(subset), envir=data)

  if(!is.null(data)) {
    if(!is.null(subset)) data <- subset(data,subset)
    x.factor <- eval(substitute(x.factor), envir=data)
    response <- eval(substitute(response), envir=data)
    group <- eval(substitute(group), envir=data)
  }

  subset = NULL
  
  #////////////////////////////////////////////////////////////////////#
  # Below modified from the interaction.plot function in stats package #
  # Take the legend section out to allow greater flexibility           #
  #////////////////////////////////////////////////////////////////////#

  int.plot <- function (x.factor = x.factor, group = group,
                        response = response, type = c("l","p", "b"),
                        legend = legend,
                        trace.label = deparse(substitute(group)),
                        fixed = FALSE, xlab = deparse(substitute(x.factor)),
                        ylab = ylabel, lty = nc:1, pch = NA, xpd = NULL,
                        leg.bg = par("bg"), leg.bty = "n", xtick = FALSE,
                        xlim=xlim, ylim=ylim, axes = TRUE, ...) {
  
    ylabel <- paste(deparse(substitute(fun)), "of ", 
                    deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, group), fun)
    nr <- nrow(cells)
    nc <- ncol(cells)
    xvals <- 1:nr
    if (is.ordered(x.factor)) {
      wn <- getOption("warn")
      options(warn = -1)
      xnm <- as.numeric(levels(x.factor))
      options(warn = wn)
      if (!any(is.na(xnm)))
        xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs))
      xlabs <- as.character(xvals)
    if (is.null(ylabs))
      ylabs <- as.character(1:nc)
    if(is.null(xlim)) { 
      xlim <- range(xvals)
      xleg <- xlim[2] + 0.05 * diff(xlim)
      xlim <- xlim + c(-0.2/nr, if (legend&is.null(x.leg)&is.null(y.leg))
                       0.2 + 0.02 * nch else 0.2/nr) * diff(xlim)}
    else {
      xlim
      xleg <- xlim[2] - 0.25 * diff(xlim)}
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim,
            xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col,
            lty = lty, lwd=lwd, pch = pch)
    if (axes && xaxt != "n") {
      axisInt <- function(x, main, sub, lwd, bg, log, asp,
                          ...) axis(1, x, ...)
      mgp. <- par("mgp")
      if (!xtick)
        mgp.[2] <- 0
      axisInt(1, at = xvals, labels = xlabs, tick = xtick,
              mgp = mgp., xaxt = xaxt, ...)
    }
    ord <- sort.list(cells[nr, ], decreasing = TRUE)
    if (legend) {
      yrng <- diff(ylim)
      yleg <- ylim[2] - 0.1 * yrng
      if (!is.null(xpd) || {
        xpd. <- par("xpd")
        !is.na(xpd.) && !xpd. && (xpd <- TRUE)
      }) {
        op <- par(xpd = xpd)
        on.exit(par(op))
      }
      text(xleg, ylim[2] - 0.05 * yrng, paste("  ", trace.label),
           adj = 0)
      if (!fixed) {
        ylabs <- ylabs[ord]
        lty <- lty[1 + (ord - 1)%%length(lty)]
        col <- col[1 + (ord - 1)%%length(col)]
        pch <- pch[ord]
      }
    }
    invisible()

    return.data<-if(legend)
      list(pch=pch,ord=ord,xleg=xleg,yleg=yleg,ylabs=ylabs,lty=lty,
           leg.bty=leg.bty,leg.bg=leg.bg,ord=ord)
    else list(pch=pch,ord=ord)
    return(return.data)
  }
  #////////////////////////////////////////////////////////////////////#
  #                         End Section                                #
  #////////////////////////////////////////////////////////////////////#

  # Figure out if we're dealing with 1 or more total groups (i.e., we always
  # have 1 "x.factor" and we may have 1 or more "group".
  if(is.null(group)) groups = x.factor else {   
    # If more than 1 "y-factor", combine for plotting
    if(length(group[[1]]) > 1)
      group <- do.call("paste", c(group, sep = "."))
    # "groups" defines the combination of "x.factor" and "group"
    groups <- list(x.factor, group)
  }

  # Calculate mean and SE's
  mn.data <- tapply(response, groups, fun)
  CI.data <-
    array(unlist(tapply(response, groups, ci.fun)),
          c(2,
            length(levels(as.factor(x.factor))),
            if(is.null(group)) 1
            else length(levels(as.factor(group)))))
  CI.L <- CI.data[1,,]
  CI.H <- CI.data[2,,]
  
  # Replace undefined SE with zero. Note that this will return the warning
  # message: "zero-length arrow is of indeterminate angle and so skipped"
  replace.NA <- function(x) if(is.na(x)) 0 else x
  
  if(!is.null(group)) {
    CI.L <- apply(CI.L, c(1,2), replace.NA)
    CI.H <- apply(CI.H, c(1,2), replace.NA)
  }
  else {
    CI.L <- as.vector(unlist(lapply(CI.L, replace.NA)))
    CI.H <- as.vector(unlist(lapply(CI.H, replace.NA)))
  }
  
  # Determine y-axis plot region
  plot.limits = c(min(CI.L), max(CI.H))

  # Draw lines
  if(is.null(group)) {
    plot(mn.data, xaxt="n", type=type, col=col, pch=NA, cex=cex,
         cex.axis=cex.axis,
         xlim=if(is.null(xlim)) c(.8,length(mn.data)+.2) else xlim,
         ylim=if(is.null(ylim)) plot.limits else ylim, ...)
    if(xaxt!="n") axis(1,label=names(mn.data),
         at=c(1:length(names(mn.data))), cex.axis=cex.axis)
  }
  else leg.vals <-
    int.plot(x.factor, group, response, type = type,
             xlim = xlim, ylim = if(is.null(ylim)) plot.limits else ylim,
             cex.axis = cex.axis, trace.label = trace.label, pch = NA,
             legend = legend, ...)

  # Draw CI's
  nlevels.x <- dim(mn.data)[1]
  if(is.null(group))
    arrows(seq(1:nlevels.x), CI.L, seq(1:nlevels.x), CI.H, angle = 90,
           col = col, length = 0.1, code = 3, lwd = lwd)
  else {
    nlevels.y <- dim(mn.data)[2]
    for(i in 1:nlevels.y)
      arrows(seq(1:nlevels.x), CI.L[,i], seq(1:nlevels.x),
             CI.H[,i], angle=90, col=if(length(col)>1) col[i] else col,
             length=.1, code=3,lwd=lwd)
  }

  # Draw points (Note: adding points at this point allows points to be in
  # the foreground)
  if(type %in% c("p", "b")) {
    if(is.null(group))
      points(mn.data,pch=pch[1],bg="white",cex=cex,col=col)
    else {
      nlevels.y<-dim(mn.data)[2]
      for(i in 1:nlevels.y)
        points(mn.data[,i],pch=pch[i],bg="white",
               col=if(length(col)>1) col[i] else col,cex=cex)
    }
  }

  #////////////////////////////////////////////////////////////////////#
  # Below modified from the interaction.plot function in stats package #
  #////////////////////////////////////////////////////////////////////#

  if(legend & !is.null(group)) {
    legend(x=if(is.null(x.leg)) leg.vals$xleg else x.leg,
           y=if(is.null(y.leg)) leg.vals$yleg else y.leg,
           legend = if(!is.null(leg.lab)) leg.lab else {
             if(fixed) leg.vals$ylabs[leg.vals$ord] else leg.vals$ylabs
           },
           pch = if(type %in% c("p", "b") & !fixed) pch[leg.vals$ord] else pch,
           col = if(type %in% c("p", "b") & length(col)>1 & !fixed)
           col[leg.vals$ord] else col,
           lty = if (type %in% c("l", "b") & fixed) leg.vals$lty[leg.vals$ord]
           else leg.vals$lty,
           ncol=ncol, bty = leg.vals$leg.bty, bg = leg.vals$leg.bg,
           cex=cex.leg)
  }

  #////////////////////////////////////////////////////////////////////#
  #                      End Section                                   #
  #////////////////////////////////////////////////////////////////////#

  invisible(list(vals=mn.data, CI=CI.data))
}
