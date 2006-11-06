xppauto.plot <- function(data, xlim=NULL, ylim=NULL,
                         max.step=NULL, min.step=NULL, ...) {
  
  if(is.null(xlim))
    xlim <- c(min(data$V1), max(data$V1))
  if(is.null(ylim))
    ylim <- c(min(data$V2,data$V3), max(c(data$V2,data$V3)))
  if(is.null(max.step))
    max.step <- (xlim[2]-xlim[1])*.075
  if(is.null(min.step))
    min.step <- (xlim[2]-xlim[1])*.0075

  data.ymin <- subset(data, V2>ylim[1]&V3>ylim[1])
  cats <- levels(as.factor(data.ymin$V4))
  
  thin.fn <- function(x) {
      index <- vector()
      index[1] <- 1
      if(length(x$V1)>1) {
        for(i in 2:length(x$V1)) {
          if(abs(x$V1[i]-x$V1[max(index)])>=min.step)
            index[(length(index)+1)] <- i
        }
      }
      x[index,]
    }
  
  breaks.fn <- function(x) {  # Split up "unconnected" vectors
    breaks <- vector()
    breaks[1] <- 1
    for(i in 1:(length(x)-1)) {
      breaks[i+1] <- ifelse(abs(x[i+1]-x[i])>max.step,breaks[i]+1, breaks[i])
      }
    return(breaks)
  }

  plot(xlim, ylim, type="n", ...)

  for(i in 1:length(cats)) {
    data.cat <- subset(data.ymin, V4==cats[[i]])
    breaks <- breaks.fn(data.cat$V1)
    for(j in 1:max(breaks)) {
      data.break <- subset(data.cat, breaks==j)
      data.plot <- if(i<3) data.break else thin.fn(data.break)
      resp1 <- data.plot$V2
      resp2 <- data.plot$V3
      
      points(resp1~V1, data=data.plot, type=ifelse(i<3, "l", "p"),
             lty=if(i==2) 2, pch=if(i==3) 16)
      points(resp2~V1, data=data.plot, type=ifelse(i<3, "l", "p"),
             lty=if(i==2) 2, pch=if(i==3) 16)
    }
  }
  invisible(ylim=ylim)
}
