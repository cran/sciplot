dev2ps <- function(file, horizontal=FALSE, title=NULL, ...) {
  if(is.null(title)) title=file
  dev.print(device=postscript, file=file, title=title, onefile=FALSE,
            horizontal=horizontal, ...)
}
