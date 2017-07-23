#' Plot mean and median with bootstrapped confidence intervals.
#'
#' An alternative to \code{\link{boxplot}} that plots the 
#' mean and median for each variable and their confidence intervals,
#' all calculated using bootstrapping.
#' 
#' @param x vector, matrix, data.frame or list of variables.

#' @param nrep number of repetitions. By default set to
#' \code{max(10, (10^5)/lengths(x))}.

#' @keywords multivariate hplot
#' @export
#' @examples
#' ## works with variables of unequal length.
#' ## returs invisibly the output of bootstats for each variable,
#' ## plus the x-positions used when plotting.
#' set.seed(1)
#' 
#' l <- list(a=c(rnorm(5)), b=rnorm(9), ctrl=rnorm(15, 0.5))
#' (errorplot(l, ylab="Q", main="Pilot test results"))
#'  
#' ## many variables
#' set.seed(1)
#' xx <- matrix(0.1*(1:40)*runif(160, 1, 4)^2, 20, byrow=TRUE)
#' errorplot(xx, ylab="Magnitude")
#' 
#' ## more advanced graphical parameters
#' set.seed(1)
#' n <- 20
#' dtf <- data.frame(aa=runif(n, 0, 2)^2, bb=rnorm(n), cc=rgamma(n, 1.5))
#'
#' col.mean <- "darkgray"
#' col.median <- "lightgray"
#' col.box <- rainbow(3, start=0.6, end=1)
#'
#' z <- errorplot(dtf, p.level=0.99, whisker.length=0.9, box.width=0.7,
#'   col.mean=col.mean, col.median=col.median, col.box=col.box, 
#'   lwd.box=2, lwd.centre=1, lty.centre=2)
#'


errorplot <- function(x, nrep=NULL, p.level=0.95, smooth=TRUE, box.width=0.5,
  whisker.length=box.width*1.4, col.mean="lightblue", col.median="orange",
  col.box=1, lwd.box=1, lwd.centre=2, lty.centre=1, ...) {
          
    opar <- par(no.readonly=TRUE)
    par(lend=0)
    on.exit(par(opar))
    
    # pad with NAs to make lengths equal
    if (is.list(x)) {
        x <- lapply(x, 'length<-', max(lengths(x)))
    }
    
    x <- data.frame(x)
    
    colMax <- function(x) sapply(data.frame(x), max)
    colMin <- function(x) sapply(data.frame(x), min)
    
    stats0 <- lapply(x, bootstats, nrep=nrep, p.level=p.level, smooth=smooth)
    stats0 <- as.data.frame(stats0)
    stats <- stats0[!rownames(stats0) %in% c("nrep", "p.level"), , drop=FALSE]
    stats <- as.matrix(stats)
    
    if (ncol(x) == 1) {
        stats <- as.matrix(data.frame(stats))
        colnames(stats) <- names(x)
        xseq <- 0.5
        wpar <- 2
    } else {
        ng <- ncol(stats)
        xseq <- seq(0, 1, length.out=ng) 
        wpar <- (1/(2*(ng-1)))
    }
    
    whisker <- wpar*max(c(whisker.length, box.width))
    box <- wpar*box.width
    
    xmin <- 0-whisker
    xmax <- 1+whisker 

    ymin <- min(unlist(stats)) 
    ymax <- max(unlist(stats))
    
    value <- stats["b.mean",]
    plot(xseq, value, xlim=c(xmin, xmax), ylim=c(ymin, ymax),
      type="n", xaxt="n", xlab="", ...)
    
    ### Mean, left
        rect(xseq-box, stats["mean.lower",],
             xseq, stats["mean.upper",], 
             col=col.mean, border=NA)
    # Centre
    segments(x0=xseq-box, y0=stats["b.mean",],
             x1=xseq, y1=stats["b.mean",], 
             lwd=lwd.centre, col=col.box, lty=lty.centre, lend=1)
    # Upper confidence
    segments(x0=xseq-whisker, y0=stats["mean.upper",], 
             x1=xseq, y1=stats["mean.upper",], 
             lwd=lwd.box, col=col.box)
    # Lower confidence
    segments(x0=xseq-whisker, y0=stats["mean.lower",], 
             x1=xseq, y1=stats["mean.lower",], 
             lwd=lwd.box, col=col.box)
    # Outer box segment
    segments(x0=xseq-box, y0=stats["mean.lower",], 
             x1=xseq-box, y1=stats["mean.upper",], 
             lwd=lwd.box, col=col.box)


    ### Median, right
        rect(xseq+box, stats["median.lower",],
             xseq, stats["median.upper",], 
             col=col.median, border=NA)
    # Centre
    segments(x0=xseq, y0=stats["b.median",], 
             x1=xseq+box, y1=stats["b.median",], 
             lwd=lwd.centre, col=col.box, lty=lty.centre, lend=1)
    # Upper confidence
    segments(x0=xseq, y0=stats["median.upper",], 
             x1=xseq+whisker, y1=stats["median.upper",], 
             lwd=lwd.box, col=col.box)
    # Lower confidence
    segments(x0=xseq, y0=stats["median.lower",], 
             x1=xseq+whisker, y1=stats["median.lower",], 
             lwd=lwd.box, col=col.box)
    # Outer box segment
    segments(x0=xseq+box, y0=stats["median.lower",], 
             x1=xseq+box, y1=stats["median.upper",], 
             lwd=lwd.box, col=col.box)
    
    
    # Vertical
    segments(x0=xseq, y0=colMin(stats), x1=xseq, y1=colMax(stats),
    lwd=lwd.box, col=col.box)
    
    axis(1, xseq, colnames(x))

    invisible(list(stats=stats0, x=xseq))
}
