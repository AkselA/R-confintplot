#' Calculate mean and median with bootstrapped confidence intervals.
#'
#' This function returns the bootstrapped mean and median, 
#' and the respective confidence intervals, of the supplied numeric vector.
#' @param x A vector of numerical values.
#' @keywords univar
#' @export
#' @examples
#' bootstats(1:4)
#' 
#' bootstats(1:4, nrep=9999)
#' 
#' ## Simple smooth function based on jitter()
#' bootstats(1:5, smooth=function(b=b) jitter(b))
#' 
#' ## Alternative entropy based smooth function
#' x <- round(rnorm(15, 10, 2))
#'
#' entropy <- function(x, base=2) {
#'     freqs <- table(x) / length(x)
#'     -sum(freqs * log(freqs, base=base))
#' }
#'
#' H5 <- entropy(x, base=5)
#'
#' bootstats(x, 
#'   smooth=function(b=b, x=x, H5=H5) {
#'       b + rnorm(length(b), 0, H5/sqrt(length(x)))
#'   })
#'
#' bootstats(x)

bootstats <- function(x, p.level=0.95, nrep=NULL, exact.thrs=5, 
  smooth=TRUE, return_resamples=FALSE) {
    
    x <- na.omit(x)
        
    if (!is.function(smooth)) {
        if (is.logical(smooth)) {
            smooth <- ifelse(smooth,
              function(b=b, x=x) {
              	  b + rnorm(length(b), 0, 1/sqrt(length(x)))
              },
              function(b=b) b)
        } else {
            stop("smooth needs to be a function or logical")
        }
    }
    
    if (length(x) <= exact.thrs & is.null(nrep)) {
        b <- t(expand.grid(rep(list(x), length(x))))
        nrep <- ncol(b)
        b <- do.call(smooth, as.list(formals(smooth)))

    } else {
        
        if (is.null(nrep)) {
            nrep <- (10^5)/length(x)
            nrep <- floor(max(100, nrep))
        }
        b <- replicate(nrep, sample(x, length(x), replace=TRUE))
        b <- do.call(smooth, as.list(formals(smooth)))
    }
        
    mn <- apply(b, 2, mean)
    b.mean <- mean(mn)
    mean.upper <- quantile(mn, (1-p.level)/2)
    mean.lower <- quantile(mn, 1-((1-p.level)/2))
    
    md <- apply(b, 2, median)
    b.median <- mean(md)
    median.upper <- quantile(md, (1-p.level)/2)
    median.lower <- quantile(md, 1-((1-p.level)/2))
    
    stats <- data.frame(b.mean, mean.upper, mean.lower, 
                        b.median, median.upper, median.lower, 
                        nrep, p.level)
    rownames(stats) <- NULL
    stats <- t(stats)
    
    if (return_resamples) {
    	stats <- list(resamples=b, stats)
    	class(stats) <- c("bootlist", class(stats))
    }
    
    stats
    
}

#' Print method for class bootlist.
#'
#' @examples
#' x <- c(1, 3, 4, 2, 8)
#' bst <- bootstats(x, nrep=5, return_resamples=TRUE)
#' bst
#' str(bst)
#' bst$resamples

print.bootlist <- function(x, ...){
    x <- x[[2]]
    NextMethod()
}



