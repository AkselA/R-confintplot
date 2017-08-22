#' Calculate mean and median with bootstrapped confidence intervals.
#'
#' This function returns the bootstrapped mean and median, 
#' and the respective confidence intervals, of the supplied numeric vector.
#' @param x A vector of numerical values.
#' @param p.level A number giving the level of confidence for the intervals
#' @param nrep The number of repetitions, or resamples, to perform. Defaults to
#' \code{max(500, (10^6)/length(x))}, except if \code{length(x) <= exact.thrs}.
#' @param exact.thrs Upper level of \code{length(x)} at which all 
#' @keywords univar
#' @export
#' @examples
#' bootstats(rnorm(100), p.level=0.99)
#' 
#' bootstats(c(1, 3, 4, 4, 6, 7, 9), exact.thrs=7, smooth=FALSE)
#' 
#' bootstats(c(1, 3, 4, 4, 6, 7, 8), exact.thrs=1, smooth=FALSE)
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

bootstats <- function(x, p.level=0.95, nrep=NULL, exact.thrs=8, 
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
        b <- t(multisubsets(x))
        nrep <- ncol(b)
        b <- do.call(smooth, as.list(formals(smooth)))

    } else {
        
        if (is.null(nrep)) {
            nrep <- (10^6)/length(x)
            nrep <- floor(max(500, nrep))
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
    	stats <- list(stats, resamples=b)
    } else {
    	stats <- list(stats)
    }
    class(stats) <- c("bootlist", class(stats))    
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
    x <- round(x[[1]], 5)
    NextMethod()
}

#' Multisubsets of cardinality k
#'
#' All possible ways to sample k elements from a given set with replacement.
#'
#' @param v A vector to sample from
#' @param k The size of the subsamples
#' @examples
#' 
#' multisubsets(1:3, 4)
#' 
#' multisubsets(1:4, 3)
#'
#' multisubsets(c("red", "blue", "green"))
multisubsets <- function(v, k=length(v)) {
	if(k <= 0 | length(v) <= 0)
		NULL
	else 
	    if (k == 1)
		matrix(v, ncol = 1)
	else
	    rbind(cbind(v[1], 
	          Recall(v, k - 1)),
	          Recall(v[-1], k))
}

multichoose <- function(n, k=n) {
	choose(n + k - 1, k)
}
