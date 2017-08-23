#' Multisubsets of cardinality k
#'
#' All possible ways to sample k elements from a given set with replacement.
#' Based off multic::subsets()
#' See also: Statistics and Computing, Venebles and Ripley, p49
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
