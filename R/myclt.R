#' @title My CLT
#'
#' @param n sample size
#' @param iter number of iterations
#'
#' @return Creates a uniform distribution of the central limit theorem, given a
#' set sample size and number of iterations.
#'
#' @importFrom stats runif
#' @export
#'
#' @examples
#' \dontrun{myclt(n=50,iter=10000)}
#'
myclt=function(n,iter){
  y=runif(n*iter,0,5)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
}
