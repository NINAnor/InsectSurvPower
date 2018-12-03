#' expEnd
#'
#' Calculates the resulting number from a initial number given a yearly decay rate, given an exponential change function
#'
#' @param N0 Initial amount
#' @param lambda yearly rate of change
#' @param T timespan to calculate over
#' @param perc should the result be presented as a percentage of the initial value? True/False
#' @export

expEnd <- function(N0 = 100, lambda, T, perc = T){

  end <- N0 * exp(lambda * T)

  if(perc){
    out <- abs(N0 - end) / N0
    if(end < N0){out <- - out}
    return(out)
  } else return(end)

}
