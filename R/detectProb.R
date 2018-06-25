#' detectProb
#'
#' Function that produces empirical estimates of probability to detect a foreign species, and associated costs
#' based on varying occurrences, detectabilities and survey schemes.
#'
#'
#'
#' @param occProb vector of occurence probabilities to test
#' @param detectProb vector of detection probabilities
#' @param locations vector of number of locations to visit/sample
#' @param visits vector of number of visits per sample
#' @param visitCost vector of costs of each visit
#' @param nBoot number of bootstrap samples to calculate std error of percent detected
#'
#'
#' @examples{
#' tt <- detectProp(occProb = seq(0.01, 0.9, by = 0.05),
#' detectProb = 0.8,
#' locations = 30,
#' visits =  4,
#' visitCost = 10,
#' nBoot =  9999)
#'
#' }
#'

##Make class with plot function?

detectProp <- function(occProb,
                       detectProb,
                       locations,
                       visits,
                       visitCost,
                       nBoot){

  calc1 <- function(occProb,
                    detectProb,
                    locations,
                    visits,
                    visitCost,
                    nBoot){


    detectFun <- function(occProb,
                          detectProb,
                          visits){
      occur <- rbinom(n = locations, size = 1, prob = occProb)
      detect <- rbinom(n = occur, size = visits, prob = occur * detectProb)
      out <- as.integer(sum(detect) > 0)
      return(out)
    }

    reps <- replicate(n = nBoot,
              detectFun(occProb = occProb,
                           detectProb = detectProb,
                           visits = visits)
              )


    out <- dplyr::tibble(
      occProb = occProb,
      detectProb = detectProb,
      locations = locations,
      visits = visits,
      visitCost = visitCost,
      percDetected = sum(reps) / length(reps),
      std.error = sd(reps),
      totCost = sum(locations * visits * visitCost)
    )

    class(out) <- c("detectProp", "tbl_df", "tbl", "data.frame")
    return(out)
  }


input <- data.frame(occProb,
                    detectProb,
                    locations,
                    visits,
                    visitCost,
                    nBoot)

out <- do.call(rbind.data.frame, apply(input, 1,  function(x) calc1(x[1], x[2], x[3], x[4], x[5], x[6])))


}






tt <- detectProp(occProb = seq(0.01, 0.9, by = 0.05),
                    detectProb = 0.8,
                    locations = 30,
                    visits =  4,
                    visitCost = 10,
                    nBoot =  9999)

