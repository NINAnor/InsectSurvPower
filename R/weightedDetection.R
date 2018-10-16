#' weightedDetection
#'
#' Calculate the probability of detecting a species with weighted occurrences and visits
#'
#'
#' @param occWeights occurrenceWeights
#'
#'
#'
#' @import ggplot2
#'
#'
#' @examples
#' \dontrun{
#'
#' tt <- weightedDetection(noOccur = 1,
#' noLocations = 1:10,
#' detectProb =  0.5)
#'
#' plot(tt)
#'
#' occ <- createOccProb(map10km)
#'
#' visWeights <- occ %>%
#'  select(sites = ssbid,
#'          weights = prob)
#'
#' system.time(tt <- weightedDetection(occWeights = visWeights,
#'                                     visWeights = visWeights,
#'                                     noOccur = 100,
#'                                     noLocations = seq(10, 100, by = 10),
#'                                     noVisits = 5
#' ))
#'
#'
#' }
#'
#' @export

#
# weightedDetection <- function(occWeights = data.frame("sites" = 1:10,
#                                                       "weights" = c(0.05,
#                                                                     0.05,
#                                                                     0.05,
#                                                                     0.05,
#                                                                     0.1,
#                                                                     0.1,
#                                                                     0.1,
#                                                                     0.1,
#                                                                     0.2,
#                                                                     0.2)
#                                                       ),
#                               visWeights = data.frame("sites" = 1:10,
#                                                       "weights" = c(0.05,
#                                                                     0.05,
#                                                                     0.05,
#                                                                     0.05,
#                                                                     0.1,
#                                                                     0.1,
#                                                                     0.1,
#                                                                     0.1,
#                                                                     0.2,
#                                                                     0.2)
#                               ),
#                               noOccur = 5,
#                               noLocations = 5,
#                               noVisits = 5,
#                               detectProb = 0.1,
#                               nIter = 9999,
#                               ...){
#
#   occWeights$weights <- occWeights$weights / sum(occWeights$weights)
#   visWeights$weights <- visWeights$weights / sum(visWeights$weights)
#
#
#             #Any occupied locations visited?
#             visitOccFun <- function(visWeights.,
#                                  occWeights.,
#                                  noLocations.,
#                                  noOccur.){
#
#               visited <- sample(visWeights.$sites, noLocations., prob = visWeights.$weights, replace = F)
#               occupied <- sample(occWeights.$sites, noOccur., prob = occWeights.$weights, replace = F)
#               ##any(visited %in% occupied)
#              propVisitedOccupied =  sum(visited %in% occupied)/noLocations.
#               return(propVisitedOccupied)
#
#
#               }
#
#
#           #How often do we visit occupied locations?
#           propMatchFun <- function(visWeights.,
#                                 occWeights.,
#                                 noLocations.,
#                                 noOccur.) {sum(replicate(nIter, visitOccFun(visWeights.,
#                                                                          occWeights.,
#                                                                          noLocations.,
#                                                                          noOccur.)))/nIter}
#
#
#         #Calculate for a set of number of locations visited
#          propMatch <- sapply(noLocations, function(x) propMatchFun(visWeights. = visWeights,
#                                                         occWeights. = occWeights,
#                                                         noLocations. = x,
#                                                         noOccur. = noOccur))
#
#
#           probObs <- 1 - (1 - propMatch * (1 - (1 - detectProb) ^ noVisits)) ^ noLocations
#
#           out <- tibble::tibble("noLocations" = noLocations,
#                         "probObs" = probObs)
#
#           class(out) <- c("detectProb", class(out))
#
#           return(out)
#
# }


weightedDetection <- function(occWeights = data.frame("sites" = 1:10,
                                                      "weights" = c(0.05,
                                                                    0.05,
                                                                    0.05,
                                                                    0.05,
                                                                    0.1,
                                                                    0.1,
                                                                    0.1,
                                                                    0.1,
                                                                    0.2,
                                                                    0.2)
),
visWeights = data.frame("sites" = 1:10,
                        "weights" = c(0.05,
                                      0.05,
                                      0.05,
                                      0.05,
                                      0.1,
                                      0.1,
                                      0.1,
                                      0.1,
                                      0.2,
                                      0.2)
),
noOccur = 5,
noLocations = 5,
noVisits = 5,
detectProb = 0.1,
nIter = 999){

  occWeights$weights <- occWeights$weights / sum(occWeights$weights)
  visWeights$weights <- visWeights$weights / sum(visWeights$weights)


  #Any occupied locations visited?
  visitOccFun <- function(visWeights.,
                          occWeights.,
                          noLocations.,
                          noOccur.){

    visited <- sample(visWeights.$sites, noLocations., prob = visWeights.$weights, replace = F)
    occupied <- sample(occWeights.$sites, noOccur., prob = occWeights.$weights, replace = F)
    ##any(visited %in% occupied)
    #propVisitedOccupied =  sum(visited %in% occupied)/noLocations.
    #return(propVisitedOccupied)

    noMatch <- sum(visited %in% occupied)

    obs <- rbinom(noMatch, noVisits, detectProb)
    any(obs > 0)

  }


  #How often do we visit occupied locations?
  propMatchFun <- function(visWeights.,
                           occWeights.,
                           noLocations.,
                           noOccur.) {sum(replicate(nIter, visitOccFun(visWeights.,
                                                                       occWeights.,
                                                                       noLocations.,
                                                                       noOccur.)))/nIter}


  #Calculate for a set of number of locations visited
  propMatch <- sapply(noLocations, function(x) propMatchFun(visWeights. = visWeights,
                                                            occWeights. = occWeights,
                                                            noLocations. = x,
                                                            noOccur. = noOccur))


  #probObs <- 1 - (1 - propMatch * (1 - (1 - detectProb) ^ noVisits)) ^ noLocations
  probObs <- propMatch
  out <- tibble::tibble("noLocations" = noLocations,
                        "probObs" = probObs)

  class(out) <- c("detectProb", class(out))

  return(out)

}


#' @export
plot.detectProb <- function(detectProb,
                            lineCol = "blue",
                            lwd = 1.5,
                            threshold = NULL){
  p <- ggplot(detectProb) +
    geom_line(aes(x = noLocations,
                  y = probObs),
              col = lineCol,
              lwd = lwd) +
    scale_x_continuous(name = "Number of visited locations",
                     breaks = detectProb$noLocations
                     ) +
    scale_y_continuous(name = "Observation probability",
                       limits = c(0, 1))

 if(!is.null(threshold)){
   p <- p +
     geom_hline(yintercept = threshold, lty = 2)
 }

  p

}


