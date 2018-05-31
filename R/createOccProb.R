#' createOccProb
#'
#' Creates a probability of species occurrence based on explanatory factors.
#' The levels are calculated additively with a fylke effect, a kommune effect, plus a random part (sigma) that
#' determines the intra-kommune variation among grid cells.
#' This is intended to fill a "map" with values of occurrences, and expects columns named fylke and kommune.
#'
#' @param formula Not yet implemented
#' @param sigmaFylke Standard deviance of fylkes
#' @param sigmaFylke Standard deviance of kommunes.
#' @param sigmaFylke Standard deviance of grids.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#' @import sf
#' @import tidyverse


createOccProb <- function(map,
                          intercept = 0.5,
                          formula = ~ NULL,
                          sigmaFylke = 0.1,
                          sigmaKommune = 0.1,
                          sigmaGrid = 0,
                          sortFylke = T,
                          sortKommune = T,
                          sortGrid = T) {

  #range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  #formula <- as.formula(formula)
  # formulaEff <- map %>%
  #   select(all.vars(formula)) %>%
  #   as.matrix() %>%
  #   range01() %*% fylkeParams


  #fylkeEff
  .fylkeEff <- dplyr::tibble(FYLKESNUMMER = unique(map$FYLKESNUMMER))
  .fylkeVals <- rnorm(nrow(.fylkeEff), 0, sigmaFylke)
  if(sortFylke){
    .fylkeVals <- sort(.fylkeVals)
  }
  .fylkeEff <- .fylkeEff %>%
    transform(fylkeEff = .fylkeVals)
  map <- map %>%
    left_join(.fylkeEff, by = c("FYLKESNUMMER" = "FYLKESNUMMER"))

  ##kommuneEff
  .kommuneEff <- dplyr::tibble(KOMMUNENUMMER = unique(map$KOMMUNENUMMER))
  .kommuneVals <- rnorm(nrow(.kommuneEff), 0, sigmaKommune)
  if(sortKommune){
    .kommuneVals <- sort(.kommuneVals)
  }
  .kommuneEff <- .kommuneEff %>%
    transform(kommuneEff = .kommuneVals)
  map <- map %>%
    dplyr::left_join(.kommuneEff, by = c("KOMMUNENUMMER" = "KOMMUNENUMMER"))

  #gridEff
  .gridVals <- rnorm(nrow(map), 0, sigmaGrid)
  if(sortGrid){
    .gridVals <- sort(.gridVals)
  }
  map <- map %>%
    transform(gridEff = .gridVals)

  ##NEED TO LIMIT THE TOTAL EFFECT TO BTW 0 AND 1
  #sum the effects
  map <- map %>%
    transform(out = intercept + fylkeEff + kommuneEff + gridEff)

  return(map$out)
}
