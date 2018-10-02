#' createOccPois
#'
#' Creates a lambda of species counts (for use in a Poisson draw) based on explanatory factors.
#' The levels are calculated additively with a fylke effect, a kommune effect, plus a random part (sigma) that
#' determines the intra-kommune variation among grid cells.
#' This is intended to fill a "map" with values of occurrences, and expects columns named fylke and kommune.
#'
#' @param formula Not yet implemented
#' @param sigmaFylke Standard deviance of fylkes
#' @param sigmaFylke Standard deviance of kommunes.
#' @param sigmaFylke Standard deviance of grids.
#' @param artypeEff Note that all parameters are input on the original scale (easier than using log scale)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#' @import sf
#' @import tidyverse


createOccPois <- function(map,
                          intercept = 22,
                          formula = ~ NULL,
                          sigmaFylke = 0.1,
                          sigmaKommune = 0.1,
                          sigmaGrid = 0,
                          sigmaFylkeTrend = 0,
                          sigmaKommuneTrend = 0,
                          artypeEff = c("Bebygd" = 1,
                                        "Samferdsel" = 1,
                                        "Fulldyrka jord" = 1,
                                        "Overflatedyrka jord" = 1,
                                        "Innmarksbeite" = 1,
                                        "Skog" = 1,
                                        "Åpen fastmark" = 1,
                                        "Myr" = 1,
                                        "Isbre" = 1,
                                        "Ferskvann" = 1,
                                        "Hav" = 1,
                                        "Ikke kartlagt" = 1
                          ),
                          artypeTrend = c("Bebygd" = 1,
                                          "Samferdsel" = 1,
                                          "Fulldyrka jord" = 1,
                                          "Overflatedyrka jord" = 1,
                                          "Innmarksbeite" = 1,
                                          "Skog" = 1,
                                          "Åpen fastmark" = 1,
                                          "Myr" = 1,
                                          "Isbre" = 1,
                                          "Ferskvann" = 1,
                                          "Hav" = 1,
                                          "Ikke kartlagt" = 1
                          ),
                          nYears = 5,
                          interceptTrend = 0.95,
                          sdInterceptTrend = 0,
                          sortFylke = T,
                          sortKommune = T,
                          sortGrid = T){


  intercept <- log(intercept)
  #sigmaKommune <- log(sigmaKommune)
  #sigmaFylke <- log(sigmaFylke)
  #sigmaGrid <- log(sigmaGrid)
  #sigmaFylkeTrend <- log(sigmaFylkeTrend)
  #sigmaKommuneTrend <- log(sigmaKommuneTrend)
  interceptTrend <- log(interceptTrend)
  #sdInterceptTrend <- log(sdInterceptTrend)
  artypeEff <- log(artypeEff)
  artypeTrend <-log(artypeTrend)

  #fylkeEff
  .fylkeEff <- dplyr::tibble(FYLKESNUMMER = unique(map$FYLKESNUMMER))
  .fylkeVals <- rnorm(nrow(.fylkeEff), 0, sigmaFylke)
  .fylkeTrends <- rnorm(nrow(.fylkeEff), 0, sigmaFylkeTrend)
  if(sortFylke){
    .fylkeVals <- sort(.fylkeVals)
    .fylkeTrends <- sort(.fylkeTrends)
  }
  .fylkeEff <- .fylkeEff %>%
    transform(fylkeEff = .fylkeVals,
              fylkeTrend = .fylkeTrends)
  map <- map %>%
    left_join(.fylkeEff, by = c("FYLKESNUMMER" = "FYLKESNUMMER"))

  ##kommuneEff
  .kommuneEff <- dplyr::tibble(KOMMUNENUMMER = unique(map$KOMMUNENUMMER))
  .kommuneVals <- rnorm(nrow(.kommuneEff), 0, sigmaKommune)
  .kommuneTrends <- rnorm(nrow(.kommuneEff), 0, sigmaKommuneTrend)
  if(sortKommune){
    .kommuneVals <- sort(.kommuneVals)
    .kommuneTrends <- sort(.kommuneTrends)
  }
  .kommuneEff <- .kommuneEff %>%
    transform(kommuneEff = .kommuneVals,
              kommuneTrend = .kommuneTrends)
  map <- map %>%
    dplyr::left_join(.kommuneEff, by = c("KOMMUNENUMMER" = "KOMMUNENUMMER"))

  #gridEff
  .gridVals <- rnorm(nrow(map), 0, sigmaGrid)
  if(sortGrid){
    .gridVals <- sort(.gridVals)
  }
  map <- map %>%
    transform(gridEff = .gridVals)


  ##artypeEff
  #use left_join to attach the artype values to the map
  artypeEff <- tibble(ARTYPE = names(artypeEff),
                      artypeEff = artypeEff)

  artypeTrend <- tibble(ARTYPE = names(artypeTrend),
                        artypeTrend = artypeTrend)

  map <- map %>%
    left_join(artypeEff, by = c("ARTYPE" = "ARTYPE"))

  map <- map %>%
    left_join(artypeTrend, by = c("ARTYPE" = "ARTYPE"))

  #sum the effects
  out <- map %>%
    transform(year = 1,
              lambda = exp(intercept + artypeEff + fylkeEff + kommuneEff + gridEff))

  if(nYears > 1){
    increment <- list()
    for(i in 2:nYears){
      increment[[(i-1)]] <- map %>%
        transform(year = i,
                  lambda = exp(intercept + rnorm(1, interceptTrend, sdInterceptTrend) * i + artypeEff + artypeTrend * i + fylkeEff + fylkeTrend * i + kommuneEff + kommuneTrend * i + gridEff)
        )

    }

    increments <- do.call(rbind, increment)

    combined <- rbind(out, increments) %>%
      dplyr::as_tibble()  %>%
      sf::st_as_sf()


  }

  combined <- combined %>%
    mutate(KOMMUNENUMMER = as.integer(KOMMUNENUMMER))

  out <- list()
  out$map <- combined

  out$params <- as.list(match.call())

  class(out) <- c("surveyHat", "list")

  return(out)


}
