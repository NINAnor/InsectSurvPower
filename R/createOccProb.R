#' createOccProb
#'
#' Creates a probability of species occurrence based on explanatory factors.
#' The levels are calculated additively with a fylke effect, a kommune effect, plus a random part (sigma) that
#' determines the intra-kommune variation among grid cells.
#' This is intended to fill a "map" with values of occurrences, and expects columns named fylke and kommune.
#'
#' @param formula
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
                          intercept = 0.1,
                          formula = ~ lat + lon,
                          fylkeParams = c(0.1, 0.4),
                          kommuneParams = c(0.1, 0.4),
                          sigma = 0,
                          scale = T) {

  range01 <- function(x){(x-min(x))/(max(x)-min(x))}

  formula <- as.formula(formula)

  fylkeEff <- map %>%
    select(all.vars(formula)) %>%
    as.matrix() %>%
    range01() %*% fylkeParams

  kommuneEff <- map %>%
    select(all.vars(formula)) %>%
    as.matrix() %>%
    range01() %*% kommuneParams

  out <- intercept + fylkeEff + kommuneEff + rnorm(nrow(map), 0, sd = sigma)

  return(out)
}
