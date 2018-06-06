#' sampleNorm
#'
#'
#' @param column Column to draw samples from.
#' @param gridCells Vector of which map grid cells to draw from, for repeated samples from the same grid cells. Incompatible with n.
#' @param nSites Integer, number of new random grids to sample. Imcompatible with rows.
#' @param sampleErr Double precision. Sampling error as standard deviation of a normally distributed variable.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#' @import sf
#' @import dplyr

sampleNorm <- function(map,
                      column,
                      subFylke = NULL,
                      subKommune = NULL,
                      gridCells = NULL,
                      nSites = NULL,
                      sampleErr = 0){

  if(!is.null(gridCells) & !is.null(nSites)) stop("Don't specify gridCells AND n at the same time.")

  sub <- map

  if(!is.null(subFylke)){
    sub <- sub %>%
      dplyr::filter(fylke %in% subFylke)
  }

  if(!is.null(subKommune)){
    sub <- sub %>%
      dplyr::filter(kommune %in% subKommune)
  }

  if(!is.null(gridCells)){
    sub <- sub %>% filter(ssbid %in% gridCells)
  } else {
    randomGridCells <- sample(unique(sub$ssbid), nSites, replace = F)
    sub <- sub %>%
      dplyr::filter(ssbid %in% randomGridCells)
  }


  sub <- sub %>%
    dplyr::mutate(nFound = rnorm(nrow(.), sampleErr, get(column)))

  return(sub)

}
