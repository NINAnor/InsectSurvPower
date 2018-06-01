#' sampleBin
#'
#'
#' @param column Column to draw samples from.
#' @param rows Vector of which map grid cells (row numbers) to draw from, for repeated samples from the same grid cells. Incompatible with n.
#' @param nSites Integer, number of new random grids to sample. Imcompatible with rows.
#' @param nObs Number of observations to draw. Can be a single integer or a vector of integers. This will be coerced to integer(s).
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

sampleBin <- function(map,
                      column,
                      subFylke = NULL,
                      subKommune = NULL,
                      rows = NULL,
                      nSites = NULL,
                      nObs = 1){

  if(!is.null(rows) & !is.null(n)) stop("Don't specify rows AND n at the same time.")
  nObs <- as.integer(nObs)

  sub <- map

  if(!is.null(subFylke)){
    sub <- sub %>%
      filter(fylke %in% subFylke)
  }

  if(!is.null(subKommune)){
    sub <- sub %>%
      filter(kommune %in% subKommune)
  }

  if(!is.null(rows)){
    sub <- sub[rows, ]
  } else {
  sub <- sub %>%
    dplyr::sample_n(nSites, replace = F)
  }


  sub <- sub %>%
    dplyr::mutate(nVisits = nObs,
                  nFound = rbinom(nrow(.), nObs, get(column)))

  return(sub)

}
