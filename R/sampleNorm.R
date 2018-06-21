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
    dplyr::mutate(amount = rnorm(nrow(.), sampleErr, get(column)))

  return(sub)

}

#need to make check for large enough map (enough years)
sampleNorm2 <- function(map,
                        years = NULL,
                        yearlyCapacity = NULL,
                        resampleTime = NULL,
                        resampleWithin = c("none", "fylke", "kommune"),
                        column = "norm",
                        subFylke = NULL,
                        subKommune = NULL,
                        gridCells = NULL,
                        sampleErr = 0){

  resampleWithin <- match.arg(resampleWithin, c("none", "fylke", "kommune"))

  reSamp <- resampleTime
  sampleAlt <- sampleAlternatives(maxTime = years, maxCapacity = yearlyCapacity, stepsCapacity = yearlyCapacity)
  sampleAlt <- sampleAlt %>%
    filter(resampleTime == reSamp) %>%
    transmute_all(as.character) %>%
    transmute_all(as.integer)

  ##Update this!
  #if(!is.null(gridCells) & !is.null(nSites)) stop("Don't specify gridCells AND n at the same time.")

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

    stagger <- sapply(rep(0:(sampleAlt$resampleTime -1)),  FUN = function(x) (seq(from = min(sub$year) + x,
                                                                                  to = 2 * sampleAlt$timespan,
                                                                                  by = sampleAlt$resampleTime)))

    colnames(stagger) <- rep(0:(sampleAlt$resampleTime -1))

    stagger <- as_tibble(stagger) %>%
      gather(key = stagger,
             value = "selectYear") %>%
      transmute_all(as.integer) %>%
      filter(selectYear <= sampleAlt$timespan)

    noCells <- tibble("fylke" = length(unique(sub$fylke)),
                      "kommune" = length(unique(sub$kommune))
                      )
    randomGridCells <- sub %>%
      sf::st_set_geometry(NULL) %>%
      group_by(ssbid) %>%
      slice(1)

  if(resampleWithin != "none"){
    noCells <- noCells %>%
      select(resampleWithin)


    table <- randomGridCells %>%
      group_by(get(resampleWithin)) %>%
      tally() %>%
      select(n)

    if(as.integer(ceiling(sampleAlt$locations / noCells)) > min(table)){

    stop(paste0("Not enough grid cells to resample within ", resampleWithin,
                " with this sampling regime."))
    }

    randomGridCells <- randomGridCells %>%
      group_by(get(resampleWithin)) %>%
      sample_n(size = as.integer(ceiling(sampleAlt$locations / noCells)))

    #remove extra cells
    toRemove <- nrow(randomGridCells) - sampleAlt$locations
    removeFrom <- randomGridCells %>%
      slice(n()) %>%
      ungroup() %>%
      sample_n(size = toRemove)

    randomGridCells <- randomGridCells %>%
      filter(!(ssbid %in% removeFrom$ssbid))
  } else {

    randomGridCells <- randomGridCells %>%
      ungroup() %>%
      sample_n(size = sampleAlt$locations )

  }


    staggerLength <- rep(0:(sampleAlt$resampleTime -1), length.out = nrow(sub))

    randomGridCells <- randomGridCells %>%
      mutate(staggerInt = row_number())

    randomGridCells <- randomGridCells %>%
      ungroup() %>%
      mutate(stagger = staggerLength[staggerInt]) %>%
    left_join(stagger, by = c("stagger" = "stagger")) %>%
      select(ssbid,
             year = selectYear)


    sub <- sub %>%
      dplyr::inner_join(randomGridCells,
                        by = c("ssbid" = "ssbid",
                               "year" = "year"))

  }

  sub <- sub %>%
    dplyr::mutate(amount = rnorm(nrow(.), sampleErr, get(column)))

  return(sub)

}

