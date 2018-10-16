#' sampleNorm
#'
#' @param map a surveyHat object (data frame containing ssbids fylke and kommune colums, response colum and potential other grouping columns)
#' @param nYears the number of years of the survey. Note that you need at least two years to have one revisit. Defaults to the timespan of the map.
#' @param yearlyCapacity the total yearly survey capacity.
#' @param resampleTime how many years between revisits? Defaults to 1 for yearly revisits.
#' @param resampleWithin within which region (or other category) should the revisits be distributed.
#' Ensures even sampling schemes in all regions. Note that all regions may get fewer locations if the yearlyCapacity and number of regions don't match up.
#' @param column Column to draw samples from. Defaults to "norm" for normally distributed variables.
#' @param subFylke optionally limit the draws to set of fylkes. Character vector.
#' @param subKommune optionally limit the drawn cells to a set of kommunes. Character vector.
#' @param gridCells Vector of which map grid cells to draw from, for repeated samples from the same grid cells. Overrides arguments associated
#' with random draws of map cells.
#' @param sampleErr Double precision. Sampling error as standard deviation of a normally distributed variable. (Only applicable to normal draws.)
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#' @import sf
#' @import tidyverse

sampleNorm <- function(map,
                        nYears = NULL,
                        yearlyCapacity = NULL,
                        resampleTime = 1,
                        resampleWithin = "none",
                        column = "norm",
                        subFylke = NULL,
                        subKommune = NULL,
                        gridCells = NULL,
                        sampleErr = 0){

  map <- map$map

  resampleWithin <- match.arg(resampleWithin, c(names(map), "none"))

  if(is.null(nYears)){
    nYears <- max(map$year)
  }

  reSamp <- resampleTime
  sampleAlt <- sampleAlternatives(maxTime = nYears, maxCapacity = yearlyCapacity, stepsCapacity = yearlyCapacity)
  if(!(resampleTime %in% sampleAlt$resampleTime)){
    stop(paste0("ResamplingTime needs to be one of ",
                paste(sampleAlt$resampleTime, collapse = ", "),
                " for a time period of ",
                nYears,
                " and yearly capacity of ",
                yearlyCapacity))
  }
  sampleAlt <- sampleAlt %>%
    filter(resampleTime == reSamp) %>%
    transmute_all(as.character) %>%
    transmute_all(as.integer)


  if(!is.null(gridCells) & resampleWithin != "none") stop("Don't specify gridCells AND resampleWithin at the same time.")
  if(!is.null(gridCells) & !is.null(subFylke)) stop("Don't specify gridCells AND subFylke at the same time.")
  if(!is.null(gridCells) & !is.null(subKommune)) stop("Don't specify gridCells AND subKommune at the same time.")

  yearsInMap <- map %>%
    sf::st_set_geometry(NULL) %>%
    select(year) %>%
    distinct() %>%
    nrow

  if(yearsInMap < nYears) stop(paste("surveyHat input (map) doesn't contain enough years to sample", nYears, "years."))


  sub <- map

  if(!is.null(subFylke) & is.null(gridCells)){
    sub <- sub %>%
      dplyr::filter(fylke %in% subFylke)
  }

  if(!is.null(subKommune) & is.null(gridCells)){
    sub <- sub %>%
      dplyr::filter(kommune %in% subKommune)
  }

  if(!is.null(gridCells)){
    sub <- sub %>% filter(ssbid %in% gridCells)
  }

    stagger <- sapply(rep(0:(sampleAlt$resampleTime -1)),  FUN = function(x) (seq(from = min(sub$year) + x,
                                                                                  to = 2 * sampleAlt$timespan,
                                                                                  by = sampleAlt$resampleTime)))

    colnames(stagger) <- rep(0:(sampleAlt$resampleTime -1))

    stagger <- as_tibble(stagger) %>%
      tidyr::gather(key = stagger,
             value = "selectYear") %>%
      transmute_all(as.integer) %>%
      filter(selectYear <= sampleAlt$timespan)


    allGridCells <- sub %>%
      sf::st_set_geometry(NULL) %>%
      group_by(ssbid) %>%
      slice(1)

    selectedGridCells <- allGridCells

  if(resampleWithin != "none" & is.null(gridCells)){

    noCells <- sub %>%
      sf::st_set_geometry(NULL) %>%
      select(resampleWithin) %>%
      distinct() %>%
      nrow()

    table <- allGridCells %>%
      group_by(get(resampleWithin)) %>%
      tally() %>%
      select(n)

    if(as.integer(ceiling(sampleAlt$locations / noCells)) > min(table)){

    stop(paste0("Not enough grid cells to resample within ", resampleWithin,
                " with this sampling regime."))
    }

    randomGridCells <- allGridCells %>%
      group_by(get(resampleWithin)) %>%
      sample_n(size = as.integer(ceiling(sampleAlt$locations / noCells)))

    #remove extra cells
    toRemove <- nrow(randomGridCells) - sampleAlt$locations
    removeFrom <- randomGridCells %>%
      slice(n()) %>%
      ungroup() %>%
      sample_n(size = toRemove)

    selectedGridCells <- randomGridCells %>%
      filter(!(ssbid %in% removeFrom$ssbid))
  }

  if(resampleWithin == "none" & is.null(gridCells)) {

    randomGridCells <- allGridCells %>%
      ungroup() %>%
      sample_n(size = sampleAlt$locations )

    selectedGridCells <- randomGridCells

  }


    staggerLength <- rep(0:(sampleAlt$resampleTime -1), length.out = nrow(sub))

    selectedGridCells <- selectedGridCells %>%
      mutate(staggerInt = row_number())

    selectedGridCells <- selectedGridCells %>%
      ungroup() %>%
      mutate(stagger = staggerLength[staggerInt]) %>%
    left_join(stagger, by = c("stagger" = "stagger")) %>%
      select(ssbid,
             year = selectYear)


    sub <- sub %>%
      dplyr::inner_join(selectedGridCells,
                        by = c("ssbid" = "ssbid",
                               "year" = "year"))


  sub <- sub %>%
    dplyr::mutate(amount = rnorm(n = nrow(.), mean = get(column), sd = sampleErr))

  return(sub)

}
