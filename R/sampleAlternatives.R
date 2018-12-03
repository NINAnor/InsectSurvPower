#' sampleAlternatives
#'
#' Function to calculate possible regimes in terms of number of replicates, number of localities and number of replicates per locality.
#' This calculates possible survey regimes given the possible resampling times that results in a balanced data set. This hinges on only
#' two parameters, the total timespan of the entire survey, the yearly survey capacity of locations.
#'
#'
#' @param maxTime Total number of years of the study. Reasonably between 5 - 100 years.
#' @param maxCapacity Maximum number of localities possible to survey each year.
#' @param stepsCapacity Interval step size of number of localities per year. E.g. calculate possible regimes with 10, 20, and 30 number of localities per year. Defaults to maxCapacity.
#'
#' @export
#'
#' @return A tibble with potential survey regimes.

sampleAlternatives <- function(maxTime, maxCapacity, stepsCapacity = NULL){

  if(is.null(stepsCapacity)) {stepsCapacity = maxCapacity}


  r <- function(T, a, possible_t){
    l <- possible_t * a
    rLinear <- T + ((-1 + 1/T) / a) * l ## legacy. slope of end points, as if it was linear. Not so.
    r <- T / possible_t
    s <- T * a

      out <- dplyr::tibble(a = a, T = T, s = s, t = possible_t, rLinear = rLinear, reps = r, locations = l)
    return(out)
  }

  invmodulo <- function(maxTime){
    which(maxTime %% 1:maxTime == 0)
  }

  aSeries <- seq(stepsCapacity, maxCapacity, by = stepsCapacity)

  reps <- lapply(aSeries, FUN = r, T = maxTime,  possible_t = invmodulo(maxTime))
  names(reps) <- paste0("a = ", aSeries)

  df <- cbind(do.call(rbind, reps))

  df <- dplyr::as_tibble(df) %>%
    filter(reps > 0) %>%
    transmute("yearlyCapacity" = as.factor(a),
              "timespan" = as.factor(T),
              "totSamples" = as.factor(s),
              "resampleTime" = as.factor(t),
              "repsPerLocation" = as.integer(reps),
              "locations" = as.integer(locations)
  )

  class(df) <- c("surveyAlt", "tbl_df", "tbl", "data.frame")

return(df)
}
