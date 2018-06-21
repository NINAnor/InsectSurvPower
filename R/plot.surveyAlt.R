#' plotSurveyAlt
#'
#' Plot function for
#'
#'
#'
#'




plot.surveyAlt <- function(df,
                   grouping = "yearlyCapacity",
                   color = c("yearlyCapacity", "totSamples", "resampleTime"),
                   allTicks = T){

  color <- match.arg(color)
  p <- ggplot(df, aes(locations, repsPerLocation, group = get(grouping), color = get(color))) +
    geom_point(aes(shape = get(grouping)),
               size = 2.5) +
    geom_line(lty = 1,
              lwd = 0.5) +
    ylab("Replicates per location") +
    xlab("Number of locations") +
    ggtitle(paste0("Number of possible locations \nand replicates per location \nwith staggered sampling in ", df$timespan, " years.")) +
    labs(colour = color) +
    labs(shape = grouping)

  if(allTicks){
    p + scale_x_continuous(breaks = unique(df$locations)) +
      scale_y_continuous(breaks = unique(df$repsPerLocation))
  } else p

}
