#' plotSurveyAlt
#'
#' Plot function for
#'
#'
#'
#' @export




plot.surveyAlt <- function(df,
                   grouping = "yearlyCapacity",
                   color = c("yearlyCapacity", "totSamples", "resampleTime"),
                   allTicks = T){

  color <- match.arg(color)
  p <- ggplot2::ggplot(df, ggplot2::aes(locations, repsPerLocation, group = get(grouping), color = get(color))) +
    ggplot2::geom_point(ggplot2::aes(shape = get(grouping)),
               size = 2.5) +
    ggplot2::geom_line(lty = 1,
              lwd = 0.5) +
    ggplot2::ylab("Replicates per location") +
    ggplot2::xlab("Number of locations") +
    ggplot2::ggtitle(paste0("Number of possible locations \nand replicates per location \nwith staggered sampling in ", df$timespan, " years.")) +
    ggplot2::labs(colour = color) +
    ggplot2::labs(shape = grouping)

  if(allTicks){
    p + ggplot2::scale_x_continuous(breaks = unique(df$locations)) +
      ggplot2::scale_y_continuous(breaks = unique(df$repsPerLocation))
  } else p

}
