#' obsProb
#'
#' Function that produces empirical estimates of probability to detect a foreign species, and associated costs
#' based on varying occurrences, detectabilities and survey schemes.
#'
#'
#'
#' @param occProb vector of occurence probabilities to test
#' @param detectProb vector of detection probabilities
#' @param locations vector of number of locations to visit/sample
#' @param visits vector of number of visits per sample
#' @param visitCost vector of costs of each visit
#' @param nBoot number of bootstrap samples to calculate std error of percent detected
#'
#'
#' @return Returns a tibble where percDetected shows in percentage, how many times it is expected to find at least one record of the species,
#' given the occurrence probabilities in each location, the detection probabilities if it is present, the number of locations sampled, and the
#' number of samples per location. It also shows the standard error on the estimated percentage detected, based on the bootstrap
#'
#'
#' @export
#' @examples{
#' tt <- detectProp(occProb = seq(0.01, 0.9, by = 0.05),
#' detectProb = 0.8,
#' locations = 30,
#' visits =  4,
#' visitCost = 10,
#' nBoot =  9999)
#'
#' }
#'

obsProb <- function(occProb,
                       detectProb,
                       locations,
                       visits,
                       visitCost){

  calc1 <- function(occProb,
                    detectProb,
                    locations,
                    visits,
                    visitCost){


    # detectFun <- function(occProb,
    #                       detectProb,
    #                       visits){
    #   occur <- rbinom(n = locations, size = 1, prob = occProb)
    #   detect <- rbinom(n = occur, size = visits, prob = occur * detectProb)
    #   out <- as.integer(sum(detect) > 0)
    #   return(out)
    # }

    #reps <- replicate(n = nBoot,
     #         detectFun(occProb = occProb,
      #                     detectProb = detectProb,
       #                    visits = visits)
        #      )


    out <- dplyr::tibble(
      occProb = occProb,
      detectProb = detectProb,
      locations = locations,
      visits = visits,
      visitCost = visitCost,
      #probObserv = sum(reps) / length(reps),
      #std.error = sqrt((probObserv * (1 - probObserv)) / length(reps)),
      obsProb = (1 - (1 - occProb)^locations) * (1 - (1 - detectProb)^visits),
      totCost = sum(locations * visits * visitCost)
    )

    class(out) <- c("obsProb", "
                    tbl_df", "tbl", "data.frame")
    return(out)
  }


input <- data.frame(expand.grid(occProb = occProb,
                    detectProb = detectProb,
                    locations = locations,
                    visits = visits,
                    visitCost = visitCost))


out <- do.call(rbind.data.frame, apply(input, 1,  function(x) calc1(x[1], x[2], x[3], x[4], x[5])))

# out$occProb <- as.factor(out$occProb)
# out$detectProb <- as.factor(out$detectProb)
# out$locations <- as.factor(out$locations)
# out$visits <- as.factor(out$visits)
# out$visitCost <- as.factor(out$visitCost)

return(out)

}


#' @export
plot.obsProb <- function(input,
                            group,
                            yVar,
                            xVar,
                            titleVar = NULL,
                            hline = NULL){

  ##match args here
  input[[group]] <- as.factor(input[[group]])

   p <- input %>%
    ggplot2::ggplot(.) +
     ggplot2::geom_line(ggplot2::aes(x = get(xVar),
                  y = get(yVar),
                  color = get(group),
                  group = get(group)
                  ),
                  lwd = 1.5
              ) +
     ggplot2::scale_colour_discrete(name = group) +
     ggplot2::xlab(xVar) +
     ggplot2::ylab(yVar)


   if(!is.null(titleVar)){
   titleSect <- list()
   for(i in 1:length(titleVar)){
     titleSect[i] <- paste(titleVar[i], "=", mean(input[[titleVar[i]]]))
   }

   title <- paste(unlist(titleSect), collapse = ", ")

   p <- p + ggplot2::ggtitle(label = title)
   }

   if(!is.null(hline)){
     p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed" , color = "black")

   }

   # if(confBands){
   #   p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = get(yVar) - 1.96 * std.error,
   #                                              ymax = get(yVar) + 1.96 * std.error,
   #                                              x = get(xVar),
   #                                              group = get(group)),
   #                                   linetype=2,
   #                                   alpha=0.1)
   # }

  p
}

