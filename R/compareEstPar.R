#' compareEstPar
#'
#' Compare estimated parameters with the simulated "truth"
#'
#' @param estimatePars an estimatePar object
#'
#'
#' @export
#'

compareEstPar <- function(pars){
  if(!("estimatePar" %in% class(pars))) stop("The input object needs to be of 'estimatePar' class")

  TypeM  <- t(apply(pars$lower, 1,  `>=`, pars$hat)) | t(apply(pars$upper, 1,  `<=`, pars$hat))

  TypeS <- t(apply(sign(pars$estHat), 1, function(x) x != sign(pars$hat) & pars$hat != 0))

  TypeI <- t(apply(t(apply(pars$lower, 1,  `>=`, 0) | apply(pars$upper, 1,  `<=`, 0)), 1, function(x) x & pars$hat == 0))

  TypeII <- t(apply(t(apply(pars$lower, 1,  `<=`, 0) & apply(pars$upper, 1,  `>=`, 0)), 1, function(x) x & pars$hat != 0))



  perFunc <- function(x) sum(x, na.rm = T) / sum(!is.na(x))

  TypeIErr <-  apply(TypeI, 2, perFunc)
  TypeIIErr <- apply(TypeII, 2, perFunc)
  TypeSErr <- apply(TypeS, 2, perFunc)
  TypeMErr <- apply(TypeM, 2, perFunc)

  return(list("TypeIErr" = TypeIErr,
              "TypeIIErr" = TypeIIErr,
              "TypeSErr" = TypeSErr,
              "TypeMErr" = TypeMErr))


}

#compareEstPar(tt)
