#' estimatePars
#'
#' Fits a model to the drawn survey data that estimates the population parameters and compares these to the true population parameters.
#'
#'
#' @param population a surveyHat object, containing the realised mean occurrences and population parameters
#' @param sampleFun which sampling function to use
#' @param samplePars an optional list of parameters for the sampling function, such as resampling times etc
#' @param model which model to fit. Currently only full models of three types is supported. Could perhaps be more flexible.
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
#' @import lme4


## Default är att köra samma modell, med kommuner och fylken som random effect, och jämföra varianser etc med parametrarna för bildandet
## av surveyHat.
## Alternativa modeller kan vara fylke och eller kommun som fixed effekt, och jämföra estimaten och osäkerhetena med de realiserade värdena i surveyHat



estimatePars <- function(map = NULL,
                         sampleFun = c("sampleNorm", "sampleBin", "samplePois"),
                         samplePars = list("yearlyCapacity" = 100),
                         model = c("normalFull", "binomialFull", "poissonFull"),
                         nIter = 1){

  map$map$ARTYPE <- as.factor(map$map$ARTYPE)
  #contrasts(map$map$ARTYPE)
  map$map$ARTYPE <- relevel(map$map$ARTYPE, "Skog")  ##This is to set the reference level to something that most often will be included

  map$map$fylke <- as.factor(map$map$fylke)
  map$map$fylke <- relevel(map$map$fylke, "Akershus")

  map$map$kommune <- as.factor(map$map$kommune)
  map$map$kommune <- relevel(map$map$kommune, "Aurskog-Høland")

  sampleFun <- match.arg(sampleFun, c("sampleNorm", "sampleBin", "samplePois"))

  model <- match.arg(model, c("normalFull", "binomialFull", "poissonFull"))

  sampleParsRun <- c(list("map" = map), samplePars)

  draw <- do.call(sampleFun, sampleParsRun)

  intercept <- rep(map$params$intercept, nrow(draw))

  if(model == "normalFull"){
    modelFun = lme4::lmer
    formula = as.formula(norm ~ 1 + year * ARTYPE + (year | fylke) + (year | kommune))

  modelPars <- list("data" = sf::st_set_geometry(draw, NULL),
                     "formula" = formula)
  }

  if(model == "poissonFull"){
    modelFun = lme4::glmer
    formula = as.formula(nCount ~ 1 + year * ARTYPE + (year | fylke) + (year | kommune))

    modelPars <- list("data" = sf::st_set_geometry(draw, NULL),
                      "formula" = formula,
                      "family" = poisson)
  }

  modelRes <- do.call(modelFun, modelPars)
  #Something funny about how this gets printed when running it through do.call. Maybe doesn't matter
  #hm <- lmer(modelPars$formula, data = modelPars$data)

  #summary(modelRes)

  #map$params



  fullParams <- c("(Intercept)" = as.numeric(paste0(as.character(map$params$intercept), collapse = "")),
                  "year" = as.numeric(paste0(as.character(map$params$interceptTrend), collapse = "")),
                  "ARTYPEÅpen fastmark" = as.numeric(paste0(as.character(map$params$artypeEff$`Åpen fastmark`), collapse = "")),
                  "ARTYPEBebygd" = as.numeric(paste0(as.character(map$params$artypeEff$Bebygd), collapse = "")),
                  "ARTYPEFerskvann" = as.numeric(paste0(as.character(map$params$artypeEff$Ferskvann), collapse = "")),
                  "ARTYPEFulldyrka jord" = as.numeric(paste0(as.character(map$params$artypeEff$`Fulldyrka jord`), collapse = "")),
                  "ARTYPEHav" = as.numeric(paste0(as.character(map$params$artypeEff$Hav), collapse = "")),
                  "ARTYPEIkke kartlagt" = as.numeric(paste0(as.character(map$params$artypeEff$`Ikke kartlagt`), collapse = "")),
                  "ARTYPEInnmarksbeite" = as.numeric(paste0(as.character(map$params$artypeEff$Innmarksbeite), collapse = "")),
                  "ARTYPEIsbre" = as.numeric(paste0(as.character(map$params$artypeEff$Isbre), collapse = "")),
                  "ARTYPEMyr" = as.numeric(paste0(as.character(map$params$artypeEff$Myr), collapse = "")),
                  "ARTYPEOverflatedyrka jord" = as.numeric(paste0(as.character(map$params$artypeEff$`Overflatedyrka jord`), collapse = "")),
                  "ARTYPESamferdsel" = as.numeric(paste0(as.character(map$params$artypeEff$Samferdsel), collapse = "")),
                  "ARTYPESkog" = as.numeric(paste0(as.character(map$params$artypeEff$Skog), collapse = "")),
                  "year:ARTYPEÅpen fastmark" = as.numeric(paste0(as.character(map$params$artypeTrend$`Åpen fastmark`), collapse = "")),
                  "year:ARTYPEBebygd" = as.numeric(paste0(as.character(map$params$artypeTrend$Bebygd), collapse = "")),
                  "year:ARTYPEFerskvann" = as.numeric(paste0(as.character(map$params$artypeTrend$Ferskvann), collapse = "")),
                  "year:ARTYPEFulldyrka jord" = as.numeric(paste0(as.character(map$params$artypeTrend$`Fulldyrka jord`), collapse = "")),
                  "year:ARTYPEHav" = as.numeric(paste0(as.character(map$params$artypeTrend$Hav), collapse = "")),
                  "year:ARTYPEIkke kartlagt" = as.numeric(paste0(as.character(map$params$artypeTrend$`Ikke kartlagt`), collapse = "")),
                  "year:ARTYPEInnmarksbeite" = as.numeric(paste0(as.character(map$params$artypeTrend$Myr), collapse = "")),
                  "year:ARTYPEIsbre" = as.numeric(paste0(as.character(map$params$artypeTrend$Isbre), collapse = "")),
                  "year:ARTYPEMyr" = as.numeric(paste0(as.character(map$params$artypeTrend$Myr), collapse = "")),
                  "year:ARTYPEOverflatedyrka jord" = as.numeric(paste0(as.character(map$params$artypeTrend$`Overflatedyrka jord`), collapse = "")),
                  "year:ARTYPESamferdsel" = as.numeric(paste0(as.character(map$params$artypeTrend$Samferdsel), collapse = "")),
                  "year:ARTYPESkog" = as.numeric(paste0(as.character(map$params$artypeTrend$Skog), collapse = "")))


  estHat <- lme4::fixef(modelRes)
  estSD <- sqrt(Matrix::diag(lme4::vcov.merMod(modelRes)))
  names(estSD) <- names(estHat)

  sortEstHat <- rep(NA, length = length(fullParams))
  names(sortEstHat) <- names(fullParams)
  sortEstHat[match(names(estHat), names(sortEstHat))] <- estHat

  sortEstSD <- rep(NA, length = length(fullParams))
  names(sortEstSD) <- names(fullParams)
  sortEstSD[match(names(estSD), names(sortEstSD))] <- estSD


  compList <- list("hat" = fullParams,
                  "estHat" = sortEstHat,
                  "lower" = sortEstHat - 1.96 * sortEstSD,
                  "upper" = sortEstHat + 1.96 * sortEstSD)

  if(nIter > 1){
    for(i in 2:nIter){
      draw <- do.call(sampleFun, sampleParsRun)

      if(model == "normalFull"){
        modelFun = lme4::lmer
        formula = as.formula(norm ~ 1 + year * ARTYPE + (year | fylke) + (year | kommune))

        modelPars <- list("data" = sf::st_set_geometry(draw, NULL),
                          "formula" = formula)
      }

      if(model == "poissonFull"){
        modelFun = lme4::glmer
        formula = as.formula(nCount ~ 1 + year * ARTYPE + (year | fylke) + (year | kommune))

        modelPars <- list("data" = sf::st_set_geometry(draw, NULL),
                          "formula" = formula,
                          "family" = poisson)
      }

      modelRes <- do.call(modelFun, modelPars)

      estHat <- lme4::fixef(modelRes)
      estSD <- sqrt(Matrix::diag(lme4::vcov.merMod(modelRes)))
      names(estSD) <- names(estHat)


      sortEstHat <- rep(NA, length = length(fullParams))
      names(sortEstHat) <- names(fullParams)
      sortEstHat[match(names(estHat), names(sortEstHat))] <- estHat

      sortEstSD <- rep(NA, length = length(fullParams))
      names(sortEstSD) <- names(fullParams)
      sortEstSD[match(names(estSD), names(sortEstSD))] <- estSD


      compList$estHat <- rbind(compList$estHat, sortEstHat)
      compList$lower <- rbind(compList$lower, sortEstHat - 1.96 * sortEstSD)
      compList$upper <- rbind(compList$upper, sortEstHat + 1.96 * sortEstSD)

    }



  }


  if(model == "poissonFull"){
    compList$estHat <- exp(compList$estHat)
    compList$lower <- exp(compList$lower)
    compList$upper <- exp(compList$upper)
    }

  class(compList) <- c("estimatePar", "List")

  return(compList)
}
