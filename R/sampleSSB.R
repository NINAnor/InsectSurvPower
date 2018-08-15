#' sampleSSB
#'
#' Function to sample a set of ssb grid cells (for later sampling), based on various criteria.
#'
#'
#' @param ssbScale Which SSB grid cell size to sample from? Options are (10 km, 1 km, 500 m) Default = 1 km.
#' @param focusArealtype Which AR5 type to sample from? Defaults to bebygd (11).
#' @param conditions Additional SQL conditions
#' @param weights An optional raster layer that weighs the sampling. Defaults to NULL
#' @param totSamples The number of samples to draw. Integer, defaults to 100
#'
#' @return
#'
#'
#' @export
#' @examples{
#'
#'
#'
#' }
#'

sampleSSB <- function(){

  checkCon()

  sampleQ <- "
  SELECT ssb.ssbid, ar5.\"ARTYPE\", ssb.geom, COALESCE(ST_value(rast, ST_Centroid(ssb.geom)), 0) prob
  FROM \"Topography\".\"Norway_FKB_ar5_polygon\" ar5, ssb_data_utm33n.ssb_1km_population_2014 ssb,
  hotspot_ias.evenintbigpred1km alien
  WHERE ST_DWithin(ssb.geom, ar5.geom, 100)
  AND ST_Intersects(ST_Centroid(ssb.geom), alien.rast)
  AND ar5.\"ARTYPE\" = 11
  "

  prelSamp <- dbGetQuery(con, sampleQ)



}
