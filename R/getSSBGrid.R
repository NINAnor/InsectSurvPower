#' getSSBGrid
#'
#' Retrieves the SSB grids, together with fylke and kommune names (and ids) overlapping the center of each grid cell, as an sf object.
#'
#' @param scale Scale of SSB grid to retrieve. Options are 10km, 5km, 1km and 500m.
#' Note that smaller scales are time consuming!
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fylke10km <- getSSBGrid()
#' plot(fylke10km["fylke"])
#' }
#' @import sf
#' @import tidyverse

getSSBGrid <- function(conn = con, scale = c("10km","5km", "1km", "500m")){

  scale <- match.arg(scale)
  ssb <- paste0("ssb_data_utm33n.ssb_", scale)

  query <- paste0("SELECT s.ssbid, s.xcoor lon, s.ycoor lat, s.geom, f.\"FYLKESNUMMER\", f.navn fylke, k.\"KOMMUNENUMMER\", k.navn kommune
  FROM ", ssb," s,
  \"AdministrativeUnits\".\"Norway_County_Fylke_polygons\" f,
  \"AdministrativeUnits\".\"Norway_Municipality_Kommune_polygons\" k
  WHERE ST_Intersects(ST_Centroid(s.geom), f.geom)
  AND ST_Intersects(ST_Centroid(s.geom), k.geom)
  --ORDER BY s.ssbid
  --LIMIT 10")

  out <- st_read_db(conn = conn, query = query, geom_column = "geom") %>% as_tibble() %>% st_as_sf
  return(out)
}
