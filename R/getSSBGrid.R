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
#' @import dplyr

getSSBGrid <- function(conn = con, scale = c("10km","5km", "1km", "500m")){

  scale <- match.arg(scale, c("10km","5km", "1km", "500m"))
  ssb <- paste0("ssb_data_utm33n.ssb_", scale)

  query <- paste0("SELECT s.ssbid::varchar, s.xcoor lon, s.ycoor lat, s.geom,
f.\"FYLKESNUMMER\", f.navn fylke, k.\"KOMMUNENUMMER\", k.navn kommune,
a.\"ARTYPE\"
  FROM ", ssb," s,
  \"AdministrativeUnits\".\"Norway_County_Fylke_polygons\" f,
  \"AdministrativeUnits\".\"Norway_Municipality_Kommune_polygons\" k,
  \"Topography\".\"Norway_FKB_ar5_polygon\" a
  WHERE ST_Intersects(ST_Centroid(s.geom), f.geom)
  AND ST_Intersects(ST_Centroid(s.geom), k.geom)
  AND ST_Intersects(ST_Centroid(s.geom), a.geom)
  --ORDER BY s.ssbid
  --LIMIT 10")

  out <- st_read(dsn = conn, query = query, geom_column = "geom") %>%
    transform(ARTYPE = recode(ARTYPE,
                              `11` = "Bebygd",
                              `12` = "Samferdsel",
                              `21` = "Fulldyrka jord",
                              `22` = "Overflatedyrka jord",
                              `23` = "Innmarksbeite",
                              `30` = "Skog",
                              `50` = "Åpen fastmark",
                              `60` = "Myr",
                              `70` = "Isbre",
                              `81` = "Ferskvann",
                              `82` = "Hav",
                              `99` = "Ikke kartlagt")) %>%
    dplyr::as_tibble() %>%
    sf::st_as_sf()

   return(out)

}
