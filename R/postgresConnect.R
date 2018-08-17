#' postgresConnect
#'
#'
#'
#' @param user Username, defaults to guest account with basic read privileges
#' @param password Password Beware of storing this in cleartext in scripts. Consider storing it in a file that is included in gitignore.
#' @param host Name of database server. Defaults to (new) main PostgreSQL machine "gisdata-db.nina.no".
#'
#' @import DBI
#' @import RPostgres
#' @export

postgresConnect <- function(Username = "postgjest", Password = "gjestpost", host = "gisdata-db.nina.no"){

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it using devtools::install_github(\"rstats-db/DBI\") ",
         call. = FALSE)
  }


  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Pkg needed for this function to work. Please install it using devtools::install_github(\"rstats-db/RPostgres\") ",
         call. = FALSE)
  }


  tmp <- DBI::dbConnect(RPostgres::Postgres(), host = host, dbname = "gisdata", user = Username, password = Password)
  assign("con", tmp, .GlobalEnv)
}
