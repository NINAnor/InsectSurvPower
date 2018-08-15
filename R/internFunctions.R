#' Internal functions
#'
#'
#'

checkCon <- function() {if(!exists("con")){ stop("No connection, run postgresConnect()")} else{
  if(class(con)!= "PqConnection"){ stop("\"con\" is not of class \"PqConnection\". Have you run postgresConnect()?")}
  if(!DBI::dbIsValid(con)) { stop("No connection, run postgresConnect()")}
}
}
