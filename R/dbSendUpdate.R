#' A function that executes a SQL statement that does not return a resultset.
#' Uses DBI "immediate" execution option where available so that e.g. SQL Server
#' temp tables are created in the same session.
#'
#' @param cn The database connection to use
#' @param statement The SQL statemment to be executed
#' @keywords SQL Server ODBC
#' @export

dbSendUpdate <- function(cn, statement) {
    DBI::dbExecute(cn, statement, immediate = TRUE)
}
