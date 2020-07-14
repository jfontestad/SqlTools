#' A function that executes a SQL statement that does not return a resultset.
#'
#' @param cn The database connection to use
#' @param stmt The SQL statemment to be executed
#' @keywords SQL Server ODBC
#' @export

dbSendUpdate <- function(cn, stmt) {
    rs <- dbSendQuery(cn, stmt)
    dbClearResult(rs)
}
