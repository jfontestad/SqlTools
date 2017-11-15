#' A helper function that is called by the fork'd child process to execute the SQL statement.
#'
#' @keywords sql

doDbSendUpdate <- function (params) {

	require(SqlServerJtds)
	
	cn = connect.sql.server(
		database=params$database, 
		user=params$user, 
		password=params$password, 
		server.address=params$server.address, 
		domain=params$domain)
	
	dbSendUpdate(cn, params$statement)
	return()
}
