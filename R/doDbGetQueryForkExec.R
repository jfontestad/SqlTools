#' A helper function that is called by the fork'd child process to execute the SQL statement.
#'
#' @keywords sql

doDbGetQuery <- function (params) {

	require(SqlServerJtds)
	
	cn = connect.sql.server(
		database=params$database, 
		user=params$user, 
		password=params$password, 
		server.address=params$server.address, 
		domain=params$domain)
	
	x = dbGetQuery(cn, params$statement)
	return(x)
}
