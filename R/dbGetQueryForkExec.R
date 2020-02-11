#' A function that uses the parallel package to execute a SQL statement in a separate process,
#' thereby avoiding creating a JVM in the current process.
#'
#' @param database The name of the database to connect to
#' @param user The name of the user for the database
#' @param password The database user's password
#' @param server.address The network address (DNS or IP) of the database server
#' @param domain The Windows domain for the specified users
#' @param statement The SQL statement to be issued in a dbGetQuery(...) call
#' @keywords sql
#' @export

dbGetQueryForkExec <- function (
	database, 
	user,
	password,
	server.address,
	domain,
	statement) {
	
	require(parallel)
	
	params = list(
		"database"=database,
		"user"=user,
		"password"=password,
		"server.address"=server.address,
		"domain"=domain,
		"statement"=statement)
	
	param.list = list()
	param.list[[1]] = params
	p=mcparallel(doDbGetQuery(params))
	z = mccollect(p)
	return(z)
}
