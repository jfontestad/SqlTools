#' A function that uses the parallel package to execute a SQL statement in a separate process.
#'
#' @param dbConnectionFun The function to be called to instantiate a database connection
#' @param dbConnectionArgs The values of the parameters (in a named list) to be passed to dbConnectionFun to instantiate a connection
#' @param statement The SQL statement to be executed
#' @param async If FALSE, then the function blocks and waits for the subprocess to complete, and returns the result of the query.  If TRUE, 
#' the function asyncronously returns an object of the class parallelJob that can be used to collect the results via parallel::mccollect
#' @keywords sql
#' @export

dbGetQueryForkExec <- function (
	dbConnectionFun,
	dbConnectionArgs,
	statement,
	async = FALSE) {
	
	## a simple function that takes instructions on how to create a DB connection,
	## then uses the connection to run the SQL statement, and returns the result.
	## this is just a mechanism to get the parallel package to fork / exec for us.
	f <- function(dbConnectionFun, dbConnectionArgs, statement) {
		
		cn <- do.call(what=dbConnectionFun, args=dbConnectionArgs)
		x = DBI::dbGetQuery(cn, statement)
		DBI::dbDisconnect(cn)
		return(x)
	}

	## call the parallel package to run the query in a subprocess
	p = parallel::mcparallel(f(dbConnectionFun, dbConnectionArgs, statement))

	if (async) {
		return(p)
	} else {
		z = parallel::mccollect(p)[[1]]
		return(z)
	}
}
