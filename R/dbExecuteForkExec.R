#' A function that uses the parallel package to execute a SQL statement in a subprocess.  If async=FALSE, it 
#' returns the number of rows affected if the statement was successfully executed, or errors if not.  If async=TRUE,
#' it asyncronously returns an object of the class parallelJob that can be used to determine job status via parallel::mccollect.
#'
#' @param dbConnectionFun The function to be called to instantiate a database connection
#' @param dbConnectionArgs The values of the parameters (in a named list) to be passed to dbConnectionFun to instantiate a connection
#' @param statement The SQL statement to be executed
#' @param async If FALSE, the function blocks, waiting for the subprocess to complete. If TRUE, the 
#' function asyncronously returns an object of the class parallelJob that can be used to determine job status via parallel::mccollect
#' @keywords sql
#' @export

dbExecuteForkExec <- function (
	dbConnectionFun,
	dbConnectionArgs,
	statement,
	async=FALSE) {
	
	## a function that takes instructions on how to create a DB connection,
	## then uses the connection to run the SQL statement.
	## this is just a mechanism to get the parallel package to fork / exec for us.
	f <- function(dbConnectionFun, dbConnectionArgs, statement) {
		
		cn <- do.call(what=dbConnectionFun, args=dbConnectionArgs)
		x = DBI::dbExecute(cn, statement, immediate = TRUE)
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
