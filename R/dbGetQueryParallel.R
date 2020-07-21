#' A function to pull a large data table from SQL Server to R in parallel chunks
#' This function relies on the parallel package which uses a fork / exec mechanism.
#' Therefore, it is important that the parent process not have an initialized JVM if using rJDBC
#' for database connectivity, as the child processes will see it and attempt to use it.
#'
#' @param dbConnectionFun The function to be called to instantiate a database connection
#' @param dbConnectionArgs The values of the parameters (in a named list) to be passed to dbConnectionFun to instantiate a connection
#' @param tableName The name of the table to be pulled from the database
#' @param orderByClause The ORDER BY clause that will be used to determine the order in which
#' rows are fetched.  This MUST define a total ordering on the rows of the table, 
#' or the behavior of the FETCH operation will not be predictable.
#' @param nThreads The number of parallel jobs to be used to fetch the table
#' @keywords sql
#' @export

dbGetQueryParallel <- function (dbConnectionFun,
						dbConnectionArgs,
						tableName, 
						orderByClause,
						nThreads=8) {
	
	## get the number of rows in the target table
	nRowsTotal = as.numeric(unlist(SqlTools::dbGetQueryForkExec(
		dbConnectionFun,
		dbConnectionArgs,
		statement = paste(sep="", "SELECT COUNT(*) FROM ", tableName),
		async = FALSE)))

	## this function takes an integer (job / thread number), determines which block of 
	## the table it should pull, constructs the SQL statement to pull that chunk, and
	## calls the asynchronous code to do the actual work
	f <- function (i) {
	
		## based on requested number of threads, figure out how many rows this process should pull
		nRows = ceiling(nRowsTotal / nThreads)
		sliceStartOffset = ((i-1) * nRows)

		## construct the query to grab this block of rows
		query = paste(sep="", "
			SELECT 
				* 
			FROM 
				", tableName, " 
		  ", orderByClause, " 
			OFFSET ", format(scientific=FALSE, sliceStartOffset), " ROWS 
				FETCH NEXT ", format(scientific=FALSE, nRows), " ROWS ONLY"
		)
		
		## run the query in another process and block on the result
		return(
			dbGetQueryForkExec(
				dbConnectionFun,
				dbConnectionArgs,
				statement=query,
				async=FALSE
			)
		)
	}
	
	## run nThreads separate jobs
	z = lapply(FUN=f, X=1:nThreads)
	return(z)
}
