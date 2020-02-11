#' A function to pull a large data table from SQL Server to R in parallel chunks
#' This function relies on the parallel package which uses a fork / exec mechanism.
#' Therefore, it is important that the parent process not have an initialized JVM,
#' as the child processes will see it and attempt to use it.
#'
#' @param database The name of the database to connect to
#' @param user The name of the user for the database
#' @param password The database user's password
#' @param server.address The network address (DNS or IP) of the database server
#' @param domain The Windows domain for the specified users
#' @param table.name The name of the table to be pulled from the database
#' @param order.by.clause The ORDER BY clause that will be used to determine the order in which
#' rows are fetched.  This MUST contain a column or combination of columns that are guaranteed have
#' unique values for every row, or the behavior of the FETCH operation may not be predictable.
#' @param n.threads The number of parallel jobs to be used to fetch the table
#' @keywords sql
#' @export

threaded.chunked.query <- function (
	database, 
	user,
	password,
	server.address,
	domain,
	table.name, 
	order.by.clause,
	n.threads=8) {
	
	require(parallel)
	
	## package params up in a list of vectors to pass to each subprocess
	param.list = list()
	
	for (i in 1:n.threads) {
	
		param.list[[i]] = list(
			"ix"=i,
			"database"=database,
			"user"=user,
			"password"=password,
			"server.address"=server.address,
			"domain"=domain,
			"table.name"=table.name,
			"order.by.clause"=order.by.clause,
			"n.threads"=n.threads
		)
	}
	
	## each subprocess will return the value of calling this function on its argument
	f <- function (params) {
	
		library(SqlServerJtds)
		library(SqlTools)
		
		## connect this process to the DB server
		cn = connect.sql.server(
			database=params$database, 
			user=params$user, 
			password=params$password, 
			server.address=params$server.address, 
			domain=params$domain)
		
		## figure out how many rows we need to pull
		n.rows.total = as.numeric(unlist(dbGetQuery(cn, paste(sep="", "SELECT COUNT(*) FROM ", params$table.name))))

		## based on requested number of threads, figure out how many rows this process should pull
		n.rows = ceiling(n.rows.total / params$n.threads)
		slice.start.offset = ((params$ix-1) * n.rows)

		query = paste(sep="", "
			SELECT 
				* 
			FROM 
				", params$table.name, " 
		  ", params$order.by.clause, " 
			OFFSET ", format(scientific=FALSE, slice.start.offset), " ROWS 
				FETCH NEXT ", format(scientific=FALSE, n.rows), " ROWS ONLY"
		)
		
		#cat(query, "\n")
		#cat(sep="", "Retrieving slice ", params$ix, " of ", params$n.threads, " (", format(scientific=FALSE, n.rows), " rows)", "\n")
		return(dbGetQuery(cn, query))
	}
	
	z = mclapply(FUN=f, X=param.list, mc.cores=n.threads)
	return(z)
}
