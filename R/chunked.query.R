#' A function to pull a large data table from SQL Server to R in chunks,
#' as RJDBC has problems pulling very large tables using the standard
#' dbGetQuery(...) approach. This function splits the table up into logical
#' chunks that are pulled one at a time.  The specific technique for splitting
#' up the calls using OFFSET ... ROWS FETCH NEXT ... ROWS ONLY may or may not
#' be portable to other SQL dialects, I don't know.
#'
#' @param cn The database connection to use for the queries.
#' @param table.name You need to have the data that you want to pull in a single table (or view, UDF, etc.).
#' This parameter is the name of the table that will be used in the SQL queries.
#' @param order.by.clause The ORDER BY clause that will be used to determine the order in which
#' rows are fetched.  This MUST contain a column or combination of columns that are guaranteed have
#' unique values for every row, or the behavior of the FETCH operation may not be predictable.
#' @param max.n.rows The maximum number of rows to pull per chunk
#' @keywords sql
#' @export

chunked.query <- function (cn, table.name, order.by.clause, max.n.rows=2e6) {

  ## figure out how many rows we need to pull
  n.rows.total = as.numeric(unlist(dbGetQuery(cn, paste(sep="", "SELECT COUNT(*) FROM ", table.name))))

  ## based on requested max n rows per pull, figure out how many slices we will need
  n.slices = ceiling(n.rows.total / max.n.rows)
  slice.start.offset = floor(seq(from=0, to=n.rows.total, by=(n.rows.total/n.slices)))
  slice.size = 1+slice.start.offset[2:length(slice.start.offset)] - slice.start.offset[1:(length(slice.start.offset)-1)]-1
  slice.start.offset = slice.start.offset[1:(length(slice.start.offset)-1)]
  n.slices = length(slice.start.offset)

  slab.list = list()

  for (i in 1:n.slices) {

    curr.query = paste(sep="", "
  		SELECT
  			*
  		FROM
  			", table.name, "
      ", order.by.clause, "
  		OFFSET ", format(scientific=FALSE, slice.start.offset[i]), " ROWS
  			FETCH NEXT ", format(scientific=FALSE, slice.size[i]), " ROWS ONLY"
    )
    ##cat(curr.query, "\n")
    q = dbSendQuery(cn, curr.query)

    cat(sep="", "Retrieving slice ", i, " of ", n.slices, " (", format(scientific=FALSE, slice.size[i]), " rows)", "\n")
    cat(date(), "\n")
    slab.list[[length(slab.list)+1]] = fetch(q, slice.size[i])
  }

  stmt = paste("retval = rbind(", paste(sep="", "slab.list[[", 1:n.slices, "]]", collapse=", "), ")")
  eval(parse(text=stmt))
  return(retval)
}
