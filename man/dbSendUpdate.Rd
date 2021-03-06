% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbSendUpdate.R
\name{dbSendUpdate}
\alias{dbSendUpdate}
\title{A function that executes a SQL statement that does not return a resultset.
Uses DBI "immediate" execution option where available so that e.g. SQL Server
temp tables are created in the same session.}
\usage{
dbSendUpdate(cn, statement)
}
\arguments{
\item{cn}{The database connection to use}

\item{statement}{The SQL statemment to be executed}
}
\description{
A function that executes a SQL statement that does not return a resultset.
Uses DBI "immediate" execution option where available so that e.g. SQL Server
temp tables are created in the same session.
}
\keyword{ODBC}
\keyword{SQL}
\keyword{Server}
