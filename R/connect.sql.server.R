#' A function that connects an R session to a running instance of
#' Microsoft SQL Server using the FreeTDS driver and ODBC.
#'
#' @param database The name of the database on the server that you want to connect to
#' @param user The user name that you want to use to authenticate against SQL Server
#' @param password The password for the user that will be authenticated against SQL Server
#' @param server.address The network address of the Windows instance that is running SQL Server
#' @param domain The domain to use in the case of NTLM logins, NA if using SQL Server login credentials
#' @param port The TCP port SQL Server is listening on
#' @keywords SQL Server ODBC
#' @export

connect.sql.server <- function (database, user, password, server.address, domain = NA, port=1433) {
    
    require(odbc)

    cn <- NA

    if (is.na(domain)) {
        cn <- dbConnect(
            odbc::odbc(), 
            driver = "FreeTDS", 
            server = server.address, 
            port = port, 
            uid = user, 
            pwd = password, 
            TDS_Version = "8.0")        

    } else {
        cn <- dbConnect(
            odbc::odbc(), 
            driver = "FreeTDS", 
            server = server.address, 
            port = port, 
            database = database, 
            uid = paste(sep="", domain, "\\", user), 
            pwd = password, 
            TDS_Version = "8.0")
    }

    return(cn)
}