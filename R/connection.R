#' Start connection
#'
#' @param host Presto server's addres
#' @param port Presto server's port
#' @param user Presto server's user for making queries
#' @param schema Presto server's schema
#' @param catalog Presto server's catalog
#' @param source Presto server's source
#' @param password Presto server's user password
#' @export
start_connection <- function(host,port,user,schema,catalog,source,password=''){
	vars = list(
		host = host,
		port = port,
		user = user,
		schema = schema,
		catalog = catalog,
		source = source,
		password = password
	)
}
