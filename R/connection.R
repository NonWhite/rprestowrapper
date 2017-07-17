#' Start connection
#'
#' @param host Presto server's addres
#' @param port Presto server's port
#' @param port Presto server's user for making queries
#' @param port Presto server's schema
#' @param port Presto server's catalog
#' @param port Presto server's source
#' @export
start_connection <- function(host,port,user,schema,catalog,source){
	vars = list(
		host = host,
		port = port,
		user = user,
		schema = schema,
		catalog = catalog,
		source = source
	)
}
