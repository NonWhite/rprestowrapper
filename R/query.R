send_query <- function(conn,sql_query){
	url = paste(conn$host, ':', conn$port, '/v1/statement', sep = '')
	body = sql_query
	r = POST(url, body = body, encode = "raw",
			add_headers('X-Presto-Catalog' = conn$catalog,
				'X-Presto-Source' = conn$source,
				'X-Presto-Schema' = conn$schema,
				'User-Agent' = 'rprestowrapper',
				'X-Presto-User' = conn$user
			)
		)
}

get_query_result <- function(res){
	browser()
	parsed = fromJSON(content(res,'text'))
	nextUri = parsed$nextUri
	res = GET(nextUri)
	parsed = fromJSON(content(res,'text'))
	keys = names(parsed)
	dataframes = list()
	while('nextUri' %in% keys){
		column_names = as.list(parsed$columns[,1])
		dataframe = data.frame(parsed$data)
		if(nrow(dataframe) > 0){
			colnames(dataframe) = column_names
		}
		dataframes = c(dataframes, list(dataframe))
		nextUri = parsed$nextUri
		res = GET(nextUri)
		parsed = fromJSON(content(res,'text'))
		keys = names(parsed)
	}
	Reduce(function(x, y) merge(x, y, all=TRUE), dataframes)
}

#' Run query on presto
#'
#' @param conn Connection object for Presto server
#' @param sql_query Query to run
#' @return A dataframe with the query result
#' @export
#' @examples
#' run_query(conn,"select * from my_table limit 1")
run_query <- function(conn,sql_query){
	res = send_query(conn,sql_query)
	dataframe = get_query_result(res)
}
