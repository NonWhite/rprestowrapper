type_converter = function(presto_type){
  presto_type = ifelse(presto_type %like% 'varchar', 'varchar', presto_type)
  switch(presto_type,
  'bigint' = as.numeric,
  'boolean' = as.logical,
  'char' = as.character,
  'date' = as.Date,
  'decimal' = as.numeric,
  'double' = as.numeric,
  'integer' = as.numeric,
  'real' = as.numeric,
  'smallint' = as.numeric,
  'timestamp' = as.POSIXlt,
  'tinyint' = as.numeric,
  'varchar' = as.character,
  as.character
  )
}

check_error <- function(res){
	keys = names(res)
	if('error' %in% keys){
		stop(res$error$message)
	}
}

get_headers <- function(conn){
	headers = httr::add_headers(
		'X-Presto-Catalog' = conn$catalog,
		'X-Presto-Source' = conn$source,
		'X-Presto-Schema' = conn$schema,
		'User-Agent' = 'rprestowrapper',
		'X-Presto-User' = conn$user
	)
}

send_query <- function(conn,sql_query){
	url = paste(conn$host, ':', conn$port, '/v1/statement', sep = '')
	body = gsub(';', '', sql_query)
	headers = get_headers(conn)
	if(conn$password == ''){
		r = httr::POST(url, body = body, encode = "raw",headers)
	}else{
		r = httr::POST(url, body = body, encode = "raw",headers,
			httr::authenticate(user=conn$user,password=conn$password))
	}
	parsed = jsonlite::fromJSON(httr::content(r,'text',encoding='UTF-8'))
	check_error(parsed)
	parsed
}

make_rows_request <- function(conn,nextUri){
	if(conn$password == ''){
		res = httr::GET(nextUri,get_headers(conn))
	}else{
		res = httr::GET(nextUri,get_headers(conn),
						httr::authenticate(user=conn$user,password=conn$password))
	}
	parsed = jsonlite::fromJSON(httr::content(res,'text',encoding='UTF-8'))
	check_error(parsed)
	parsed
}

get_query_result <- function(conn,res){
	parsed = res
	keys = names(res)
	df = NULL
	while('nextUri' %in% keys){
		parsed = make_rows_request(conn,parsed$nextUri)
		keys = names(parsed)
		if('data' %in% keys){
			column_names = as.list(parsed$columns[,1])
			dataframe = data.frame(parsed$data)
			if(nrow(dataframe) > 0){
				colnames(dataframe) = column_names
				if(!is.null(df)){
					df = dplyr::bind_rows(df, dataframe)
				}
				else{
					df = dataframe
				}
			}
		}
		else{
			Sys.sleep(5)
		}
	}
	columns = dplyr::select(parsed$columns,name,type)
	if(is.null(df)){
		df <- data.frame(matrix(ncol = length(columns$name), nrow = 0))
		colnames(df) <- columns$name
	}
	for(col in columns$name){
		df[,col] = type_converter(columns$type[which(columns$name == col)])(as.character(df[,col]))
	}
	df
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
	dataframe = get_query_result(conn,res)
}
