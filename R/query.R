send_query <- function(conn,sql_query){
  url = paste(conn$host, ':', conn$port, '/v1/statement', sep = '')
  body = gsub(';', '', sql_query)
  r = httr::POST(url, body = body, encode = "raw",
                httr::add_headers('X-Presto-Catalog' = conn$catalog,
                                  'X-Presto-Source' = conn$source,
                                  'X-Presto-Schema' = conn$schema,
                                  'User-Agent' = 'rprestowrapper',
                                  'X-Presto-User' = conn$user))
}

get_query_result <- function(res){
  parsed = jsonlite::fromJSON(httr::content(res,'text', encoding='UTF-8'))
  keys = names(parsed)
  df = NULL
  while('nextUri' %in% keys){
    nextUri = parsed$nextUri
    res = httr::GET(nextUri)
    parsed = jsonlite::fromJSON(httr::content(res,'text', encoding='UTF-8'))
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
  dataframe = get_query_result(res)
}
