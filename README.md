# rprestowrapper
Package for making queries on Presto server

## 1. Install package

	devtools::install_github("NonWhite/rprestowrapper")

## 2. Use package

	conn = start_connection(host,port,user,schema,catalog,source)
	query = "select * from my_table limit 1"
	result = run_query(conn,query)
