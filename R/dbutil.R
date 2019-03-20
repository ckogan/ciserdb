

ciserdb <- function(user = "root") {
  DBI::dbConnect(RMySQL::MySQL(),
                 host = "ec2-18-222-8-242.us-east-2.compute.amazonaws.com",
                 user = user,
                 password = getPass(),
                 dbname = "ciserdb"
  )
}




list_tables <- function(con) {
  dbGetQuery(con, "SELECT table_name FROM information_schema.tables where table_schema = 'ciserdb';")
}

