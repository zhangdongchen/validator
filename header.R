library(tidyverse)
library(yaml)
library(mongolite)
library(cli)
library(plumber)
library(jsonlite)

# Helper Function ---------------------------------------------------------

est_mongo_conn <- function(conn) {
  
  # make sure file exists
  if (!file.exists("dbconfig.yaml")) {
    stop("Database credential not found.")
  }
  
  # read config
  d <- yaml::read_yaml("dbconfig.yaml")
  
  # check if db is specified
  if( "db" %in% names(d[[conn]]) ){
    db = d[[conn]][['db']]
  }else{
    db = conn
  }
  
  # check if the credentials are specified
  if(!all(c("host", "port", "collection") %in% names(d[[conn]]))){
    stop("One or more database parameters is incorrect.")
  }
  
  # est conn
  c <- mongo(
    collection = d[[conn]][["collection"]],
    url = with(d[[conn]], 
               # mongodb://username:password@host:port
               sprintf("mongodb://%s:%s@%s:%d/", user, password, host, port)),
    db = d[[conn]][["db"]]
  )
  
  # return connection
  c
  
}