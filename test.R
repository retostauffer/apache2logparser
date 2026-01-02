#!/usr/bin/env Rscript

library("apache2logparser")
library("devtools")
load_all()

dir <- system.file("extdata", package = "apache2logparser")
stopifnot(file.exists(FILE_A <- file.path(dir, "www.example.com_access_log")))
stopifnot(file.exists(FILE_E <- file.path(dir, "www.example.com_error_log")))

# Create random file name for sqlite3 database.
DBFILE <- basename(tempfile(fileext = "_demo.sqlite3"))

# Connecting to database
con <- open_database(DBFILE)

# Parsing access log file and error log file; both directly
# write to database via the database connection
an <- parse_file(FILE_A, con = con, n = 30, verbose = TRUE)
en <- parse_file(FILE_E, con = con, n = 30, verbose = TRUE)

# Reading all data from the three different tables created
msg <- dbGetQuery(con, "SELECT * FROM messages")
alog <- dbGetQuery(con, "SELECT * FROM access_logs")
elog <- dbGetQuery(con, "SELECT * FROM error_logs")

# Disconnecting from database
dbDisconnect(con)

# Some output for testing
message("Size of table 'messages':    ", paste(dim(msg), collapse = " x "))
message("Size of table 'access_logs': ", paste(dim(alog), collapse = " x "))
message("Size of table 'error_logs':  ", paste(dim(elog), collapse = " x "))

# Removing demo file
file.remove(DBFILE)


