#!/usr/bin/env Rscript

library("apache2logparser")

# Create random file name for sqlite3 database.

# Connecting to database
con <- open_database("DOS.sqlite3")

# Reading all data from the three different tables created
msg  <- dbGetQuery(con, "SELECT * FROM messages")
alog <- dbGetQuery(con, "SELECT * FROM access_logs")
elog <- dbGetQuery(con, "SELECT * FROM error_logs")

# Disconnecting from database
dbDisconnect(con)

# Some output for testing
message("Read data.frame from table 'messages':    ", paste(dim(msg), collapse = " x "))
message("Read data.frame from table 'access_logs': ", paste(dim(alog), collapse = " x "))
message("Read data.frame from table 'error_logs':  ", paste(dim(elog), collapse = " x "))

