

library("apache2logparser")

dir <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105"
FILE_A <- file.path(dir, "000-www.jstatsoft.org_access_log")
FILE_E <- file.path(dir, "000-www.jstatsoft.org_error_log")

# Create random file name for sqlite3 database.
DBFILE <- "DOS.sqlite3"

# Connecting to database
con <- open_database(DBFILE)

# Parsing access log file and error log file; both directly
# write to database via the database connection
N    <- 50000
an   <- parse_file(FILE_A, con = con, n = N, verbose = TRUE)
en   <- parse_file(FILE_E, con = con, n = N, verbose = TRUE)

# Reading all data from the three different tables created
msg  <- dbGetQuery(con, "SELECT * FROM messages")
alog <- dbGetQuery(con, "SELECT * FROM access_logs")
elog <- dbGetQuery(con, "SELECT * FROM error_logs")

# Disconnecting from database
dbDisconnect(con)

# Some output for testing
message("Size of table 'messages':    ", paste(dim(msg), collapse = " x "))
message("Size of table 'access_logs': ", paste(dim(alog), collapse = " x "))
message("Size of table 'error_logs':  ", paste(dim(elog), collapse = " x "))

