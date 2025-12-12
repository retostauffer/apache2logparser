

library("devtools")
library("RSQLite")
f <- function(...) { document(); load_all() }

FILE_A <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/000-www.jstatsoft.org_access_log"
FILE_E <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/test_error_log"

f()
library("apache2logparser")

if (file.exists("test.sqlite3")) file.remove("test.sqlite3")
f()
con <- open_database("test.sqlite3")
f()

f(); t <- system.time(k <- parse_file(FILE_A, con = con, n = 50, maxbatches = 1, verbose = TRUE, warn = TRUE))
print(t)
f(); t <- system.time(k <- parse_file(FILE_E, con = con, n = 50, maxbatches = 1, verbose = TRUE, warn = TRUE))
print(t)

traceback()
msg <- dbGetQuery(con, "SELECT * FROM messages")
log <- dbGetQuery(con, "SELECT * FROM logs")
dbDisconnect(con)
print(dim(log))
print(dim(msg))

head(msg)
head(log)

