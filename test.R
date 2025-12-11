

library("devtools")
library("RSQLite")
f <- function(...) { document(); load_all() }

#FILE <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/test_error_log"

FILE <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/000-www.jstatsoft.org_access_log"

library("apache2logparser")

if (file.exists("foo.sqlite3")) file.remove("foo.sqlite3")
f()
con <- open_database("foo.sqlite3")
f()
t <- system.time(k <- parse_file(FILE, con = con, n = 1e5, verbose = TRUE, warn = TRUE))
print(t)
msg <- dbGetQuery(con, "SELECT * FROM messages")
log <- dbGetQuery(con, "SELECT * FROM logs")
dbDisconnect(con)
dim(log)
dim(msg)

head(msg)
head(log)

